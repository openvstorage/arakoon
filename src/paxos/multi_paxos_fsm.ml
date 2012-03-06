(*
This file is part of Arakoon, a distributed key-value store. Copyright
(C) 2010 Incubaid BVBA

Licensees holding a valid Incubaid license may use this file in
accordance with Incubaid's Arakoon commercial license agreement. For
more information on how to enter into this agreement, please contact
Incubaid (contact details can be found on www.arakoon.org/licensing).

Alternatively, this file may be redistributed and/or modified under
the terms of the GNU Affero General Public License version 3, as
published by the Free Software Foundation. Under this license, this
file is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.

See the GNU Affero General Public License for more details.
You should have received a copy of the
GNU Affero General Public License along with this program (file "COPYING").
If not, see <http://www.gnu.org/licenses/>.
*)

open Lwt
open Messaging
open Mp_msg
open MPMessage

open Update
open Lwt_buffer
open Fsm
open Multi_paxos_type
open Multi_paxos
open Master_type


(* a forced master always suggests himself *)
let forced_master_suggest constants (n,i) () =
  let me = constants.me in
  let n' = update_n constants n in
  mcast constants (Prepare (n',i)) >>= fun () ->
  start_election_timeout constants n >>= fun () ->
  log ~me "forced_master_suggest: suggesting n=%s" (Sn.string_of n') >>= fun () ->
  let tlog_coll = constants.tlog_coll in
  tlog_coll # get_last_update i >>= fun l_upd ->
  
  let v_lims =
    begin
      match l_upd with
        | None -> 
           (1,[]) 
        | Some u ->
           (0, [(Update.make_update_value (u),1)])      
    end in
  let who_voted = [me] in
  
  let i_lim = Some (me,i) in
  let state = (n', i, who_voted, v_lims, i_lim) in
  Lwt.return (Promises_check_done state)

(* in case of election, everybody suggests himself *)
let election_suggest constants (n,i,vo) () =
  let me = constants.me in
  let v_lims, msg = 
    match vo with 
      | None ->
        (1,[]) , "None"
      | Some x -> (0,[(x,1)]) , "Some _"
  in
  log ~me "election_suggest: n=%s i=%s %s"  (Sn.string_of n) (Sn.string_of i) msg >>= fun () ->
  start_election_timeout constants n >>= fun () ->
  let delay =
    match constants.master with
      | Preferred p when p <> me -> 1 + (constants.lease_expiration /2)
      | _ -> 0
  in
  let df = float delay in
  Lwt.ignore_result 
    (Lwt_unix.sleep df >>= fun () -> mcast constants (Prepare (n,i)));
  let who_voted = [me] in
  let i_lim = Some (me,i) in
  let state = (n, i, who_voted, v_lims, i_lim) in
  Lwt.return (Promises_check_done state)

let read_only constants state () =
  Lwt_unix.sleep 60.0 >>= fun () ->
  Lwt_log.debug "read_only ..." >>= fun () ->
  Lwt.return (Read_only state)

(* a pending slave that is waiting for a prepare or a nak
   in order to discover a master *)
let slave_waiting_for_prepare constants ( (current_i:Sn.t),(current_n:Sn.t)) event =
  match event with 
    | FromNode(msg,source) ->
      begin
	let me = constants.me in
	log ~me "slave_waiting_for_prepare: %S" (MPMessage.string_of msg) 
	>>= fun () ->
	match msg with
	  | Prepare(n',i') ->
	    begin
              handle_prepare constants source current_n n' i' >>= function
		| Nak_sent
		| Prepare_dropped ->
		  Lwt.return( Slave_waiting_for_prepare(current_i, current_n ) )
		| Promise_sent_up2date ->
		  let tlog_coll = constants.tlog_coll in
		  tlog_coll # get_last_i () >>= fun tlc_i ->
		  tlog_coll # get_last_update tlc_i >>= fun l_update ->
		  let l_uval = 
		    begin
		      match l_update with 
			| Some u -> Some( ( Update.make_update_value u ), tlc_i ) 
			| None -> None
		    end in
		  Lwt.return (Slave_wait_for_accept (n', current_i, None, l_uval))
		    | Promise_sent_needs_catchup ->
		      Store.get_catchup_start_i constants.store >>= fun i ->
		      let state = (source, i, n',i') in 
		      Lwt.return (Slave_discovered_other_master state)
	    end
	  | Nak(n',(n2, i2)) when n' = -1L ->
	    begin
	      log ~me "fake prepare response: discovered master" >>= fun () ->
              Store.get_catchup_start_i constants.store >>= fun cu_pred ->
              Lwt.return (Slave_discovered_other_master (source, cu_pred, n2, i2))
	    end
	  | Nak(n',(n2, i2)) when i2 > current_i ->
	    begin
	      log ~me "got %s => go to catchup" (string_of msg) >>= fun () ->
              Store.get_catchup_start_i constants.store >>= fun cu_pred ->
              Lwt.return (Slave_discovered_other_master (source, cu_pred, n2, i2))
	    end
	  | Nak(n',(n2, i2)) when i2 = current_i ->
	    begin
	      log ~me "got %s => we're in sync" (string_of msg) >>= fun () ->
              (* pick in @ steady state *)
	      constants.get_value i2 >>= fun p ->
	      match p with
          | None ->
            begin
	      Lwt.return (Slave_waiting_for_prepare (i2,current_n) )
            end
          | Some v ->
            begin
              log ~me "reentering steady state @(%s,%s)" 
		(Sn.string_of n2) (Sn.string_of i2) 
              >>= fun () ->
              start_lease_expiration_thread constants n2 constants.lease_expiration >>= fun () ->
              Lwt.return (Slave_steady_state (n2, i2, v))
	    end
	    end
    | Accept(n', i', v) when current_n = n' && i' > current_i ->
      begin
        Store.get_catchup_start_i constants.store >>= fun cu_pred ->
        Lwt.return (Slave_discovered_other_master (source, cu_pred, n', i'))
      end
	  | _ -> log ~me "dropping unexpected %s" (string_of msg) >>= fun () ->
	    Lwt.return (Slave_waiting_for_prepare (current_i,current_n))
      end
    | ElectionTimeout n' 
    | LeaseExpired n' ->
      if n' = current_n then
        Lwt.return(Slave_fake_prepare(current_i, current_n))
      else
        Lwt.return(Slave_waiting_for_prepare(current_i, current_n))
    | FromClient _ -> paxos_fatal constants.me "Slave_waiting_for_prepare cannot handle client requests"
    
    | Quiesce (sleep,awake) ->
      handle_quiesce_request constants.store sleep awake >>= fun () ->
      Lwt.return (Slave_waiting_for_prepare (current_i,current_n) )
      
    | Unquiesce ->
      handle_unquiesce_request constants current_n >>= fun (store_i, vo) ->
      Lwt.return (Slave_waiting_for_prepare (current_i,current_n) )


(* a potential master is waiting for promises and if enough
   promises are received the value is accepted and Accept is broadcasted *)
let promises_check_done constants state () =
  let n, i, who_voted, (v_lims:v_limits), i_lim = state in
  (* 
     3 cases:
     -) too early to decide
     -) consensus on some value (either my wanted or another)
     -) no consensus possible anymore. (split vote)
  *)
  let me = constants.me in
  let nnones, v_s = v_lims in 
  let bv,bf =
  begin 
    match v_s with 
      | [] ->  (Update.make_update_value (Update.make_master_set me None), 0)
      | hd::tl -> 
        let bv, bf = hd in
        if Update.is_master_set bv 
        then (Update.make_update_value (Update.make_master_set me None), bf)
        else bv, bf
         
  end in 
  let nnodes = List.length constants.others + 1 in
  let needed = constants.quorum_function nnodes in
  let nvoted = List.length who_voted in
  if bf + nnones = needed 
  then
    begin
      log ~me "promises_check_done: consensus on %s" (Sn.string_of i)
      >>= fun () ->
      constants.on_accept (bv,n,i) >>= fun v ->
      start_lease_expiration_thread constants n (constants.lease_expiration / 2)  >>= fun () ->
      let msg = Accept(n,i,bv) in
      mcast constants msg >>= fun () ->
      let new_ballot = (needed-1 , [me] ) in
      Lwt.return (Accepteds_check_done (None, n, i, new_ballot, v))
    end
  else (* bf < needed *)
    if nvoted < nnodes 
    then Lwt.return (Wait_for_promises state)
    else (* split vote *)
      begin
    	if am_forced_master constants me
	then
	  Lwt.return (Forced_master_suggest (n,i))
	else 
	  if is_election constants 
	  then
	    let n' = update_n constants n in
	    Lwt.return (Election_suggest (n',i, Some bv))
	  else
	    paxos_fatal me "slave checking for promises"
      end
	

(* a potential master is waiting for promises and receives a msg *)
let wait_for_promises constants state event =
  let me = constants.me in
  let (n, i, who_voted, v_lims, i_lim) = state in
  match event with
    | FromNode (msg,source) ->
      begin
        let wanted =
        begin
          let nnones, nsomes = v_lims in
          match nsomes with
            | [] -> None
            | hd::tl -> 
              let bv, bf = hd in
              Some bv
        end
        in
        let drop msg reason = 
          log ~me "dropping %s because: %s" (string_of msg) reason >>= fun () ->
          Lwt.return (Wait_for_promises state) 
        in
	      let who_voted_s = Log_extra.string_of_list (fun s -> s) who_voted in
        log ~me "wait_for_promises:n=%s i=%s who_voted = %s" (Sn.string_of n) (Sn.string_of i) who_voted_s 
	      >>= fun () ->
        begin
          log ~me "wait_for_promises:: received %S from %s" (string_of msg) source 
	        >>= fun () ->
          match msg with
            | Promise (n' ,i', limit) when n' < n ->
              let reason = Printf.sprintf "old promise (%s < %s)" (Sn.string_of n') (Sn.string_of n) in
              drop msg reason
            | Promise (n' ,new_i, limit) when n' = n ->
              if List.mem source who_voted then
                drop msg "duplicate promise"
              else
                let v_lims' = 
                begin
                   if new_i < i then
                     let (nnones, nsomes) = v_lims in
                     (nnones+1, nsomes)
                   else
                     update_votes v_lims limit 
                end in
                let who_voted' = source :: who_voted in
                let new_ilim = match i_lim with
                  | Some (source',i') -> if i' < new_i then Some (source,new_i) else i_lim
                  | None -> Some (source,new_i)
		            in
                let state' = (n, i, who_voted', v_lims', new_ilim) in
                Lwt.return (Promises_check_done state')
            | Promise (n' ,i', limit) -> (* n ' > n *)
              begin
		log ~me "Received Promise from previous incarnation. Bumping n from %s over %s." (Sn.string_of n) (Sn.string_of n') 
		>>= fun () ->
		let new_n = update_n constants n' in
		Lwt.return (Election_suggest (new_n,i, wanted))
              end
            | Nak (n',(n'',i')) when n' < n ->
              begin
		log ~me "wait_for_promises:: received old %S (ignoring)" (string_of msg) 
		>>= fun () ->
		Lwt.return (Wait_for_promises state)
              end
            | Nak (n',(n'',i')) when n' > n ->
              begin
		log ~me "Received Nak from previous incarnation. Bumping n from %s over %s." (Sn.string_of n) (Sn.string_of n') 
		>>= fun () ->
		let new_n = update_n constants n' in
		Lwt.return (Election_suggest (new_n,i, wanted))
              end
            | Nak (n',(n'',i')) -> (* n' = n *)
              begin
		log ~me "wait_for_promises:: received %S for my Prep" (string_of msg) 
		>>= fun () ->
		if am_forced_master constants me
		then
		  begin
                    log ~me "wait_for_promises; forcing new master suggest" >>= fun () ->
                    let n3 = update_n constants (max n n'') in
                    Lwt.return (Forced_master_suggest (n3,i))
		  end
		else 
                  (* if is_election constants 
                  then *)
                    begin
                      log ~me "wait_for_promises; discovered other node" 
                      >>= fun () ->
                      if n'' > n || i' > i then
                        Store.get_catchup_start_i constants.store >>= fun cu_pred ->
                        Lwt.return (Slave_discovered_other_master (source,cu_pred,n'',i'))
                      else
			let new_n = update_n constants (max n n'') in
			Lwt.return (Election_suggest (new_n,i, wanted))
                    end
                  (* else (* forced_slave *) (* this state is impossible?! *)
                    begin
                      log ~me "wait_for_promises; forced slave back waiting for prepare" >>= fun () ->
                      Lwt.return (Slave_waiting_for_prepare (i,n))
                    end *)
              end
            | Prepare (n',i') ->
              begin
                if (am_forced_master constants me) && n' > 0L
	              then
		              begin
		                log ~me "wait_for_promises:dueling; forcing new master suggest" >>= fun () ->
		                let reply = Nak (n', (n,i))  in
				            constants.send reply me source >>= fun () ->
				            let new_n = update_n constants n' in
				            Lwt.return (Forced_master_suggest (new_n, i))
				          end
		            else
                  handle_prepare constants source n n' i' >>= function
                    | Nak_sent ->
                      log ~me "wait_for_promises: resending prepare" >>= fun () ->
                      let reply = Prepare(n, i) in
                      constants.send reply me source >>= fun () ->
                      Lwt.return (Wait_for_promises state)
                    | Prepare_dropped -> 
                      Lwt.return (Wait_for_promises state)
                    | Promise_sent_up2date ->
		      begin
			let tlog_coll = constants.tlog_coll in
			tlog_coll # get_last_i () >>= fun tlc_i ->
			tlog_coll # get_last_update tlc_i >>= fun l_update ->
			let l_uval = 
			  begin
			    match l_update with 
			      | Some u -> Some( ( Update.make_update_value u ), tlc_i ) 
			      | None -> None
			  end in
			Lwt.return (Slave_wait_for_accept (n', i, None, l_uval))
		      end
		    | Promise_sent_needs_catchup ->
		      Store.get_catchup_start_i constants.store >>= fun i ->
		      Lwt.return (Slave_discovered_other_master (source, i, n', i'))
              end
            | Accept (n',_i,_v) when n' < n ->
              begin
                if i < _i
                then
                  begin
                    log ~me "wait_for_promises: still have an active master (received %s) -> catching up from master"  (string_of msg) >>= fun () ->
                    Lwt.return (Slave_discovered_other_master (source, i, n', _i))
		              end
                else
		              log ~me "wait_for_promises: ignoring old Accept %s" (Sn.string_of n') 
		              >>= fun () ->
		              Lwt.return (Wait_for_promises state)
              end
            | Accept (n',_i,_v) ->
              if n' = n && _i < i 
              then
		begin
                  log ~me "wait_for_promises: ignoring %s (my i is %s)" (string_of msg) (Sn.string_of i) >>= fun () ->
                  Lwt.return (Wait_for_promises state)
		end
              else
		begin
                  log ~me "wait_for_promises: received %S -> back to fake prepare"  (string_of msg) >>= fun () ->
                  Lwt.return (Slave_fake_prepare (i,n))
		end
            | Accepted (n',_i) when n' < n ->
              begin
		log ~me "wait_for_promises: ignoring old Accepted %s" (Sn.string_of n') >>= fun () ->
		Lwt.return (Wait_for_promises state)
              end
            | Accepted (n',_i) -> (* n' >= n *)
              begin 
		log ~me "Received Nak from previous incarnation. Bumping n from %s over %s." (Sn.string_of n) (Sn.string_of n') 
		>>= fun () ->
		let new_n = update_n constants n' in
		Lwt.return (Election_suggest (new_n,i, wanted))
              end
        end
      end
    | ElectionTimeout n' ->
      let (n,i,who_voted, v_lims, i_lim) = state in
      let wanted =
        begin
          let nnones, nsomes = v_lims in
          match nsomes with
            | [] -> None
            | hd::tl -> 
              let bv, bf = hd in
              Some bv
        end
        in
      if n' = n && not ( constants.store # quiesced () ) then
	begin
    log ~me "wait_for_promises: election timeout, restart from scratch"	  
	  >>= fun () ->
	  Lwt.return (Election_suggest (n,i, wanted))
	end
      else
	begin
	  Lwt.return (Wait_for_promises state)
	end
    | LeaseExpired _ ->
      Lwt_log.debug "Ignoring lease expiration" >>= fun () ->
      Lwt.return (Wait_for_promises state)
    | FromClient _ -> 
      paxos_fatal me "wait_for_promises: don't want FromClient"
    
    | Quiesce (sleep,awake) ->
      handle_quiesce_request constants.store sleep awake >>= fun () ->
      Lwt.return (Wait_for_promises state)
      
    | Unquiesce ->
      handle_unquiesce_request constants n >>= fun (store_i, vo) ->
      Lwt.return (Wait_for_promises state)
      
  
  
(* a (potential or full) master is waiting for accepteds and if he has received
   enough, consensus is reached and he becomes a full master *)
let accepteds_check_done constants state () =
  let (mo,n,i,ballot,v) = state in
  let me = constants.me in
  let needed, already_voted = ballot in
  if needed = 0 then
    begin
      log ~me "accepted_check_done :: we're done! returning %s %s"
	(Sn.string_of n) ( Sn.string_of i )
      >>= fun () ->
      Lwt.return (Master_consensus (mo,v,n,i))
    end
  else
    Lwt.return (Wait_for_accepteds (mo,n,i,ballot,v))
      
(* a (potential or full) master is waiting for accepteds and receives a msg *)
let wait_for_accepteds constants state (event:paxos_event) =
  let me = constants.me in
  match event with
    | FromNode(msg,source) ->
      begin
    (* TODO: what happens with the client request
       when I fall back to a state without mo ? *)
	let (mo,n,i,ballot,v) = state in
	let drop msg reason =
	  log ~me "dropping %s because : '%s'" (MPMessage.string_of msg) reason
	  >>= fun () ->
	  Lwt.return (Wait_for_accepteds state)
	in
	let needed, already_voted = ballot in
	log ~me "wait_for_accepteds(%i to go) got %S from %s" needed 
	  (MPMessage.string_of msg) source >>= fun () ->
	match msg with
	  | Accepted (n',i') when (n',i')=(n,i)  ->
	    begin
	      constants.on_witness source i' >>= fun () ->
	      if List.mem source already_voted then
		let reason = Printf.sprintf "%s already voted" source in
		drop msg reason
	      else
		let ballot' = needed -1, source :: already_voted in
		Lwt.return (Accepteds_check_done (mo,n,i,ballot',v))
	    end
	  | Accepted (n',i') when n' = n && i' < i ->
	    begin
	      constants.on_witness source i' >>= fun () ->
	      log ~me "wait_for_accepteds: received older Accepted for my n: ignoring" 
	      >>= fun () ->
	      Lwt.return (Wait_for_accepteds state)
	    end
	  | Accepted (n',i') when n' < n ->
	    begin
	      constants.on_witness source i' >>= fun () ->
	      let reason = Printf.sprintf "dropping old %S we're @ (%s,%s)" (string_of msg)
		(Sn.string_of n) (Sn.string_of i) in
	      drop msg reason
	    end
	  | Accepted (n',i') -> (* n' > n *)
	    paxos_fatal me "wait_for_accepteds:: received %S with n'=%s > my n=%s FATAL" (string_of msg) (Sn.string_of n') (Sn.string_of n)
	      
	  | Promise(n',i', limit) ->
	    begin
	      constants.on_witness source i' >>= fun () ->
	      if n' <= n then
          begin
					  let reason = Printf.sprintf
					    "already reached consensus on (%s,%s)" 
					    (Sn.string_of n) (Sn.string_of i) 
					  in
					  drop msg reason
					end
	      else
		      begin
		        let reason = Printf.sprintf "future Promise(%s,%s), local (%s,%s)"
			        (Sn.string_of n') (Sn.string_of i') (Sn.string_of n) (Sn.string_of i) in
			      drop msg reason
			    end
	    end
	  | Prepare (n',i') -> (* n' > n *)
	    begin
	      let lost_master_role () =
			      begin
			        match mo with
			        | None -> Lwt.return ()
			        | Some finished -> 
			          let msg = "lost master role during wait_for_accepteds while handling client request" in
			          let rc = Arakoon_exc.E_NOT_MASTER in
			          let result = Store.Update_fail (rc, msg) in
	              finished result
	          end
	      in
	      if am_forced_master constants me
	      then
          if n' <= n
          then
            let reply = Nak( n', (n,i) ) in
            constants.send reply me source >>= fun () ->
            let followup = Accept( n, i, v) in
            constants.send followup me source >>= fun () ->
            Lwt.return (Wait_for_accepteds state)
          else
            lost_master_role() >>= fun () ->
            Lwt.return (Forced_master_suggest (n',i))
	      else 
          begin
            handle_prepare constants source n n' i' >>= function
              | Prepare_dropped
              | Nak_sent -> 
                Lwt.return( Wait_for_accepteds state )
              | Promise_sent_up2date ->
                let tlog_coll = constants.tlog_coll in
				        tlog_coll # get_last_i () >>= fun tlc_i ->
				        tlog_coll # get_last_update tlc_i >>= fun l_update ->
				        let l_uval = 
				        begin
				          match l_update with 
				            | Some u -> Some( ( Update.make_update_value u ), tlc_i ) 
				            | None -> None
				        end in
                lost_master_role () >>= fun () ->
					      Lwt.return (Slave_wait_for_accept (n', i, None, l_uval))
              | Promise_sent_needs_catchup ->
                Store.get_catchup_start_i constants.store >>= fun i ->
                lost_master_role () >>= fun () ->
                Lwt.return (Slave_discovered_other_master (source, i, n', i'))
          end
      end 
	  | Nak (n',i) ->
	    begin
	      log ~me "wait_for_accepted: ignoring %S from %s when collecting accepteds" (MPMessage.string_of msg) source >>= fun () ->
	      Lwt.return (Wait_for_accepteds state)
	    end
	  | Accept (n',_i,_v) when n' < n ->
	    begin
	      log ~me "wait_for_accepted: dropping old Accept %S" (string_of msg) >>= fun () ->
	      Lwt.return (Wait_for_accepteds state)
	    end
	  | Accept (n',i',v') when (n',i',v')=(n,i,v) ->
	    begin
	      log ~me "wait_for_accepted: ignoring extra Accept %S" (string_of msg) >>= fun () ->
	      Lwt.return (Wait_for_accepteds state)
	    end
	  | Accept (n',i',v') when n' > n ->
      begin
      match mo with
        | None -> Lwt.return ()
        | Some finished -> 
		      let msg = "lost master role during wait_for_accepteds while handling client request" in
		      let rc = Arakoon_exc.E_NOT_MASTER in
		      let result = Store.Update_fail (rc, msg) in
		      finished result
      end >>= fun () ->
      begin 
        (* Become slave, goto catchup *)
        log ~me "wait_for_accepteds: received Accept from new master %S" (string_of msg) >>= fun () ->
        Store.get_catchup_start_i constants.store >>= fun cu_pred ->
        let new_state = (source,cu_pred,n,i') in 
        Lwt.return (Slave_discovered_other_master new_state)
      end
	  | Accept (n',i',v') when i' <= i -> (* n' = n *)
      begin
        log ~me "wait_for_accepteds: dropping accept with n = %s and i = %s" (Sn.string_of n) (Sn.string_of i') >>= fun () ->
        Lwt.return (Wait_for_accepteds state)
      end
    | Accept (n',i',v') -> (* n' = n *)
	    begin
        log ~me "wait_for_accepteds: got accept with n = %s and higher i = %s" (Sn.string_of n) (Sn.string_of i') >>= fun () ->
        Store.get_catchup_start_i constants.store >>= fun cu_pred ->
        let new_state = (source,cu_pred,n,i') in
        Lwt.return(Slave_discovered_other_master new_state)
      end
      end
    | FromClient (vo, cb) -> paxos_fatal me "no FromClient should get here"
    | LeaseExpired n' -> paxos_fatal me "no LeaseExpired should get here"
    | ElectionTimeout n' -> 
      begin
	let (_,n,i,ballot,v) = state in
	log ~me "wait_for_accepteds : election timeout " >>= fun () ->
	if n' < n then
	  begin
	    log ~me "ignoring old timeout %s<%s" (Sn.string_of n') (Sn.string_of n) 
	    >>= fun () ->
	  Lwt.return (Wait_for_accepteds state)
	  end
	else if n' = n then
	  begin
	    if (am_forced_master constants me) then
	      begin
		log ~me "going to RESEND Accept messages" >>= fun () ->
		let needed, already_voted = ballot in
		let msg = Accept(n,i,v) in
		let silent_others = List.filter (fun o -> not (List.mem o already_voted)) 
		  constants.others in
		Lwt_list.iter_s (fun o -> constants.send msg me o) silent_others >>= fun () ->
		mcast constants msg >>= fun () ->
    Lwt.return (Wait_for_accepteds state)
	      end
	    else
	      begin
		log ~me "TODO: election part of election timeout" >>= fun () ->
		Lwt.return (Wait_for_accepteds state)
	      end
		
	  end
	else
	  begin
	    Lwt.return (Wait_for_accepteds state)
	  end
      end
	  
    | Quiesce (sleep,awake) ->
      fail_quiesce_request constants.store sleep awake Quiesced_fail_master >>= fun () ->
      Lwt.return (Wait_for_accepteds state)
      
    | Unquiesce ->
      Lwt.fail (Failure "Unexpected unquiesce request while running as master")
      



(* the state machine translator *)

type product_wanted =
  | Nop
  | Node_only
  | Full
  | Node_and_inject
  | Node_and_timeout
  | Node_and_inject_and_timeout

let machine constants = 
  let nop = Nop in
  let full = Full in
  let node_and_timeout = Node_and_timeout in
  let node_and_inject_and_timeout = Node_and_inject_and_timeout in
  function
  | Forced_master_suggest state ->
    (Unit_arg (forced_master_suggest constants state), nop)

  | Slave_fake_prepare i ->
    (Unit_arg (Slave.slave_fake_prepare constants i), nop)
  | Slave_waiting_for_prepare state ->
    (Msg_arg (slave_waiting_for_prepare constants state), node_and_inject_and_timeout)
  | Slave_wait_for_accept state ->
    (Msg_arg (Slave.slave_wait_for_accept constants state), node_and_inject_and_timeout)
  | Slave_steady_state state ->
    (Msg_arg (Slave.slave_steady_state constants state), full)
  | Slave_discovered_other_master state ->
    (Unit_arg (Slave.slave_discovered_other_master constants state), nop)

  | Wait_for_promises state ->
    (Msg_arg (wait_for_promises constants state), node_and_inject_and_timeout)
  | Promises_check_done state ->
    (Unit_arg (promises_check_done constants state), nop)
  | Wait_for_accepteds state ->
    (Msg_arg (wait_for_accepteds constants state), node_and_timeout)
  | Accepteds_check_done state ->
    (Unit_arg (accepteds_check_done constants state), nop)

  | Master_consensus state ->
    (Unit_arg (Master.master_consensus constants state), nop)
  | Stable_master state ->
    (Msg_arg (Master.stable_master constants state), full)
  | Master_dictate state ->
    (Unit_arg (Master.master_dictate constants state), nop)

  | Election_suggest state ->
    (Unit_arg (election_suggest constants state), nop)
  | Read_only state -> 
    (Unit_arg (read_only constants state), nop)
  | Start_transition -> failwith "Start_transition?"

let __tracing = ref false

let trace_transition me key =
  if !__tracing 
  then log ~me "new transition: %s" (show_transition key)
  else Lwt.return ()

type ready_result =
  | Inject_ready
  | Client_ready
  | Node_ready
  | Election_timeout_ready

let prio = function
  | Inject_ready -> 0
  | Election_timeout_ready -> 1
  | Node_ready   -> 2
  | Client_ready -> 3


type ('a,'b,'c) buffers = 
    {client_buffer : 'a Lwt_buffer.t; 
     node_buffer   : 'b Lwt_buffer.t;
     inject_buffer : 'c Lwt_buffer.t;
     election_timeout_buffer: 'c Lwt_buffer.t;
    } 
let make_buffers (a,b,c,d) = {client_buffer = a;
			      node_buffer = b;
			      inject_buffer = c;
			      election_timeout_buffer = d;}
      
let rec paxos_produce buffers
    constants product_wanted =
  let me = constants.me in
  let ready_from_inject () =
    Lwt_buffer.wait_for_item buffers.inject_buffer >>= fun () ->
    Lwt.return Inject_ready
  in
  let ready_from_client () =
    Lwt_buffer.wait_for_item buffers.client_buffer >>= fun () ->
    Lwt.return Client_ready
  in
  let ready_from_node () =
    Lwt_buffer.wait_for_item buffers.node_buffer >>= fun () ->
    Lwt.return Node_ready
  in
  let ready_from_election_timeout () =
    Lwt_buffer.wait_for_item buffers.election_timeout_buffer >>= fun () ->
    Lwt.return Election_timeout_ready
  in
  let waiters =
    match product_wanted with
      | Node_only ->
	[ready_from_node ();]
      | Full ->
	[ready_from_inject();ready_from_node ();ready_from_client ();]
      | Node_and_inject ->
	[ready_from_inject();ready_from_node ();]
      | Node_and_timeout ->
	[ready_from_election_timeout (); ready_from_node();]
      | Node_and_inject_and_timeout ->
	[ready_from_inject();ready_from_election_timeout () ;ready_from_node()]
      | Nop -> failwith "Nop should not happen here"
  in
  Lwt.catch 
    (fun () ->
      Lwt.pick waiters >>= fun ready_list ->
      let get_highest_prio_evt r_list =
        let f acc b = match acc with 
        | None -> Some b
        | Some pb ->
          if prio(b) > prio (pb)
          then acc
          else Some b
       in
       let best = List.fold_left f None r_list in
       match best with
        | Some Inject_ready ->
	  begin
	    log ~me "taking from inject" >>= fun () ->
	    Lwt_buffer.take buffers.inject_buffer
	  end
	| Some Client_ready ->
	  begin
	    Lwt_buffer.take buffers.client_buffer >>= fun x ->
	    Lwt.return (FromClient x)
	  end
	| Some Node_ready ->
	  begin
	    Lwt_buffer.take buffers.node_buffer >>= fun (msg,source) ->
	    let msg2 = MPMessage.of_generic msg in
	    Lwt.return (FromNode (msg2,source))
	  end
	| Some Election_timeout_ready ->
	  begin
	    log ~me "taking from timeout" >>= fun () ->
	    Lwt_buffer.take buffers.election_timeout_buffer 
	  end
        | None -> 
	  Lwt.fail ( Failure "FSM BAILED: No events ready while there should be" )
        in get_highest_prio_evt ( [ ready_list ] ) 
    ) 
    (fun e -> log ~me "ZYX %s" (Printexc.to_string e) >>= fun () -> Lwt.fail e)

(* the entry methods *)

let enter_forced_slave constants buffers new_i vo=
  let me = constants.me in
  log ~me "+starting FSM for forced_slave." >>= fun () ->
  let trace = trace_transition me in
  let produce = paxos_produce buffers constants in
  let new_n = Sn.start in
  Lwt.catch 
    (fun () ->
      Fsm.loop ~trace produce 
	(machine constants) (Slave.slave_fake_prepare constants (new_i,new_n))
    ) 
    (fun exn ->
      Lwt_log.warning ~exn "FSM BAILED due to uncaught exception" 
      >>= fun () -> Lwt.fail exn
    )

let enter_forced_master constants buffers current_i vo =
  let me = constants.me in
  log ~me "+starting FSM for forced_master." >>= fun () ->
  let current_n = 0L in
  let trace = trace_transition me in
  let produce = paxos_produce buffers constants in
  Lwt.catch 
    (fun () ->
      Fsm.loop ~trace produce 
	(machine constants) 
	(forced_master_suggest constants (current_n,current_i))
    ) 
    (fun e ->
      log ~me "FSM BAILED due to uncaught exception %s" (Printexc.to_string e)
      >>= fun () -> Lwt.fail e
    )

let enter_simple_paxos constants buffers current_i vo =
  let me = constants.me in
  log ~me "+starting FSM election." >>= fun () ->
  let current_n = Sn.start in
  let trace = trace_transition me in
  let produce = paxos_produce buffers constants in
  Lwt.catch 
    (fun () ->
      Fsm.loop ~trace produce 
	(machine constants) 
	(election_suggest constants (current_n, current_i, vo))
    ) 
    (fun e ->
      log ~me "FSM BAILED (run_election) due to uncaught exception %s" 
	(Printexc.to_string e) 
      >>= fun () -> Lwt.fail e
    )

let enter_read_only constants buffers current_i vo =
  let me = constants.me in
  log ~me "+starting FSM for read_only." >>= fun () ->
  let current_n = 0L in
  let trace = trace_transition me in
  let produce = paxos_produce buffers constants in
  Lwt.catch
    (fun () ->
      Fsm.loop ~trace produce
	(machine constants)
	(read_only constants (current_n, current_i, vo))
    )
    (fun exn ->
      Lwt_log.info ~exn "READ ONLY BAILS OUT" >>= fun () ->
      Lwt.fail exn
    )

let expect_run_forced_slave constants buffers expected step_count new_i =
  let me = constants.me in
  log ~me "+starting forced_slave FSM with expect" >>= fun () ->
  let produce = paxos_produce buffers constants in
  Lwt.catch 
    (fun () ->
      Fsm.expect_loop expected step_count Start_transition produce 
	(machine constants) (Slave.slave_fake_prepare constants new_i)
    ) 
    (fun e ->
      log ~me "FSM BAILED due to uncaught exception %s" (Printexc.to_string e) 
      >>= fun () -> Lwt.fail e
    )

let expect_run_forced_master constants buffers expected step_count current_n current_i =
  let me = constants.me in
  let produce = paxos_produce buffers constants in
  log ~me "+starting forced_master FSM with expect" >>= fun () ->
  Lwt.catch 
    (fun () ->
      Fsm.expect_loop expected step_count Start_transition produce 
	(machine constants) 
	(forced_master_suggest constants (current_n,current_i))
    ) 
    (fun e ->
      log ~me "FSM BAILED due to uncaught exception %s" (Printexc.to_string e)
      >>= fun () -> Lwt.fail e
    )