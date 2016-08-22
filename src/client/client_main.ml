(*
Copyright (2010-2014) INCUBAID BVBA

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*)

open Node_cfg
open Network
open Statistics
open Client_helper
open Lwt

let default_create_client_context = default_create_client_context
let with_connection = with_connection
let with_connection' = with_connection'

let with_client ~tls ~tcp_keepalive node_cfg cluster_id f =
  with_client' ~tls ~tcp_keepalive (Node_cfg.node_cfg_to_node_client_cfg node_cfg) cluster_id f

let with_remote_nodestream ~tls ~tcp_keepalive node_cfg cluster_id f =
  let open Node_cfg in
  let addrs = List.map (fun ip -> make_address ip node_cfg.client_port) node_cfg.ips in
  let do_it _addr connection =
    Remote_nodestream.make_remote_nodestream cluster_id connection >>= f
  in
  with_connection' ~tls ~tcp_keepalive addrs do_it

let run f = Lwt_extra.run f ; 0

let ping ~tls ~tcp_keepalive ip port cluster_id =
  let do_it connection =
    let t0 = Unix.gettimeofday () in
    Arakoon_remote_client.make_remote_client cluster_id connection
    >>= fun (client: Arakoon_client.client) ->
    client # ping "cucu" cluster_id >>= fun s ->
    let t1 = Unix.gettimeofday() in
    let d = t1 -. t0 in
    Lwt_io.printlf "%s\ntook\t%f" s d
  in
  let sa = make_address ip port in
  let t () = with_connection ~tls ~tcp_keepalive sa do_it in
  run t




let find_master cluster_cfg ~ssl_cfg =
  let cluster_cfg' = Node_cfg.to_client_cfg cluster_cfg ~ssl_cfg in
  let tcp_keepalive = cluster_cfg.Node_cfg.tcp_keepalive in
  let open MasterLookupResult in
  find_master' ~tcp_keepalive cluster_cfg' >>= function
    | Found (node_name, _) -> return node_name
    | No_master -> Lwt.fail (Failure "No Master")
    | Too_many_nodes_down -> Lwt.fail (Failure "too many nodes down")
    | Unknown_node (n, _) -> return n (* Keep original behaviour *)
    | Exception exn -> Lwt.fail exn


let with_master_client cfg_url ~ssl_cfg f =
  Arakoon_config_url.retrieve_cfg cfg_url >>= fun ccfg ->
  find_master ccfg ~ssl_cfg >>= fun master_name ->
  let open Node_cfg in
  let master_cfg = List.hd (List.filter (fun cfg -> cfg.node_name = master_name) ccfg.cfgs) in
  with_client ~tls:(ccfg |> Node_cfg.to_client_cfg ~ssl_cfg |> get_tls_from_cluster_cfg)
              ~tcp_keepalive:ccfg.tcp_keepalive
              master_cfg
              ccfg.cluster_id
              f

let set cfg_name ~ssl_cfg key value =
  let t () = with_master_client cfg_name ~ssl_cfg (fun client -> client # set key value)
  in run t

let get cfg_name ~ssl_cfg key =
  let f (client:Arakoon_client.client) =
    client # get key >>= fun value ->
    Lwt_io.printlf "%S%!" value
  in
  let t () = with_master_client cfg_name ~ssl_cfg f in
  run t


let get_key_count cfg_name ~ssl_cfg () =
  let f (client:Arakoon_client.client) =
    client # get_key_count () >>= fun c64 ->
    Lwt_io.printlf "%Li%!" c64
  in
  let t () = with_master_client cfg_name ~ssl_cfg f in
  run t

let delete cfg_name ~ssl_cfg key =
  let t () = with_master_client cfg_name ~ssl_cfg (fun client -> client # delete key )
  in
  run t

let delete_prefix cfg_name ~ssl_cfg prefix =
  let t () = with_master_client cfg_name ~ssl_cfg (
      fun client -> client # delete_prefix prefix >>= fun n_deleted ->
        Lwt_io.printlf "%i" n_deleted
    )
  in
  run t

let nop cfg_name ~ssl_cfg =
  let t () =
    with_master_client ~ssl_cfg
      cfg_name
      (fun client -> client # nop ())
  in
  run t


let prefix cfg_name ~ssl_cfg prefix prefix_size =
  let t () = with_master_client cfg_name ~ssl_cfg
               (fun client ->
                  client # prefix_keys prefix prefix_size >>= fun keys ->
                  Lwt_list.iter_s (fun k -> Lwt_io.printlf "%S" k ) keys >>= fun () ->
                  Lwt.return ()
               )
  in
  run t

let range_entries cfg_name ~ssl_cfg left linc right rinc max_results =
  let t () =
    with_master_client ~ssl_cfg
      cfg_name
      (fun client ->
        client # range_entries ~consistency:Arakoon_client.Consistent ~first:left ~finc:linc ~last:right ~linc:rinc ~max:max_results >>= fun entries ->
       Lwt_list.iter_s (fun (k,v) -> Lwt_io.printlf "%S %S" k v ) entries >>= fun () ->
       let size = List.length entries in
       Lwt_io.printlf "%i listed" size >>= fun () ->
       Lwt.return ()
      )
  in
  run t

let rev_range_entries cfg_name ~ssl_cfg left linc right rinc max_results =
  let t () =
    with_master_client
      cfg_name ~ssl_cfg
      (fun client ->
       client # rev_range_entries ~consistency:Arakoon_client.Consistent ~first:left ~finc:linc ~last:right ~linc:rinc ~max:max_results >>= fun entries ->
       Lwt_list.iter_s (fun (k,v) -> Lwt_io.printlf "%S %S" k v ) entries >>= fun () ->
       let size = List.length entries in
       Lwt_io.printlf "%i listed" size >>= fun () ->
       Lwt.return ()
      )
  in
  run t

let user_function cfg_name ~ssl_cfg name arg =
  let t () =
    with_master_client ~ssl_cfg
      cfg_name
      (fun client ->
       client # user_function name arg >>= fun res ->
       Lwt_io.printlf "res = %s" (Log_extra.string_option2s res) >>= fun () ->
       Lwt.return ()
      )
  in
  run t

let benchmark
      cfg_name ~ssl_cfg key_size value_size tx_size max_n n_clients
      scenario_s =
  Lwt_io.set_default_buffer_size 32768;
  let scenario = Ini.p_string_list scenario_s in
  let t () =
    let with_c = with_master_client cfg_name ~ssl_cfg in
    Benchmark.benchmark
      ~with_c ~key_size ~value_size ~tx_size ~max_n n_clients scenario
  in
  run t


let expect_progress_possible cfg_name ~ssl_cfg =

  let f client =
    client # expect_progress_possible () >>= fun b ->
    Lwt_io.printlf "%b" b
  in
  let t () = with_master_client cfg_name ~ssl_cfg f
  in
  run t


let statistics cfg_name ~ssl_cfg =
  let f client =
    client # statistics () >>= fun statistics ->
    let rep = Statistics.string_of statistics in
    Lwt_io.printl rep
  in
  let t () = with_master_client cfg_name ~ssl_cfg f
  in run t

let who_master cfg_url ~ssl_cfg () =

  let t () =
    Arakoon_config_url.retrieve_cfg cfg_url >>= fun cluster_cfg ->
    find_master cluster_cfg ~ssl_cfg >>= fun master_name ->
    Lwt_io.printl master_name
  in
  run t

let _cluster_and_node_cfg node_name' cfg_url =
  Arakoon_config_url.retrieve_cfg cfg_url >>= fun cluster_cfg ->
  let _find cfgs =
    let rec loop = function
      | [] -> failwith (node_name' ^ " is not known in config " ^ (Arakoon_config_url.to_string cfg_url))
      | cfg :: rest ->
        if cfg.Node_cfg.node_name = node_name' then cfg
        else loop rest
    in
    loop cfgs
  in
  let node_cfg = _find cluster_cfg.Node_cfg.cfgs in
  Lwt.return (cluster_cfg, node_cfg)

let node_state node_name' cfg_name ~ssl_cfg =
  let f client =
    client # current_state () >>= fun state ->
    Lwt_io.printl state
  in

  let t () =
    _cluster_and_node_cfg node_name' cfg_name
    >>= fun (cluster_cfg, node_cfg) ->
    let open Node_cfg in
    let cluster = cluster_cfg.cluster_id in
    let tcp_keepalive = cluster_cfg.tcp_keepalive in
    with_client
      ~tls:(cluster_cfg |> Node_cfg.to_client_cfg ~ssl_cfg |> get_tls_from_cluster_cfg)
      ~tcp_keepalive
      node_cfg cluster f
  in
  run t



let node_version node_name' cfg_name ~ssl_cfg =
  let open Node_cfg in
  let t () =
    _cluster_and_node_cfg node_name' cfg_name >>= fun (cluster_cfg, node_cfg) ->
    let tcp_keepalive = cluster_cfg.tcp_keepalive in
    let cluster = cluster_cfg.cluster_id in
    with_client
      ~tls:(cluster_cfg |> Node_cfg.to_client_cfg ~ssl_cfg |> get_tls_from_cluster_cfg)
      ~tcp_keepalive node_cfg cluster
      (fun client ->
         client # version () >>= fun (major,minor,patch, info) ->
         Lwt_io.printlf "%i.%i.%i" major minor patch >>= fun () ->
         Lwt_io.printl info
      )
  in
  run t
