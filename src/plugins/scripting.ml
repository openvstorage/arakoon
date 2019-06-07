open Joy.Language
module Interpreter = struct

  let _fold_range (store : #Arakoon_registry.read_user_db) first finc last linc max_n f acc =
    store # with_cursor
      (fun cursor_db ->
        Arakoon_registry.Cursor_store.fold_range
          cursor_db
          first finc last linc max_n
          f
          acc
      )

  let ok s p = Ok (s,p)

  let _eval_put_store store stack p =
    match stack with
    | NONE     :: STRING k :: rest  -> let () = store # put k None in ok rest p
    | STRING v :: STRING k :: rest  -> let () = store # put k (Some v) in ok rest p
    | _ -> Error ("put_store arguments", stack, p)

  let _no_eval_put_store _ stack p = Error ("put_store not allowed", stack, p)

  let rec eval_generic (store: #Arakoon_registry.read_user_db) eval_put_store stack p =
    let open Joy in
    let open Language in
    let () = Plugin_helper.debug_f "EVAL s:%s p:%s" (p_to_string stack) (p_to_string p) in

    let push w stack p = ok (w :: stack) p in
    let eval_dup stack p =
      match stack with
      | h :: _ -> ok (h :: stack) p
      | []     -> Error ("dup on empty stack", stack, p)

    in
    let eval_int_binop f stack p =
      match stack with
      | INT a :: INT b :: rest -> ok (INT (f a b) :: rest) p
      | _ -> Error("int binop params", stack, p)
    in
    let eval_fold stack p =
      match stack with
      | (LIST _) :: acc :: LIST [] :: rest -> ok (acc::rest) p
      | (LIST f) :: acc :: LIST (item0::items) :: rest ->
         begin
           let pstep  = f in
           let sstep  = item0 :: acc :: [] in
           match eval_generic store eval_put_store sstep pstep with
           | Ok rstack ->
              let stack' = (LIST f) :: rstack @ (LIST items :: rest) in
              ok stack' (FOLD ::p)
           | Error (msg,s,p) -> Error (msg,s,p)
         end
      | _ -> Error ("fold arguments", stack, p)
    in
    let eval_fold_range stack p =
      begin
        match stack with
        | (LIST f) :: acc :: LIST params :: rest ->
           begin
             try
               let get_last = function
                 | NONE -> None
                 | STRING s -> Some s
                 | _ -> failwith "fold_range: type of last"
               in
               let first, finc, last, linc, max_n =
                 match params with
                 | [STRING first; BOOL finc; maybe_last; BOOL linc; INT max_n] ->
                    first, finc, get_last maybe_last, linc, max_n
                 | [STRING first; BOOL finc; maybe_last; BOOL linc;] ->
                    first, finc, get_last maybe_last, linc, max_int
                 | [STRING first; BOOL finc;] ->
                    first, finc, None, true, max_int
                 | _ -> failwith "fold_range : range parameters"
               in
               let stack' =
                 let (_count, (acc: stack)) =
                   let f =
                     (fun _cursor_db key count acc ->
                           let k = Key.get key in
                           let () = Plugin_helper.debug_f "KEY=%S count=%i" (Key.get key) count in
                           let sstep = STRING k :: acc in
                           let acc' =
                             match eval_generic store eval_put_store sstep f with
                             | Ok stack' -> stack'
                             | Error (msg,_s,_p) -> failwith msg
                           in
                           acc'
                     )
                   in
                   _fold_range store first finc last linc max_n f (acc :: [])
                 in
                 acc @ rest
               in
               ok stack' p
             with exn -> Error (Printexc.to_string exn, stack, p)
           end
        | _ -> Error ("fold_range arguments", stack, p)
      end
    in
    let eval_get_store stack p =
      match stack with
      | STRING s :: rest ->
         let v = match store # get s with
           | None   -> NONE
           | Some s -> STRING s
         in ok (v::rest) p
      | _ -> Error ("get arguments", stack, p)
    in
    
    let eval_cons stack p =
      match stack with
      | LIST vs :: v :: rest -> let vl = LIST (v :: vs) in ok (vl :: rest) p
      | _ -> Error ("cons arguments", stack, p)
    in
    let eval_pop stack p =
      match stack with
      | [] -> Error ("pop on empty stack", stack, p)
      | _ :: stack' -> ok stack' p
    in
    let eval_swap stack p =
      match stack with
      | []  -> Error ("swap on empty stack", stack, p)
      | [_] -> Error("swap on stack depth 1", stack, p)
      | x :: y :: rest  -> ok (y :: x :: rest) p
    in
    let eval_reverse stack p =
      match stack with
      | LIST l :: rest -> ok (LIST (List.rev l) :: rest) p
      | _  -> Error ("reverse arguments", stack, p)
    in
    let eval_substring stack p =
      match stack with
      | INT len :: INT off :: STRING s :: rest ->
         let sub = String.sub s off len in
         ok (STRING sub :: rest) p
      | _ -> Error ("substring arguments", stack, p)
    in
    let eval_serialize stack p =
      match stack with
      | LIST items :: rest ->
         let s =
           let buf = Buffer.create 128 in
           let () = p_to buf items in
           Buffer.contents buf
         in
         ok (STRING s :: rest) p
      | _ -> Error ("serialize needs list", stack, p)
    in
    let eval_deserialize stack p =
      match stack with
      | STRING s :: rest ->
         let items = let buf = Llio.make_buffer s 0 in
                     p_from buf
         in
         ok (LIST items :: rest) p
      | _ -> Error ("deserialize needs string", stack, p)
    in
    let eval_eq stack p =
      match stack with
      | BOOL x   :: BOOL y   :: rest   -> ok ( BOOL (x = y) :: rest) p
      | INT x    :: INT y    :: rest   -> ok ( BOOL (x = y) :: rest) p
      | STRING x :: STRING y :: rest   -> ok ( BOOL (x = y) :: rest) p
      | _ -> Error("= arguments", stack, p)
    in
    match p with
      | [] -> Ok stack
      | i0 :: p' ->
         begin
           let r =
             match i0 with
             | NONE       -> push i0 stack p'
             | BOOL _     -> push i0 stack p'
             | INT  _     -> push i0 stack p'
             | STRING _   -> push i0 stack p'
             | LIST _     -> push i0 stack p'
             | CONS       -> eval_cons stack p'
             | DUP        -> eval_dup stack p'
             | POP        -> eval_pop stack p'
             | SWAP       -> eval_swap stack p'
             | EQ         -> eval_eq   stack p'
             | FOLD       -> eval_fold stack p'
             | FOLD_RANGE -> eval_fold_range stack p'
             | ADD        -> eval_int_binop ( + ) stack p'
             | MUL        -> eval_int_binop ( * ) stack p'
             | DIV        -> eval_int_binop ( / ) stack p'
             | GET_STORE  -> eval_get_store stack p'
             | PUT_STORE  -> eval_put_store store stack p'
             | REVERSE    -> eval_reverse stack p'
             | SUBSTRING  -> eval_substring stack p'
             | SERIALIZE  -> eval_serialize stack p'
             | DESERIALIZE-> eval_deserialize stack p'
             | _ ->
                let msg = Printf.sprintf "todo:%s" (i_to_string i0) in
                Error (msg, stack, p)
           in
           match r with
           | Ok (stack,p') -> eval_generic store eval_put_store stack p'
           | Error (msg, stack, p)  -> Error (msg, stack, p)
         end

  let result_to buf =
    let open Joy in
    let open Language in
    function
    | Ok stack   ->
       Llio.int8_to buf 1;
       let () = Plugin_helper.debug_f "result_to: stack = %s\n%!" (p_to_string stack) in
       p_to buf stack
    | Error (msg, stack, program) ->
       Llio.int8_to buf 2;
       Llio.tuple3_to Llio.string_to p_to p_to buf (msg, stack, program)

  let run_generic store eval_put_store script =
    let buffer = Llio.make_buffer script 0 in
    let program = Joy.Language.p_from buffer in
    let result = eval_generic store eval_put_store [] program in
    let r =
      let b = Buffer.create 80 in
      let () = result_to b result in
      Buffer.contents b
    in
    r

  let run_query  store script = run_generic store _no_eval_put_store script
  let run_update store script = run_generic store _eval_put_store script
end
