module Language = struct

  type instruction =
    | NONE
    | BOOL of bool
    | INT  of int
    | ADD
    | MUL
    | DIV
    | STRING of string
    | LIST of instruction list
    | I
    | EQ
    | FOLD
    | POP
    | DUP
    | SWAP
    | GET_STORE
    | PUT_STORE
    | FOLD_RANGE
    | CONS
    | REVERSE
    | SUBSTRING
    | SERIALIZE
    | DESERIALIZE
  type program = instruction list

  let rec i_to_string = function
    | NONE           -> "none"
    | BOOL b         -> Printf.sprintf "%b" b
    | INT i          -> Printf.sprintf "%i" i
    | ADD            -> "+"
    | MUL            -> "*"
    | DIV            -> "/"
    | STRING s       -> Printf.sprintf "%S" s
    | GET_STORE      -> "get_store"
    | PUT_STORE      -> "put_store"
    | LIST ws        -> Printf.sprintf "[%s]" (String.concat " " (List.map i_to_string ws))
    | I              -> "i"
    | EQ             -> "="
    | FOLD           -> "fold"
    | FOLD_RANGE     -> "fold_range"
    | POP            -> "pop"
    | DUP            -> "dup"
    | SWAP           -> "swap"
    | CONS           -> "cons"
    | REVERSE        -> "reverse"
    | SUBSTRING      -> "substring"
    | SERIALIZE      -> "serialize"
    | DESERIALIZE    -> "deserialize"

  let p_to_string p = "[" ^ String.concat " " (List.map i_to_string p) ^ "]"

  let rec i_to buf =
    let tag_to = Llio.int8_to buf in
    function
    | NONE       -> tag_to 10;
    | BOOL b     -> tag_to 20; Llio.bool_to buf b
    | INT  i     -> tag_to 30; Llio.int_to buf i
    | ADD        -> tag_to 40
    | MUL        -> tag_to 50
    | DIV        -> tag_to 55
    | STRING s   -> tag_to 60; Llio.string_to buf s
    | LIST ws    -> tag_to 70; Llio.list_to i_to buf ws
    | I          -> tag_to 80
    | EQ         -> tag_to 85
    | FOLD       -> tag_to 90
    | POP        -> tag_to 100
    | DUP        -> tag_to 110
    | SWAP       -> tag_to 120
    | GET_STORE  -> tag_to 130
    | PUT_STORE  -> tag_to 140
    | FOLD_RANGE -> tag_to 150
    | CONS       -> tag_to 160
    | REVERSE    -> tag_to 170
    | SUBSTRING  -> tag_to 180
    | SERIALIZE  -> tag_to 190
    | DESERIALIZE-> tag_to 200

  let rec i_from buf =
    let tag = Llio.int8_from buf in
    match tag with
    | 10  -> NONE
    | 20  -> let b = Llio.bool_from buf in BOOL b
    | 30  -> let i = Llio.int_from buf in INT i
    | 40  -> ADD
    | 50  -> MUL
    | 55  -> DIV
    | 60  -> let s = Llio.string_from buf in STRING s
    | 70  -> let ws = Llio.list_from i_from buf in LIST ws
    | 80  -> I
    | 85  -> EQ
    | 90  -> FOLD
    | 100 -> POP
    | 110 -> DUP
    | 120 -> SWAP
    | 130 -> GET_STORE
    | 140 -> PUT_STORE
    | 150 -> FOLD_RANGE
    | 160 -> CONS
    | 170 -> REVERSE
    | 180 -> SUBSTRING
    | 190 -> SERIALIZE
    | 200 -> DESERIALIZE
    | k -> failwith (Printf.sprintf "%i is not an instruction" k)

  let p_to = Llio.list_to i_to
  let p_from = Llio.list_from i_from

  type stack = program
  type error = string * stack * program

  let result_from buf =
    let tag = Llio.int8_from buf in
    match tag with
    | 1 -> let stack = p_from buf in
           Ok stack
    | 2 -> let (msg, stack, program) =
             Llio.tuple3_from Llio.string_from p_from p_from buf
           in
           Error (msg, stack, program)
    | k -> failwith (Printf.sprintf "%i is not a result" k )

  let rec is_update = function
    | [] -> false
    | PUT_STORE :: _ -> true
    | LIST body :: rest -> is_update body || is_update rest
    | _ :: rest -> is_update rest

end
