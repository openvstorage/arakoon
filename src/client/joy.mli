module Language : sig
  type instruction =
    | NONE
    | BOOL of bool
    | INT  of int
    | ADD
    | MUL
    | DIV                        (** example: 6 3 / => 2 *)
    | STRING of string
    | LIST of instruction list
    | I
    | EQ                         (** X Y -> B *)
    | FOLD                       (** example: [1 2 3 4] 0 [+] fold => 10 *)
    | POP
    | DUP
    | SWAP
    | GET_STORE
    | PUT_STORE
    | FOLD_RANGE                 (** ["a" true "b" false] 0 [pop 1 add] fold_range => 42 (* all the kv-pairs in the range *) *)
    | CONS                       (** example: 42 [] cons.       => [42]        *)
    | REVERSE                    (** example: [1 2 3 4] reverse => [4 3 2 1]   *)
    | SUBSTRING                  (** stack: len :: off :: s => (sub s off len) *)
    | SERIALIZE
    | DESERIALIZE
    
  type program = instruction list
  type stack = program           (* it's homomorphic *)
  type error = string * stack * program

  val i_to_string : instruction -> string
  val p_to_string : program -> string

  val p_to : Buffer.t -> program -> unit
  val p_from : Llio.buffer -> program
  val result_from : Llio.buffer -> (stack, (string * stack * program)) result
  val is_update : program -> bool
end

