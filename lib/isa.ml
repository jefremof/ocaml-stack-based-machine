open Util
open Core

type word = int [@@deriving eq, sexp, ord, show { with_path = false }]

type opcode =
  | PC
  | JMP
  | JMP0
  | JMPP
  | CALL
  | RET
  | NOP
  | NOT
  | AND
  | XOR
  | PLUS
  | MUL2
  | DIV2
  | LIT
  | DUP
  | DROP
  | OVER
  | TOR
  | FROMR
  | TOA
  | FROMA
  | LOADAP
  | LOADRP
  | LOADA
  | STOREAP
  | STORERP
  | STOREA
  | HLT
[@@deriving eq, enum, sexp, ord, show { with_path = false }]

let get_args_number : opcode -> int = function
  | LIT | JMP | JMP0 | JMPP | CALL -> 1
  | _ -> 0

type argument = Abs of word | Label of string
[@@deriving eq, sexp, ord, show { with_path = false }]

let parse_arg (str : string) : argument =
  Option.value_map (Int.of_string_opt str) ~default:(Label str) ~f:(fun x ->
      Abs x)

type instruction = {
  opcode : opcode;
  args : argument list;
  label : string option;
      [@printer fun fmt x -> fprintf fmt "%s" (show_string_opt x)]
}
[@@deriving eq, sexp, ord, show { with_path = false }, fields ~getters]

type batch = opcode list [@@deriving sexp, show { with_path = false }]

let batch_to_word (batch : batch) : word =
  let enums = List.map batch ~f:opcode_to_enum in
  List.fold_right enums ~f:(fun y acc -> y + (acc * 32)) ~init:0
