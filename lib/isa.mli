type word = int [@@deriving eq, show, sexp, ord]

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

type argument = Abs of word | Label of string
[@@deriving eq, sexp, ord, show { with_path = false }]

type instruction = {
  opcode : opcode;
  args : argument list;
  label : string option;
      [@printer fun fmt x -> fprintf fmt "%s" (show_string_opt x)]
}
[@@deriving ord, sexp, eq, show { with_path = false }, fields ~getters]

type batch = opcode list [@@deriving show, sexp]

val batch_to_word : batch -> word
val parse_arg : string -> argument
val get_args_number : opcode -> int
