open Isa

type asm_cell [@@deriving sexp]
type cell [@@deriving sexp]
type mapping [@@deriving sexp]

val parse_source : string -> instruction list
val group_instructions : instruction list -> asm_cell list * mapping
val resolve_labels : asm_cell list * mapping -> cell list
val encode_instructions : cell list -> word list
val pipe : string -> word list
val pipe_main : string -> word list * word
val verbose_pipe : string -> word list
val verbose_pipe_main : string -> word list * word
