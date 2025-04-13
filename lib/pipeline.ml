open Isa
open Core
open Util

(* Program memory cell with assembly labels *)
type asm_cell = (batch, argument) Either.t [@@deriving sexp]

(* Program memory cell with instructions unencoded *)
type cell = (batch, word) Either.t [@@deriving sexp]
type mapping = (string, word) List.Assoc.t [@@deriving sexp]

let parse_opcode : string -> opcode = function
  | "+" -> PLUS
  | "2/" -> DIV2
  | "2*" -> MUL2
  | "R>" -> FROMR
  | ">R" -> TOR
  | "A>" -> FROMA
  | ">A" -> TOA
  | "JMP+" -> JMPP
  | "@A+" -> LOADAP
  | "@R+" -> LOADRP
  | "@A" -> LOADA
  | "!A+" -> STOREAP
  | "!R+" -> STORERP
  | "!A" -> STOREA
  | s -> Sexp.of_string s |> opcode_of_sexp

let rec parse_symbols : string list -> instruction list =
  let aux label head tail =
    let opcode = parse_opcode head in
    let arguments, rest = List.split_n tail (get_args_number opcode) in
    let args = List.map ~f:parse_arg arguments in
    { opcode; args; label } :: parse_symbols rest
  in
  function
  | [] -> []
  | x :: y :: xs when is_label x ->
      let label = String.chop_suffix_exn x ~suffix:":" in
      aux (Option.some label) y xs
  | x :: xs -> aux None x xs

let parse_source : string -> instruction list =
  Fn.compose parse_symbols string_to_symbols

type group = {
  batch : opcode list;
  label : string option;
  args : argument list;
  map : mapping;
  result : asm_cell list;
}

let blank_group = { batch = []; label = None; args = []; map = []; result = [] }
let reset_batch group = { group with batch = []; args = []; label = None }
let fill_batch : batch -> batch = fill_rest 6 PC

let is_final (x : instruction) : bool =
  match x.opcode with CALL | JMP | RET | HLT -> true | _ -> false

let group_instructions (code : instruction list) : asm_cell list * mapping =
  let rec aux rest (gr : group) : group =
    let address = List.length gr.result in
    let push_label group : group =
      let add key = List.Assoc.add gr.map ~equal:String.equal key address in
      let map = Option.value_map group.label ~default:gr.map ~f:add in
      { group with map }
    in
    let push { batch; args; label; _ } : group =
      let packed_batch = List.rev batch |> fill_batch |> Either.first in
      let packed_args = List.map args ~f:Either.second in
      let result = packed_args @ (packed_batch :: gr.result) in
      { batch; args; label; result; map = gr.map } |> push_label |> reset_batch
    in
    let next_label = List.hd rest |> Option.bind ~f:label in
    let conflict = Option.is_some next_label && not (List.is_empty gr.batch) in
    let old_pushed = push gr in
    match rest with
    | [] -> if List.is_empty gr.batch then gr else old_pushed
    | _ :: _ when conflict -> aux rest old_pushed
    | x :: xs ->
        let batch = x.opcode :: gr.batch in
        let args = x.args @ gr.args in
        let should_push = List.length batch = 5 || is_final x in
        let new_gr = { gr with batch; args; label = next_label } in
        aux xs (if should_push then push new_gr else push_label new_gr)
  in
  let gr = aux code blank_group in
  (List.rev gr.result, gr.map)

let resolve_labels ((cells, map) : asm_cell list * mapping) : cell list =
  let second : argument -> word = function
    | Abs n -> n
    | Label label -> List.Assoc.find_exn map ~equal:String.equal label
  in
  List.map cells ~f:(Either.map ~first:Fn.id ~second)

let encode_instructions : cell list -> word list =
  List.map ~f:(Either.value_map ~first:batch_to_word ~second:Fn.id)

let get_main_address map = List.Assoc.find_exn map ~equal:String.equal "main"

let pipe_main (s : string) : word list * word =
  let groups, map = parse_source s |> group_instructions in
  let resolved = resolve_labels (groups, map) in
  (encode_instructions resolved, get_main_address map)

let pipe : string -> word list = Fn.compose Tuple.T2.get1 pipe_main

let verbose_pipe_main (s : string) : word list * word =
  let instructions = parse_source s in
  let groups, map = group_instructions instructions in
  let resolved = resolve_labels (groups, map) in
  printf "%s\n" ([%show: instruction list] instructions);
  print_s ([%sexp_of: asm_cell list] groups);
  print_s ([%sexp_of: cell list] resolved);
  (encode_instructions resolved, get_main_address map)

let verbose_pipe : string -> word list =
  Fn.compose Tuple.T2.get1 verbose_pipe_main
