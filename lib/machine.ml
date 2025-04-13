open Core
open Isa
open Util

type state = {
  mem : word list; [@opaque]
  ds : word list;
  rs : word list;
  pc : word;
  mar : word;
  isr : batch;
  top : word;
  a : word;
}
[@@deriving show { with_path = false }]

let get_opcode (op : word) : opcode =
  Option.value (opcode_of_enum op) ~default:PC

let word_to_batch (word : int) : batch =
  let indices = List.init 6 ~f:(fun i -> (word lsr (5 * i)) land 31) in
  List.map indices ~f:get_opcode

let blank_state =
  {
    mem = [];
    ds = [];
    rs = [];
    pc = 0;
    mar = 0;
    isr = word_to_batch 0;
    top = 0;
    a = 0;
  }

let debug_state s =
  let () = printf "%s\n\n" ([%show: state] s) in
  s

let get_mem (s : state) : word = List.nth_exn s.mem s.mar
let load_isr (s : state) : state = { s with isr = get_mem s |> word_to_batch }
let pcinc s = { s with pc = s.pc + 1; mar = s.pc + 1 }
let pc s = load_isr s |> pcinc
let jmp s = { s with pc = get_mem s; mar = get_mem s } |> debug_state |> pc
let ds_top_ds_pop s = { s with ds = List.tl_exn s.ds; top = List.hd_exn s.ds }
let isr_shift s = { s with isr = List.tl_exn s.isr }

let jmpz s =
  ds_top_ds_pop s |> if s.top = 0 then jmp else Fn.compose pcinc isr_shift

let jmpp s =
  ds_top_ds_pop s |> if s.top > 0 then jmp else Fn.compose pcinc isr_shift

let call s = { s with rs = (s.pc + 1) :: s.rs } |> jmp

let ret s =
  {
    s with
    rs = List.tl_exn s.rs;
    pc = List.hd_exn s.rs;
    mar = List.hd_exn s.rs;
  }
  |> debug_state |> pc

let un_op op s = { s with top = op s.top } |> isr_shift
let not = un_op Int.bit_not
let mul2 = un_op (Int.( * ) 2)
let div2 = un_op (Fn.flip Int.( / ) 2)

let bin_op op s =
  { s with top = op s.top (List.hd_exn s.ds); ds = List.tl_exn s.ds }
  |> isr_shift

let bit_and = bin_op Int.bit_and
let bit_xor = bin_op Int.bit_xor
let plus = bin_op ( + )
let lit s = { s with top = get_mem s; ds = s.top :: s.ds } |> pcinc |> isr_shift
let dup s = { s with ds = s.top :: s.ds } |> isr_shift

let to_rs s =
  { s with top = List.hd_exn s.ds; ds = List.tl_exn s.ds; rs = s.top :: s.rs }
  |> isr_shift

let from_rs s =
  { s with top = List.hd_exn s.rs; ds = s.top :: s.ds; rs = List.tl_exn s.rs }
  |> isr_shift

let to_a s =
  { s with a = s.top; top = List.hd_exn s.ds; ds = List.tl_exn s.ds }
  |> isr_shift

let from_a s = { s with top = s.a; ds = s.top :: s.ds } |> isr_shift

let load s =
  { s with top = get_mem s; ds = s.top :: s.ds; mar = s.pc } |> isr_shift

let store s =
  let mem = replace s.mar s.top s.mem in
  { s with ds = List.tl_exn s.ds; top = List.hd_exn s.ds; mem; mar = s.pc }
  |> isr_shift

let mar_A_plus s = { s with mar = s.a; a = s.a + 1 }

let mar_R_plus s =
  let r = List.hd_exn s.rs in
  { s with mar = r; rs = (r + 1) :: List.tl_exn s.rs }

let load_A_plus s = mar_A_plus s |> debug_state |> load
let load_R_plus s = mar_R_plus s |> debug_state |> load
let load_A s = { s with mar = s.a } |> debug_state |> load
let store_A_plus s = mar_A_plus s |> debug_state |> store
let store_R_plus s = mar_R_plus s |> debug_state |> store
let store_A s = { s with mar = s.a } |> debug_state |> store

let drop s =
  { s with top = List.hd_exn s.ds; ds = List.tl_exn s.ds } |> isr_shift

let over s = { s with ds = s.top :: s.ds; top = List.hd_exn s.ds } |> isr_shift

exception Halt

let rec step (s : state) : state =
  let operation = Option.value (List.hd s.isr) ~default:PC in
  let () = printf "%s\n" ([%show: opcode] operation) in
  let next =
    match operation with
    | PC -> pc s
    | JMP -> jmp s
    | JMP0 -> jmpz s
    | JMPP -> jmpp s
    | CALL -> call s
    | RET -> ret s
    | NOP -> isr_shift s
    | NOT -> not s
    | AND -> bit_and s
    | XOR -> bit_xor s
    | PLUS -> plus s
    | MUL2 -> mul2 s
    | DIV2 -> div2 s
    | LIT -> lit s
    | DUP -> dup s
    | DROP -> drop s
    | OVER -> over s
    | FROMR -> from_rs s
    | TOR -> to_rs s
    | FROMA -> from_a s
    | TOA -> to_a s
    | LOADAP -> load_A_plus s
    | LOADRP -> load_R_plus s
    | LOADA -> load_A s
    | STOREAP -> store_A_plus s
    | STORERP -> store_R_plus s
    | STOREA -> store_A s
    | HLT -> raise Halt
  in
  debug_state next |> step
