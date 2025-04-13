[@@@ocaml.warning "-37"]
[@@@ocaml.warning "-32"]

open Core
open Gullwing.Pipeline
open Gullwing.Isa
open Gullwing.Machine
open Gullwing.Util

let print_parse_instructions file =
  read_file file |> parse_source |> [%show: instruction list] |> printf "%s"

let print_encode_program file =
  read_file file |> pipe |> List.iter ~f:(Printf.printf "0x%X ")

let print_execution file memsize =
  let encoded, pc = read_file file |> pipe_main in
  let mem = fill_rest memsize 0 encoded in
  let run () = debug_state { blank_state with mem; pc; mar = pc } |> step in
  try run () |> ignore with Halt -> printf "Halt"

let%expect_test "Triangle instructions" =
  print_parse_instructions "samples/triangle";
  [%expect
    {|
    [{ opcode = DUP; args = []; label = "tri" };
      { opcode = JMP0; args = [(Label "end")]; label = "" };
      { opcode = DUP; args = []; label = "" };
      { opcode = LIT; args = [(Abs -1)]; label = "" };
      { opcode = PLUS; args = []; label = "" };
      { opcode = CALL; args = [(Label "tri")]; label = "" };
      { opcode = PLUS; args = []; label = "" };
      { opcode = RET; args = []; label = "end" };
      { opcode = LIT; args = [(Abs 5)]; label = "main" };
      { opcode = CALL; args = [(Label "tri")]; label = "" };
      { opcode = HLT; args = []; label = "" }]
    |}]

let%expect_test "Triangle encoded" =
  print_encode_program "samples/triangle";
  [%expect
    {| 0xA6B84E 0x6 0x7FFFFFFFFFFFFFFF 0x4 0x0 0xA 0x5 0x8D 0x5 0x0 0x1B |}]
