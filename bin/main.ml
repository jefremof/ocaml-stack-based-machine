[@@@ocaml.warning "-37"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-26"]

open Core
open Gullwing.Pipeline
open Gullwing.Util
open Gullwing.Machine

let memsize = 20

let triangle_rec =
  "
  tri:
      DUP JMP0 end DUP LIT -1 + CALL tri +
  end:
      RET
  
  main:
      LIT 5
      CALL tri
      HLT
  "

let triangle_tailrec =
  "
  main:
    LIT 5
    LIT 0
    CALL tri
    HLT
  
  tri:
    OVER JMP0 end OVER + >R LIT -1 + R> CALL tri RET
  end:
    >R DROP R> RET
"

let triangle_iter =
  "
  tri:
    LIT 0 >R
  loop:
    DUP JMP0 end R> OVER + >R LIT -1 + JMP loop
  end:
    DROP R> RET

  main:
    LIT 5
    CALL tri
    HLT
  "

let triangle_rec_mem_steps =
  "
    tri:
      DUP JMP0 end DUP LIT -1 + CALL tri + DUP !A+
    end:
      RET
  
    main:
      LIT data
      >A
      LIT 5
      CALL tri
      HLT
    
    data:
      NOP
  "

let () =
  let encoded, pc = verbose_pipe_main triangle_rec in
  let mem = fill_rest memsize 0 encoded in
  let initial = { blank_state with mem; pc; mar = pc } in
  initial |> debug_state |> step |> ignore
