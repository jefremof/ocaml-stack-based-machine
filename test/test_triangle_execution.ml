open Test_gullwing
open Core

let%expect_test "Triangle execution" =
  print_execution "samples/triangle" 20;
  [%expect
    {|
    { mem = <opaque>; ds = []; rs = []; pc = 7; mar = 7;
      isr = [PC; PC; PC; PC; PC; PC]; top = 0; a = 0 }

    PC
    { mem = <opaque>; ds = []; rs = []; pc = 8; mar = 8;
      isr = [LIT; CALL; PC; PC; PC; PC]; top = 0; a = 0 }

    LIT
    { mem = <opaque>; ds = [0]; rs = []; pc = 9; mar = 9;
      isr = [CALL; PC; PC; PC; PC]; top = 5; a = 0 }

    CALL
    { mem = <opaque>; ds = [0]; rs = [10]; pc = 0; mar = 0;
      isr = [CALL; PC; PC; PC; PC]; top = 5; a = 0 }

    { mem = <opaque>; ds = [0]; rs = [10]; pc = 1; mar = 1;
      isr = [DUP; JMP0; DUP; LIT; PLUS; PC]; top = 5; a = 0 }

    DUP
    { mem = <opaque>; ds = [5; 0]; rs = [10]; pc = 1; mar = 1;
      isr = [JMP0; DUP; LIT; PLUS; PC]; top = 5; a = 0 }

    JMP0
    { mem = <opaque>; ds = [0]; rs = [10]; pc = 2; mar = 2;
      isr = [DUP; LIT; PLUS; PC]; top = 5; a = 0 }

    DUP
    { mem = <opaque>; ds = [5; 0]; rs = [10]; pc = 2; mar = 2;
      isr = [LIT; PLUS; PC]; top = 5; a = 0 }

    LIT
    { mem = <opaque>; ds = [5; 5; 0]; rs = [10]; pc = 3; mar = 3;
      isr = [PLUS; PC]; top = -1; a = 0 }

    PLUS
    { mem = <opaque>; ds = [5; 0]; rs = [10]; pc = 3; mar = 3; isr = [PC];
      top = 4; a = 0 }

    PC
    { mem = <opaque>; ds = [5; 0]; rs = [10]; pc = 4; mar = 4;
      isr = [CALL; PC; PC; PC; PC; PC]; top = 4; a = 0 }

    CALL
    { mem = <opaque>; ds = [5; 0]; rs = [5; 10]; pc = 0; mar = 0;
      isr = [CALL; PC; PC; PC; PC; PC]; top = 4; a = 0 }

    { mem = <opaque>; ds = [5; 0]; rs = [5; 10]; pc = 1; mar = 1;
      isr = [DUP; JMP0; DUP; LIT; PLUS; PC]; top = 4; a = 0 }

    DUP
    { mem = <opaque>; ds = [4; 5; 0]; rs = [5; 10]; pc = 1; mar = 1;
      isr = [JMP0; DUP; LIT; PLUS; PC]; top = 4; a = 0 }

    JMP0
    { mem = <opaque>; ds = [5; 0]; rs = [5; 10]; pc = 2; mar = 2;
      isr = [DUP; LIT; PLUS; PC]; top = 4; a = 0 }

    DUP
    { mem = <opaque>; ds = [4; 5; 0]; rs = [5; 10]; pc = 2; mar = 2;
      isr = [LIT; PLUS; PC]; top = 4; a = 0 }

    LIT
    { mem = <opaque>; ds = [4; 4; 5; 0]; rs = [5; 10]; pc = 3; mar = 3;
      isr = [PLUS; PC]; top = -1; a = 0 }

    PLUS
    { mem = <opaque>; ds = [4; 5; 0]; rs = [5; 10]; pc = 3; mar = 3; isr = [PC];
      top = 3; a = 0 }

    PC
    { mem = <opaque>; ds = [4; 5; 0]; rs = [5; 10]; pc = 4; mar = 4;
      isr = [CALL; PC; PC; PC; PC; PC]; top = 3; a = 0 }

    CALL
    { mem = <opaque>; ds = [4; 5; 0]; rs = [5; 5; 10]; pc = 0; mar = 0;
      isr = [CALL; PC; PC; PC; PC; PC]; top = 3; a = 0 }

    { mem = <opaque>; ds = [4; 5; 0]; rs = [5; 5; 10]; pc = 1; mar = 1;
      isr = [DUP; JMP0; DUP; LIT; PLUS; PC]; top = 3; a = 0 }

    DUP
    { mem = <opaque>; ds = [3; 4; 5; 0]; rs = [5; 5; 10]; pc = 1; mar = 1;
      isr = [JMP0; DUP; LIT; PLUS; PC]; top = 3; a = 0 }

    JMP0
    { mem = <opaque>; ds = [4; 5; 0]; rs = [5; 5; 10]; pc = 2; mar = 2;
      isr = [DUP; LIT; PLUS; PC]; top = 3; a = 0 }

    DUP
    { mem = <opaque>; ds = [3; 4; 5; 0]; rs = [5; 5; 10]; pc = 2; mar = 2;
      isr = [LIT; PLUS; PC]; top = 3; a = 0 }

    LIT
    { mem = <opaque>; ds = [3; 3; 4; 5; 0]; rs = [5; 5; 10]; pc = 3; mar = 3;
      isr = [PLUS; PC]; top = -1; a = 0 }

    PLUS
    { mem = <opaque>; ds = [3; 4; 5; 0]; rs = [5; 5; 10]; pc = 3; mar = 3;
      isr = [PC]; top = 2; a = 0 }

    PC
    { mem = <opaque>; ds = [3; 4; 5; 0]; rs = [5; 5; 10]; pc = 4; mar = 4;
      isr = [CALL; PC; PC; PC; PC; PC]; top = 2; a = 0 }

    CALL
    { mem = <opaque>; ds = [3; 4; 5; 0]; rs = [5; 5; 5; 10]; pc = 0; mar = 0;
      isr = [CALL; PC; PC; PC; PC; PC]; top = 2; a = 0 }

    { mem = <opaque>; ds = [3; 4; 5; 0]; rs = [5; 5; 5; 10]; pc = 1; mar = 1;
      isr = [DUP; JMP0; DUP; LIT; PLUS; PC]; top = 2; a = 0 }

    DUP
    { mem = <opaque>; ds = [2; 3; 4; 5; 0]; rs = [5; 5; 5; 10]; pc = 1; mar = 1;
      isr = [JMP0; DUP; LIT; PLUS; PC]; top = 2; a = 0 }

    JMP0
    { mem = <opaque>; ds = [3; 4; 5; 0]; rs = [5; 5; 5; 10]; pc = 2; mar = 2;
      isr = [DUP; LIT; PLUS; PC]; top = 2; a = 0 }

    DUP
    { mem = <opaque>; ds = [2; 3; 4; 5; 0]; rs = [5; 5; 5; 10]; pc = 2; mar = 2;
      isr = [LIT; PLUS; PC]; top = 2; a = 0 }

    LIT
    { mem = <opaque>; ds = [2; 2; 3; 4; 5; 0]; rs = [5; 5; 5; 10]; pc = 3;
      mar = 3; isr = [PLUS; PC]; top = -1; a = 0 }

    PLUS
    { mem = <opaque>; ds = [2; 3; 4; 5; 0]; rs = [5; 5; 5; 10]; pc = 3; mar = 3;
      isr = [PC]; top = 1; a = 0 }

    PC
    { mem = <opaque>; ds = [2; 3; 4; 5; 0]; rs = [5; 5; 5; 10]; pc = 4; mar = 4;
      isr = [CALL; PC; PC; PC; PC; PC]; top = 1; a = 0 }

    CALL
    { mem = <opaque>; ds = [2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 10]; pc = 0;
      mar = 0; isr = [CALL; PC; PC; PC; PC; PC]; top = 1; a = 0 }

    { mem = <opaque>; ds = [2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 10]; pc = 1;
      mar = 1; isr = [DUP; JMP0; DUP; LIT; PLUS; PC]; top = 1; a = 0 }

    DUP
    { mem = <opaque>; ds = [1; 2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 10]; pc = 1;
      mar = 1; isr = [JMP0; DUP; LIT; PLUS; PC]; top = 1; a = 0 }

    JMP0
    { mem = <opaque>; ds = [2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 10]; pc = 2;
      mar = 2; isr = [DUP; LIT; PLUS; PC]; top = 1; a = 0 }

    DUP
    { mem = <opaque>; ds = [1; 2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 10]; pc = 2;
      mar = 2; isr = [LIT; PLUS; PC]; top = 1; a = 0 }

    LIT
    { mem = <opaque>; ds = [1; 1; 2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 10];
      pc = 3; mar = 3; isr = [PLUS; PC]; top = -1; a = 0 }

    PLUS
    { mem = <opaque>; ds = [1; 2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 10]; pc = 3;
      mar = 3; isr = [PC]; top = 0; a = 0 }

    PC
    { mem = <opaque>; ds = [1; 2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 10]; pc = 4;
      mar = 4; isr = [CALL; PC; PC; PC; PC; PC]; top = 0; a = 0 }

    CALL
    { mem = <opaque>; ds = [1; 2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 5; 10];
      pc = 0; mar = 0; isr = [CALL; PC; PC; PC; PC; PC]; top = 0; a = 0 }

    { mem = <opaque>; ds = [1; 2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 5; 10];
      pc = 1; mar = 1; isr = [DUP; JMP0; DUP; LIT; PLUS; PC]; top = 0; a = 0 }

    DUP
    { mem = <opaque>; ds = [0; 1; 2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 5; 10];
      pc = 1; mar = 1; isr = [JMP0; DUP; LIT; PLUS; PC]; top = 0; a = 0 }

    JMP0
    { mem = <opaque>; ds = [1; 2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 5; 10];
      pc = 6; mar = 6; isr = [JMP0; DUP; LIT; PLUS; PC]; top = 0; a = 0 }

    { mem = <opaque>; ds = [1; 2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 5; 10];
      pc = 7; mar = 7; isr = [RET; PC; PC; PC; PC; PC]; top = 0; a = 0 }

    RET
    { mem = <opaque>; ds = [1; 2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 10]; pc = 5;
      mar = 5; isr = [RET; PC; PC; PC; PC; PC]; top = 0; a = 0 }

    { mem = <opaque>; ds = [1; 2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 10]; pc = 6;
      mar = 6; isr = [PLUS; PC; PC; PC; PC; PC]; top = 0; a = 0 }

    PLUS
    { mem = <opaque>; ds = [2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 10]; pc = 6;
      mar = 6; isr = [PC; PC; PC; PC; PC]; top = 1; a = 0 }

    PC
    { mem = <opaque>; ds = [2; 3; 4; 5; 0]; rs = [5; 5; 5; 5; 10]; pc = 7;
      mar = 7; isr = [RET; PC; PC; PC; PC; PC]; top = 1; a = 0 }

    RET
    { mem = <opaque>; ds = [2; 3; 4; 5; 0]; rs = [5; 5; 5; 10]; pc = 5; mar = 5;
      isr = [RET; PC; PC; PC; PC; PC]; top = 1; a = 0 }

    { mem = <opaque>; ds = [2; 3; 4; 5; 0]; rs = [5; 5; 5; 10]; pc = 6; mar = 6;
      isr = [PLUS; PC; PC; PC; PC; PC]; top = 1; a = 0 }

    PLUS
    { mem = <opaque>; ds = [3; 4; 5; 0]; rs = [5; 5; 5; 10]; pc = 6; mar = 6;
      isr = [PC; PC; PC; PC; PC]; top = 3; a = 0 }

    PC
    { mem = <opaque>; ds = [3; 4; 5; 0]; rs = [5; 5; 5; 10]; pc = 7; mar = 7;
      isr = [RET; PC; PC; PC; PC; PC]; top = 3; a = 0 }

    RET
    { mem = <opaque>; ds = [3; 4; 5; 0]; rs = [5; 5; 10]; pc = 5; mar = 5;
      isr = [RET; PC; PC; PC; PC; PC]; top = 3; a = 0 }

    { mem = <opaque>; ds = [3; 4; 5; 0]; rs = [5; 5; 10]; pc = 6; mar = 6;
      isr = [PLUS; PC; PC; PC; PC; PC]; top = 3; a = 0 }

    PLUS
    { mem = <opaque>; ds = [4; 5; 0]; rs = [5; 5; 10]; pc = 6; mar = 6;
      isr = [PC; PC; PC; PC; PC]; top = 6; a = 0 }

    PC
    { mem = <opaque>; ds = [4; 5; 0]; rs = [5; 5; 10]; pc = 7; mar = 7;
      isr = [RET; PC; PC; PC; PC; PC]; top = 6; a = 0 }

    RET
    { mem = <opaque>; ds = [4; 5; 0]; rs = [5; 10]; pc = 5; mar = 5;
      isr = [RET; PC; PC; PC; PC; PC]; top = 6; a = 0 }

    { mem = <opaque>; ds = [4; 5; 0]; rs = [5; 10]; pc = 6; mar = 6;
      isr = [PLUS; PC; PC; PC; PC; PC]; top = 6; a = 0 }

    PLUS
    { mem = <opaque>; ds = [5; 0]; rs = [5; 10]; pc = 6; mar = 6;
      isr = [PC; PC; PC; PC; PC]; top = 10; a = 0 }

    PC
    { mem = <opaque>; ds = [5; 0]; rs = [5; 10]; pc = 7; mar = 7;
      isr = [RET; PC; PC; PC; PC; PC]; top = 10; a = 0 }

    RET
    { mem = <opaque>; ds = [5; 0]; rs = [10]; pc = 5; mar = 5;
      isr = [RET; PC; PC; PC; PC; PC]; top = 10; a = 0 }

    { mem = <opaque>; ds = [5; 0]; rs = [10]; pc = 6; mar = 6;
      isr = [PLUS; PC; PC; PC; PC; PC]; top = 10; a = 0 }

    PLUS
    { mem = <opaque>; ds = [0]; rs = [10]; pc = 6; mar = 6;
      isr = [PC; PC; PC; PC; PC]; top = 15; a = 0 }

    PC
    { mem = <opaque>; ds = [0]; rs = [10]; pc = 7; mar = 7;
      isr = [RET; PC; PC; PC; PC; PC]; top = 15; a = 0 }

    RET
    { mem = <opaque>; ds = [0]; rs = []; pc = 10; mar = 10;
      isr = [RET; PC; PC; PC; PC; PC]; top = 15; a = 0 }

    { mem = <opaque>; ds = [0]; rs = []; pc = 11; mar = 11;
      isr = [HLT; PC; PC; PC; PC; PC]; top = 15; a = 0 }

    HLT
    Halt
    |}]
