open Isa

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

val blank_state : state
val debug_state : state -> state
val step : state -> state

exception Halt
