open Core

let read_file file = Stdlib.In_channel.with_open_bin file In_channel.input_all

let show_string_opt =
  let f s = "\"" ^ s ^ "\"" in
  Option.value_map ~default:"\"\"" ~f

let string_not_empty s = not (String.is_empty s)

let string_to_symbols s =
  let on = [ ' '; '\n' ] in
  String.split_on_chars s ~on
  |> List.map ~f:String.strip
  |> List.filter ~f:string_not_empty

let is_label = String.is_suffix ~suffix:":"
let repeat n x = List.init n ~f:(Fn.const x)

let fill_rest n (x : 'a) (xs : 'a list) : 'a list =
  xs @ repeat (n - List.length xs) x

let replace pos a = List.mapi ~f:(fun i x -> if i = pos then a else x)
