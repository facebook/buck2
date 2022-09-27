(* [get_base_value] is an ffi defined in rust *)
external get_base_value: unit -> int = "get_base_value"

let rec fib n =
  if n < 2 then
    get_base_value () (* [get_base_value] is invoked here *)
  else
    fib(n-1) + fib(n-2)

let format_result n =
  Printf.sprintf "Result is: %d\n" n

(* [fib] & [format_result] are registered so that they can be called via the C
   API *)
let _ = Callback.register "fib" fib
let _ = Callback.register "format_result" format_result
