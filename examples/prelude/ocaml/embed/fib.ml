 let rec fib n =
   if n < 2 then
     1
   else
     fib (n - 1) + fib (n - 2)

 let format_result n =
   Printf.sprintf "Result is: %d\n" n

(* [fib] & [format_result] are registered so that they can be called via the C
   API *)
let _ = Callback.register "fib" fib
let _ = Callback.register "format_result" format_result
