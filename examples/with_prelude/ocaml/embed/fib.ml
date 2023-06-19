(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 *)

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
