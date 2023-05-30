(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 *)

let _: unit = Al.print_hello () (* ok *)
let _: unit = Al.B.print_hello () (* ok *)

(*
let _: unit = Al__.A.print_hello () (* ok *)
let _: unit = Al__.B.print_hello () (* ok *)
let _: unit = Al__.C.print_hello () (* ok *)
*)

(*
let _: unit = Al__A.print_hello () (* ok *)
let _: unit = Al__B.print_hello () (* ok *)
let _: unit = Al__C.print_hello () (* ok *)
*)

(*
let _: unit = A.print_hello () (* Error: Unbound module A *)
let _: unit = B.print_hello () (* Error: Unbound module B *)
let _: unit = Al.A.print_hello () (* Error: Unbound module Al.A *)
let _: unit = Al.C.print_hello () (* Error: Unbound module Al.C *)
*)
