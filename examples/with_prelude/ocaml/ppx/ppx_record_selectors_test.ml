(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 *)

type t = {
  foo: int;
  bar: string;
}
[@@deriving record_selectors]

let r: t = { foo = 4; bar = "quux" }
let (): unit = Printf.printf "%d %s\n" (foo r) (bar r)
