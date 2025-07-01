(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 *)

type t = {
  foo: int;
  bar: string;
}
[@@deriving record_selectors]

let r: t = { foo = 4; bar = "quux" }
let (): unit = Printf.printf "%d %s\n" (foo r) (bar r)
