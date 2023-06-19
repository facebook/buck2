/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[no_mangle]
pub unsafe extern "C" fn caml_print_hello(_: usize) -> usize {
    println!("Hello Rust!\n");
    return 0;
}
