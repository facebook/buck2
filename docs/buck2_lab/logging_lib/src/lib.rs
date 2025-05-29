/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub fn info(message: &str) {
    println!("[INFO] {}", message);
}

pub fn warning(message: &str) {
    println!("\x1b[33m[WARNING] {}\x1b[0m", message);
}

pub fn error(message: &str) {
    println!("\x1b[31m[ERROR] {}\x1b[0m", message);
}
