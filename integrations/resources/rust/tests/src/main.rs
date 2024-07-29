/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::process::Command;

fn main() {
    println!("Hello from resources.rs. Will now execute the `hello_binary` resource.");
    let hello_path = buck_resources::get("buck2/integrations/resources/rust/hello_binary").unwrap();
    if let Err(err) = Command::new(&hello_path).status() {
        panic!("Failed to execute {}: {}", hello_path.display(), err);
    }
}

#[test]
fn resource_exists_in_unittest() {
    buck_resources::get("buck2/integrations/resources/rust/hello_binary").unwrap();
}
