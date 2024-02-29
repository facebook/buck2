/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use three_billion_instructions::three_billion_instructions;

/// Run 3B instructions.
fn main() -> Result<(), three_billion_instructions::Error> {
    three_billion_instructions()
}
