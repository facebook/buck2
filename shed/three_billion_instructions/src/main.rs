/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/// Run 3B instructions.
fn main() {
    let mut x = 1_000_000_000u64;

    #[cfg(target_arch = "x86_64")]
    unsafe {
        std::arch::asm!(
            "2:",
            "sub {0:r}, 1",
            "cmp {0:r}, 0",
            "jne 2b",
            inout(reg) x,
        );
    }

    #[cfg(target_arch = "aarch64")]
    unsafe {
        std::arch::asm!(
            "2:",
            "sub {0:x}, {0:x}, 1",
            "cmp {0:x}, 0",
            "bne 2b",
            inout(reg) x,
        );
    }

    assert_eq!(x, 0);
}
