/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::process;

/// Like `panic!`, but aborts the process instead of unwinding.
///
/// Although we compile buck2 with `panic=abort`, this is safer because
/// others may copy-paste code.
#[macro_export]
macro_rules! rtabort {
    ($($t:tt)*) => {
        {
            // Make sure we abort even if formatting panics.
            let _abort = $crate::rtabort::AbortOnDrop;

            // `eprintln!` followed by `abort` does not print anything in tests.
            let _ = std::io::Write::write_fmt(
                &mut std::io::stderr(),
                format_args!("{}:{}: abort: {}\n", file!(), line!(), format_args!($($t)*)));

            // Tell the compiler that we never return.
            std::process::abort();
        }
    }
}

pub struct AbortOnDrop;

impl Drop for AbortOnDrop {
    fn drop(&mut self) {
        process::abort();
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_rtabort() {
        // Uncomment to test.
        // rtabort!("test {}", 17);
        // rtabort!("test {}", { panic!(); 17 });
    }
}
