/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Arguments;
use std::io;
use std::process;

/// Like `panic!`, but aborts the process instead of unwinding.
///
/// Although we compile buck2 with `panic=abort`, this is safer because
/// others may copy-paste code.
#[macro_export]
macro_rules! rtabort {
    ($m:literal) => {
        $crate::rtabort::rtabort_impl_fixed_string(
            file!(),
            line!(),
            $m,
        )
    };
    ($($t:tt)*) => {
        $crate::rtabort::rtabort_impl(
            file!(),
            line!(),
            format_args!($($t)*),
        )
    };
}

#[cold]
pub fn rtabort_impl_fixed_string(file: &str, line: u32, message: &str) -> ! {
    rtabort_impl(file, line, format_args!("{}", message));
}

#[cold]
pub fn rtabort_impl(file: &str, line: u32, msg: Arguments) -> ! {
    // Make sure we abort even if formatting panics.
    let _abort = AbortOnDrop;

    // `eprintln!` followed by `abort` does not print anything in tests.
    io::Write::write_fmt(
        &mut io::stderr(),
        format_args!("{}:{}: abort: {}\n", file, line, msg),
    )
    .ok();

    // Tell the compiler that we never return.
    process::abort();
}

pub struct AbortOnDrop;

impl Drop for AbortOnDrop {
    fn drop(&mut self) {
        process::abort();
    }
}

#[cfg(test)]
mod tests {
    fn _test_compiles_fixed_string() {
        rtabort!("test");
    }

    fn _test_compiles_with_format_args() {
        rtabort!("test {}", 17);
    }

    #[test]
    fn test_rtabort() {
        // Uncomment to test.
        // rtabort!("test {}", 17);
        // rtabort!("test {}", { panic!(); 17 });
    }
}
