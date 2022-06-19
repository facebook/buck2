/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This module provides {e,}print{ln,} macros that return errors when they fail, unlike the std
//! macros, which yield panics. The errors returned by those methods don't make sense to handle in
//! place, and should usually just be propagated in order to lead to a quick exit.

use std::{
    fmt::Arguments,
    io::{self, Write},
};

use buck2_core::exit_result::FailureExitCode;

#[macro_export]
macro_rules! print {
    () => {
        $crate::stdio::_print(std::format_args!(""))
    };
    ($($arg:tt)*) => {
        $crate::stdio::_print(std::format_args!($($arg)*))
    };
}

#[macro_export]
macro_rules! println {
    () => {
        $crate::stdio::_print(std::format_args!("\n"))
    };
    ($fmt:tt) => {
        $crate::stdio::_print(std::format_args!(concat!($fmt, "\n")))
    };
    ($fmt:tt, $($arg:tt)*) => {
        $crate::stdio::_print(std::format_args!(concat!($fmt, "\n"), $($arg)*))
    };
}

#[macro_export]
macro_rules! eprint {
    () => {
        $crate::stdio::_eprint(std::format_args!(""))
    };
    ($($arg:tt)*) => {
        $crate::stdio::_eprint(std::format_args!($($arg)*))
    };
}

#[macro_export]
macro_rules! eprintln {
    () => {
        $crate::stdio::_eprint(std::format_args!("\n"))
    };
    ($fmt:tt) => {
        $crate::stdio::_eprint(std::format_args!(concat!($fmt, "\n")))
    };
    ($fmt:tt, $($arg:tt)*) => {
        $crate::stdio::_eprint(std::format_args!(concat!($fmt, "\n"), $($arg)*))
    };
}

pub(crate) fn _print(fmt: Arguments) -> anyhow::Result<()> {
    print(io::stdout().lock(), fmt, FailureExitCode::StdoutBrokenPipe)
}

pub(crate) fn _eprint(fmt: Arguments) -> anyhow::Result<()> {
    print(io::stderr().lock(), fmt, FailureExitCode::StderrBrokenPipe)
}

fn print(mut writer: impl Write, fmt: Arguments, err: FailureExitCode) -> anyhow::Result<()> {
    writer.write_fmt(fmt).map_err(|e| {
        if e.kind() == io::ErrorKind::BrokenPipe {
            anyhow::Error::new(err)
        } else {
            anyhow::Error::new(e)
        }
    })
}
