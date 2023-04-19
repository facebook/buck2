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

use std::fmt::Arguments;
use std::io;
use std::io::Write;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use superconsole::Line;

use crate::exit_result::FailureExitCode;

static HAS_WRITTEN_TO_STDOUT: AtomicBool = AtomicBool::new(false);

pub fn has_written_to_stdout() -> bool {
    HAS_WRITTEN_TO_STDOUT.load(Ordering::Relaxed)
}

fn stdout() -> io::Stdout {
    HAS_WRITTEN_TO_STDOUT.store(true, Ordering::Relaxed);
    io::stdout()
}

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
    ($fmt:expr) => {
        $crate::stdio::_eprint(std::format_args!(concat!($fmt, "\n")))
    };
    ($fmt:expr, $($arg:tt)*) => {
        $crate::stdio::_eprint(std::format_args!(concat!($fmt, "\n"), $($arg)*))
    };
}

fn map_stdout_error(err: io::Error) -> anyhow::Error {
    if err.kind() == io::ErrorKind::BrokenPipe {
        anyhow::Error::new(FailureExitCode::StdoutBrokenPipe)
    } else {
        anyhow::Error::new(err)
    }
}

fn map_stderr_error(err: io::Error) -> anyhow::Error {
    if err.kind() == io::ErrorKind::BrokenPipe {
        anyhow::Error::new(FailureExitCode::StderrBrokenPipe)
    } else {
        anyhow::Error::new(err)
    }
}

pub fn _print(fmt: Arguments) -> anyhow::Result<()> {
    stdout().lock().write_fmt(fmt).map_err(map_stdout_error)
}

pub fn _eprint(fmt: Arguments) -> anyhow::Result<()> {
    io::stderr().lock().write_fmt(fmt).map_err(map_stderr_error)
}

pub fn print_bytes(bytes: &[u8]) -> anyhow::Result<()> {
    stdout().lock().write_all(bytes).map_err(map_stdout_error)
}

pub fn eprint_line(line: &Line) -> anyhow::Result<()> {
    let line = line.render();
    crate::eprintln!("{}", line)
}

pub fn flush() -> anyhow::Result<()> {
    stdout().flush().map_err(map_stdout_error)
}

pub fn print_with_writer<E, F>(f: F) -> anyhow::Result<()>
where
    E: Into<anyhow::Error>,
    F: FnOnce(&mut dyn Write) -> Result<(), E>,
{
    match f(&mut stdout().lock()) {
        Ok(_) => Ok(()),
        Err(e) => {
            let e: anyhow::Error = e.into();
            match e.downcast_ref::<io::Error>() {
                Some(io_error) if io_error.kind() == io::ErrorKind::BrokenPipe => {
                    Err(anyhow::Error::new(FailureExitCode::StdoutBrokenPipe))
                }
                Some(_) | None => Err(e),
            }
        }
    }
}
