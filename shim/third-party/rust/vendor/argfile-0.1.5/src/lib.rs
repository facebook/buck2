//! # argfile
//!
//! > Load additional CLI args from file
//!
//! Prior art:
//! - [javac argument files](https://docs.oracle.com/javase/7/docs/technotes/tools/windows/javac.html#commandlineargfile)
//! - [Microsoft response files](https://docs.microsoft.com/en-us/cpp/build/reference/at-specify-a-compiler-response-file?view=msvc-170)
//! - [Python fromfile](https://docs.python.org/3/library/argparse.html#fromfile-prefix-chars)
//!
//! This is meant to work with any CLI parser, like [clap](https://docs.rs/clap), by pre-processing the
//! arguments, like [wild](https://docs.rs/wild).
//!
//! ## Examples
//!
//! ```rust,no_run
//! argfile::expand_args(
//!     argfile::parse_fromfile,
//!     argfile::PREFIX,
//! ).unwrap();
//! ```
//!
//! To integrate this with [wild](https://docs.rs/wild) and [clap](https://docs.rs/clap)
//! ```rust,no_run
//! let args = wild::args_os();
//! let args = argfile::expand_args_from(
//!     args,
//!     argfile::parse_fromfile,
//!     argfile::PREFIX,
//! ).unwrap();
//! let matches = clap::Command::new("your_app")
//!     .get_matches_from(args);
//! ```

#![cfg_attr(docsrs, feature(doc_auto_cfg))]

mod argument;
mod fromfile;
#[cfg(feature = "response")]
mod response;

pub use argument::*;
pub use fromfile::*;
#[cfg(feature = "response")]
pub use response::*;

/// Conventional prefix to mark argfiles
pub const PREFIX: char = '@';

/// Load prefixed-args from specified files
///
/// ## Examples
///
/// ```rust,no_run
/// argfile::expand_args(
///     argfile::parse_fromfile,
///     argfile::PREFIX,
/// ).unwrap();
/// ```
pub fn expand_args<F>(parser: F, prefix: char) -> Result<Vec<std::ffi::OsString>, std::io::Error>
where
    F: Fn(&str, char) -> Vec<crate::Argument>,
{
    expand_args_from(std::env::args_os(), parser, prefix)
}

/// Load prefixed-args from specified files
///
/// ## Examples
///
/// ```rust,no_run
/// argfile::expand_args_from(
///     std::env::args_os(),
///     argfile::parse_fromfile,
///     argfile::PREFIX,
/// ).unwrap();
/// ```
pub fn expand_args_from<F>(
    args: impl Iterator<Item = std::ffi::OsString>,
    parser: F,
    prefix: char,
) -> Result<Vec<std::ffi::OsString>, std::io::Error>
where
    F: Fn(&str, char) -> Vec<crate::Argument>,
{
    use std::collections::VecDeque;

    let mut expanded_args = Vec::with_capacity(args.size_hint().0);

    let mut todo: VecDeque<_> = args.map(|a| Argument::parse(a, prefix)).collect();
    while let Some(next) = todo.pop_front() {
        match next {
            Argument::PassThrough(arg) => expanded_args.push(arg),
            Argument::Path(path) => {
                let content = std::fs::read_to_string(path)?;
                let new_args = parser(&content, prefix);
                todo.reserve(new_args.len());
                for (i, arg) in new_args.into_iter().enumerate() {
                    todo.insert(i, arg);
                }
            }
        }
    }

    Ok(expanded_args)
}
