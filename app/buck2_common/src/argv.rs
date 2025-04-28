/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::fmt::Display;
use std::sync::Arc;

use buck2_core::cells::cell_path::CellPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use gazebo::prelude::VecExt;

/// Argv contains the bare process argv and the "expanded" argv. The expanded argv is
/// the argv after processing flagfiles (args like @mode/opt and --flagfile mode/opt)
/// and after possibly replacing `argv[0]` with a more representative value.
#[derive(Clone)]
pub struct Argv {
    pub argv: Vec<String>,
    pub expanded_argv: ExpandedArgv,
}

#[derive(Clone)]
pub struct ExpandedArgv {
    args: Vec<(String, ExpandedArgSource)>,
}

#[derive(Clone)]
pub enum ExpandedArgSource {
    Inline,
    Flagfile(Arc<FlagfileArgSource>),
}

#[derive(Eq, PartialEq)]
pub struct FlagfileArgSource {
    pub kind: ArgFileKind,
    pub parent: Option<Arc<FlagfileArgSource>>,
}

#[derive(Clone, Debug, Eq, PartialEq, derive_more::Display)]
pub enum ArgFilePath {
    Project(CellPath),
    External(AbsNormPathBuf),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ArgFileKind {
    PythonExecutable(ArgFilePath, Option<String>),
    Path(ArgFilePath),
    Stdin,
}

impl Display for ArgFileKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArgFileKind::PythonExecutable(abs_path_buf, Some(flag)) => {
                write!(f, "@{}#{}", abs_path_buf, flag)
            }
            ArgFileKind::PythonExecutable(abs_path_buf, None) => write!(f, "@{}", abs_path_buf),
            ArgFileKind::Path(abs_path_buf) => write!(f, "@{}", abs_path_buf),
            ArgFileKind::Stdin => f.write_str("@-"),
        }
    }
}

impl ExpandedArgv {
    pub fn new() -> Self {
        Self::from_literals(Vec::new())
    }

    pub fn from_literals(args: Vec<String>) -> Self {
        Self {
            args: args.into_map(|v| (v, ExpandedArgSource::Inline)),
        }
    }

    fn redacted(self, to_redact: &HashSet<&String>) -> ExpandedArgv {
        Self {
            args: self
                .args
                .into_iter()
                .filter(|(arg, _)| !to_redact.contains(arg))
                .collect(),
        }
    }

    pub fn args(&self) -> impl Iterator<Item = &str> {
        self.args.iter().map(|(v, _)| v as _)
    }

    pub fn iter(&self) -> impl Iterator<Item = (&str, &ExpandedArgSource)> {
        self.args.iter().map(|(l, r)| (l as _, r))
    }
}

pub struct ExpandedArgvBuilder {
    current_argfile: Option<Arc<FlagfileArgSource>>,
    argv: ExpandedArgv,
}

impl ExpandedArgvBuilder {
    pub fn new() -> Self {
        Self {
            argv: ExpandedArgv::new(),
            current_argfile: None,
        }
    }

    pub fn replace(&mut self, idx: usize, val: String) {
        self.argv.args[idx].0 = val;
    }

    pub fn push(&mut self, next_arg: String) {
        let source = match &self.current_argfile {
            Some(argfile) => ExpandedArgSource::Flagfile(argfile.clone()),
            None => ExpandedArgSource::Inline,
        };
        self.argv.args.push((next_arg, source));
    }

    pub fn build(self) -> ExpandedArgv {
        self.argv
    }

    pub fn argfile_scope<R>(&mut self, kind: ArgFileKind, func: impl FnOnce(&mut Self) -> R) -> R {
        let parent = self.current_argfile.take();
        self.current_argfile = Some(Arc::new(FlagfileArgSource { kind, parent }));
        let res = func(self);
        let current = self.current_argfile.take().unwrap();
        self.current_argfile = current.parent.clone();
        res
    }
}

/// The "sanitized" argv is the argv and expanded argv after stripping some possibly sensitive
/// arguments. What's considered sensitive is command-specific and usually determined by an implementation
/// of `StreamingCommand::sanitize_argv`.
///
/// For example, for the run command this will strip out the arguments passed to the executed command (i.e. those after `--`).
#[derive(Clone)]
#[allow(clippy::manual_non_exhaustive)] // #[non_exhaustive] would allow this crate to create these.
pub struct SanitizedArgv {
    pub argv: Vec<String>,
    pub expanded_argv: ExpandedArgv,
    _priv: (), // Ensure that all ways of creating this are in this file.
}

impl Argv {
    pub fn no_need_to_sanitize(self) -> SanitizedArgv {
        let Argv {
            argv,
            expanded_argv,
        } = self;
        SanitizedArgv {
            argv,
            expanded_argv,
            _priv: (),
        }
    }

    pub fn redacted(self, to_redact: HashSet<&String>) -> SanitizedArgv {
        SanitizedArgv {
            argv: self
                .argv
                .into_iter()
                .filter(|arg| !to_redact.contains(arg))
                .collect(),
            expanded_argv: self.expanded_argv.redacted(&to_redact),
            _priv: (),
        }
    }
}
