/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;

#[derive(Clone)]
pub struct Argv {
    pub argv: Vec<String>,
    pub expanded_argv: ExpandedArgv,
}

#[derive(Clone)]
pub struct ExpandedArgv {
    args: Vec<String>,
}

impl ExpandedArgv {
    pub fn from_literals(args: Vec<String>) -> Self {
        Self { args }
    }

    fn redacted(self, to_redact: &HashSet<&String>) -> ExpandedArgv {
        Self {
            args: self
                .args
                .into_iter()
                .filter(|arg| !to_redact.contains(arg))
                .collect(),
        }
    }

    pub fn args(&self) -> impl Iterator<Item = &str> {
        self.args.iter().map(|v| v as _)
    }
}

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
