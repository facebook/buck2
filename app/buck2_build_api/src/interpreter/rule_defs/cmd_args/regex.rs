/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_interpreter::types::regex::BuckStarlarkRegex;
use dupe::Dupe;
use regex::Regex;
use serde::Serialize;
use serde::Serializer;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenStringValue;
use starlark::values::FrozenValueTyped;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::ValueTyped;

/// Regex argument for `cmd_args.replace_regex`.
#[derive(
    StarlarkTypeRepr,
    UnpackValue,
    Debug,
    Clone,
    Dupe,
    Copy,
    Trace,
    Allocative
)]
pub(crate) enum CmdArgsRegex<'v> {
    /// Deprecated.
    // TODO(nga): migrate, soft error, remove.
    Str(StringValue<'v>),
    Regex(ValueTyped<'v, BuckStarlarkRegex>),
}

impl<'v> CmdArgsRegex<'v> {
    pub(crate) fn validate(&self) -> buck2_error::Result<()> {
        match self {
            CmdArgsRegex::Str(pattern) => {
                // Validate that regex is valid
                Regex::new(pattern.as_str())?;
            }
            CmdArgsRegex::Regex(_) => {}
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Dupe, Copy, Allocative)]
pub(crate) enum FrozenCmdArgsRegex {
    Str(FrozenStringValue),
    Regex(FrozenValueTyped<'static, BuckStarlarkRegex>),
}

impl<'v> CmdArgsRegex<'v> {
    pub(crate) fn as_str(&self) -> &str {
        match self {
            Self::Str(s) => s.as_str(),
            Self::Regex(r) => r.as_ref().as_str(),
        }
    }
}

impl<'v> Freeze for CmdArgsRegex<'v> {
    type Frozen = FrozenCmdArgsRegex;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        Ok(match self {
            Self::Str(s) => FrozenCmdArgsRegex::Str(s.freeze(freezer)?),
            Self::Regex(r) => FrozenCmdArgsRegex::Regex(
                match FrozenValueTyped::new_err(r.to_value().freeze(freezer)?) {
                    Ok(r) => r,
                    Err(e) => return Err(FreezeError::new(e.to_string())),
                },
            ),
        })
    }
}

impl<'v> Serialize for CmdArgsRegex<'v> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Str(s) => serializer.collect_str(s),
            Self::Regex(r) => serializer.collect_str(r.as_ref().as_str()),
        }
    }
}

impl Serialize for FrozenCmdArgsRegex {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Self::Str(s) => serializer.collect_str(s),
            Self::Regex(r) => serializer.collect_str(r.as_ref().as_str()),
        }
    }
}
