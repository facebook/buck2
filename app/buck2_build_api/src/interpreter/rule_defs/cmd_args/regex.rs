/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_interpreter::types::regex::StarlarkBuckRegex;
use dupe::Dupe;
use regex::Regex;
use serde::Serialize;
use serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::values::FreezeBranded;
use starlark::values::StarlarkPagable;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::ValueTyped;
use starlark::values::type_repr::StarlarkTypeRepr;

/// Regex argument for `cmd_args.replace_regex`.
#[derive(
    StarlarkTypeRepr,
    UnpackValue,
    Debug,
    Clone,
    Dupe,
    Copy,
    Trace,
    FreezeBranded,
    ProvidesStaticType,
    Allocative,
    StarlarkPagable
)]
pub(crate) enum CmdArgsRegex<'v> {
    /// Deprecated.
    // TODO(nga): migrate, soft error, remove.
    Str(StringValue<'v>),
    Regex(ValueTyped<'v, StarlarkBuckRegex>),
}

pub(crate) type FrozenCmdArgsRegex = CmdArgsRegex<'static>;

// Interop for containers whose `Freeze` impls have not been migrated to
// `FreezeBranded`; see `freeze_via_branded`.
impl<'v> starlark::values::Freeze for CmdArgsRegex<'v> {
    type Frozen = FrozenCmdArgsRegex;

    fn freeze(
        self,
        freezer: &starlark::values::Freezer,
    ) -> starlark::values::FreezeResult<Self::Frozen> {
        starlark::values::freeze_via_branded(self, freezer)
    }
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

    pub(crate) fn as_str(&self) -> &str {
        match self {
            Self::Str(s) => s.as_str(),
            Self::Regex(r) => r.as_ref().as_str(),
        }
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
