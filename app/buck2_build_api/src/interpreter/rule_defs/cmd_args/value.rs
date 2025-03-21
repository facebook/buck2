/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::convert::Infallible;
use std::fmt::Display;

use allocative::Allocative;
use dupe::Dupe;
use serde::Serializer;
use starlark::__derive_refs::serde::Serialize;
use starlark::coerce::Coerce;
use starlark::typing::Ty;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueTyped;

use crate::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::StarlarkCmdArgs;

fn serialize_as_display<V: Display, S>(v: &V, s: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    s.collect_str(v)
}

/// A tiny wrapper around `Value`/`FrozenValue` that proxies `CommandLineArgLike` calls.
///
/// This should be unnecessary, however I'm not smart enough to figure out how to get
/// things to live long enough, in `ValueAsCommandLineArgLike`, so I'm moving on with my life
/// for now. All values contained in here are guaranteed to implement `CommandLineArgLike`.
#[derive(
    Debug,
    Clone,
    Copy,
    Dupe,
    Trace,
    derive_more::Display,
    Serialize,
    Allocative
)]
#[serde(transparent)]
#[repr(transparent)]
pub struct CommandLineArg<'v>(#[serde(serialize_with = "serialize_as_display")] Value<'v>);

impl<'v> Freeze for CommandLineArg<'v> {
    type Frozen = FrozenCommandLineArg;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<FrozenCommandLineArg> {
        Ok(FrozenCommandLineArg(self.0.freeze(freezer)?))
    }
}

impl<'v> StarlarkTypeRepr for CommandLineArg<'v> {
    type Canonical = <ValueAsCommandLineLike<'v> as StarlarkTypeRepr>::Canonical;

    fn starlark_type_repr() -> Ty {
        ValueAsCommandLineLike::starlark_type_repr()
    }
}

impl<'v> UnpackValue<'v> for CommandLineArg<'v> {
    type Error = Infallible;

    fn unpack_value_impl(value: Value<'v>) -> Result<Option<Self>, Self::Error> {
        if ValueAsCommandLineLike::unpack_value_opt(value).is_some() {
            Ok(Some(CommandLineArg(value)))
        } else {
            Ok(None)
        }
    }
}

impl<'v> CommandLineArg<'v> {
    pub fn from_cmd_args(cmd_args: ValueTyped<'v, StarlarkCmdArgs<'v>>) -> Self {
        let _no_check_needed: &dyn CommandLineArgLike = cmd_args.as_ref();
        CommandLineArg(cmd_args.to_value())
    }

    pub fn as_command_line_arg(self) -> &'v dyn CommandLineArgLike {
        ValueAsCommandLineLike::unpack_value_err(self.0)
            .expect("checked type in constructor")
            .0
    }

    pub fn to_value(self) -> Value<'v> {
        self.0
    }
}

#[derive(
    Debug,
    Allocative,
    Eq,
    PartialEq,
    derive_more::Display,
    Clone,
    Copy,
    Dupe
)]
#[repr(transparent)]
pub struct FrozenCommandLineArg(FrozenValue);

unsafe impl<'v> Coerce<CommandLineArg<'v>> for FrozenCommandLineArg {}

impl FrozenCommandLineArg {
    pub fn new(value: FrozenValue) -> buck2_error::Result<FrozenCommandLineArg> {
        ValueAsCommandLineLike::unpack_value_err(value.to_value())?;
        Ok(FrozenCommandLineArg(value))
    }

    pub fn as_command_line_arg<'v>(self) -> &'v dyn CommandLineArgLike {
        CommandLineArg(self.0.to_value()).as_command_line_arg()
    }

    pub fn to_frozen_value(&self) -> FrozenValue {
        self.0
    }

    pub fn slice_from_frozen_value_unchecked(v: &[FrozenValue]) -> &[FrozenCommandLineArg] {
        // SAFETY: `#[repr(transparent)]`
        unsafe { std::slice::from_raw_parts(v.as_ptr() as *const _, v.len()) }
    }
}
