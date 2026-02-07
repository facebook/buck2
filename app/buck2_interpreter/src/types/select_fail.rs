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
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::starlark_complex_value;
use starlark::values::Freeze;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::ValueLike;
use starlark::values::starlark_value;

/// Representation of `select_fail()` in Starlark.
#[derive(
    Debug,
    Display,
    ProvidesStaticType,
    NoSerialize,
    Allocative,
    Freeze,
    Trace,
    Coerce
)]
#[repr(C)]
pub struct StarlarkSelectFailGen<V>(V);

impl<'v> StarlarkSelectFail<'v> {
    pub fn new(v: StringValue<'v>) -> Self {
        Self(v.to_value())
    }

    pub fn as_str(&self) -> &str {
        self.0.unpack_str().unwrap()
    }
}

starlark_complex_value!(pub StarlarkSelectFail);

#[starlark_value(type = "SelectFail")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for StarlarkSelectFailGen<V> where
    Self: ProvidesStaticType<'v>
{
}
