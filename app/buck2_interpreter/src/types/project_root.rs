/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::values::starlark_value;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

#[derive(Debug, PartialEq, Display, ProvidesStaticType, NoSerialize, Allocative)]
pub struct ProjectRoot;

impl ProjectRoot {
    pub fn new() -> Self {
        Self
    }
}

starlark_simple_value!(ProjectRoot);

#[starlark_value(type = "project_root")]
impl<'v> StarlarkValue<'v> for ProjectRoot {}
