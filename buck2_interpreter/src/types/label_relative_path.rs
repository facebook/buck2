/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::cells::paths::CellPath;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

#[derive(Debug, PartialEq, Display, ProvidesStaticType, NoSerialize)]
pub struct LabelRelativePath(pub CellPath);

starlark_simple_value!(LabelRelativePath);

impl<'v> StarlarkValue<'v> for LabelRelativePath {
    starlark_type!("label_relative_path");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(label_relative_path_methods)
    }
}

#[starlark_module]
fn label_relative_path_methods(builder: &mut MethodsBuilder) {
    fn add(this: &LabelRelativePath, arg: &str) -> anyhow::Result<LabelRelativePath> {
        Ok(LabelRelativePath((*this).0.join_normalized(arg)?))
    }
}
