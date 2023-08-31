/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::cells::cell_path::CellPath;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

#[derive(
    Debug,
    PartialEq,
    Display,
    ProvidesStaticType,
    NoSerialize,
    Allocative,
    StarlarkDocs
)]
pub struct StarlarkCellPath(pub CellPath);

starlark_simple_value!(StarlarkCellPath);

#[starlark_value(type = "label_relative_path")]
impl<'v> StarlarkValue<'v> for StarlarkCellPath {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(cell_path_methods)
    }
}

#[starlark_module]
fn cell_path_methods(builder: &mut MethodsBuilder) {
    fn add(this: &StarlarkCellPath, arg: &str) -> anyhow::Result<StarlarkCellPath> {
        Ok(StarlarkCellPath((this).0.join_normalized(arg)?))
    }
}

#[starlark_module]
pub fn register_cell_path(globals: &mut GlobalsBuilder) {
    const CellPath: StarlarkValueAsType<StarlarkCellPath> = StarlarkValueAsType::new();
}
