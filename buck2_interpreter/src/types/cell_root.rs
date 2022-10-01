/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::cells::CellName;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

#[derive(Debug, PartialEq, Display, ProvidesStaticType, NoSerialize)]
pub struct CellRoot(CellPath);

impl CellRoot {
    pub fn new(name: CellName) -> Self {
        Self(CellPath::new(
            name,
            CellRelativePathBuf::unchecked_new("".to_owned()),
        ))
    }

    pub fn cell_path(&self) -> &CellPath {
        &self.0
    }
}

starlark_simple_value!(CellRoot);

impl<'v> StarlarkValue<'v> for CellRoot {
    starlark_type!("cell_root");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(cell_root_methods)
    }
}

#[starlark_module]
fn cell_root_methods(builder: &mut MethodsBuilder) {}
