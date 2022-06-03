/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::cells::{
    paths::{CellPath, CellRelativePathBuf},
    CellName,
};
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    values::{NoSerialize, StarlarkValue},
};

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
    starlark_type!("test_cwd");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(test_root_methods)
    }
}

#[starlark_module]
fn test_root_methods(builder: &mut MethodsBuilder) {}
