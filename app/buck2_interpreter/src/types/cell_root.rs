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
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePathBuf;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;

#[derive(Debug, PartialEq, Display, ProvidesStaticType, NoSerialize, Allocative)]
pub struct CellRoot(CellPath);

impl CellRoot {
    pub fn new(name: CellName) -> Self {
        Self(CellPath::new(
            name,
            CellRelativePathBuf::unchecked_new("".to_owned()),
        ))
    }

    pub fn cell_path(&self) -> CellPathRef {
        self.0.as_ref()
    }
}

starlark_simple_value!(CellRoot);

impl<'v> StarlarkValue<'v> for CellRoot {
    starlark_type!("cell_root");
}
