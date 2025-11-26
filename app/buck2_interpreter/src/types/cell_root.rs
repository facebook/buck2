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
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePathBuf;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;

#[derive(Debug, PartialEq, Display, ProvidesStaticType, NoSerialize, Allocative)]
pub struct CellRoot(CellPath);

impl CellRoot {
    pub fn new(name: CellName) -> Self {
        Self(CellPath::new(
            name,
            CellRelativePathBuf::unchecked_new("".to_owned()),
        ))
    }

    pub fn cell_path(&self) -> CellPathRef<'_> {
        self.0.as_ref()
    }
}

starlark_simple_value!(CellRoot);

#[starlark_value(type = "CellRoot")]
impl<'v> StarlarkValue<'v> for CellRoot {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(cell_root_methods)
    }
}

/// A `CellRoot` represents the root directory of a cell in Buck2.
///
/// `CellRoot` is typically accessed via the `cell_root` attribute on labels (e.g., `ctx.label.cell_root`).
/// For example, given a label `root//foo:bar`, the `cell_root` would represent `root//`.
///
/// This type is commonly used with [`cmd_args.relative_to()`](../cmd_args/#cmd_argsrelative_to) to make artifact paths relative to the cell root.
#[starlark_module]
fn cell_root_methods(_builder: &mut MethodsBuilder) {}

#[starlark_module]
pub fn register_cell_root(globals: &mut GlobalsBuilder) {
    const CellRoot: StarlarkValueAsType<CellRoot> = StarlarkValueAsType::new();
}
