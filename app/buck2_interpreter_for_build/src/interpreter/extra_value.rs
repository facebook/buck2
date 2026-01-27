/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cell::OnceCell;

use allocative::Allocative;
use buck2_error::BuckErrorContext;
use starlark::any::ProvidesStaticType;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::values::Freeze;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::Trace;
use starlark::values::ValueLike;
use starlark::values::any_complex::StarlarkAnyComplex;

use crate::interpreter::package_file_extra::FrozenPackageFileExtra;
use crate::interpreter::package_file_extra::PackageFileExtra;

/// `Module.extra_value` when evaluating build, bzl, package, and bxl files.
#[derive(Default, Debug, ProvidesStaticType, Allocative, Trace)]
pub(crate) struct InterpreterExtraValue<'v> {
    /// Set when evaluating `PACKAGE` files.
    pub(crate) package_extra: OnceCell<PackageFileExtra<'v>>,
}

#[derive(Debug, ProvidesStaticType, Allocative)]
pub(crate) struct FrozenInterpreterExtraValue {
    pub(crate) package_extra: Option<FrozenPackageFileExtra>,
}

impl<'v> Freeze for InterpreterExtraValue<'v> {
    type Frozen = FrozenInterpreterExtraValue;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        Ok(FrozenInterpreterExtraValue {
            package_extra: self
                .package_extra
                .into_inner()
                .map(|p| p.freeze(freezer))
                .transpose()?,
        })
    }
}

impl<'v> InterpreterExtraValue<'v> {
    pub(crate) fn get(module: &Module<'v>) -> buck2_error::Result<&'v InterpreterExtraValue<'v>> {
        Ok(&module
            .extra_value()
            .internal_error("Extra value is missing")?
            .downcast_ref::<StarlarkAnyComplex<InterpreterExtraValue>>()
            .internal_error("Extra value had wrong type")?
            .value)
    }
}

impl FrozenInterpreterExtraValue {
    pub(crate) fn get(
        module: &FrozenModule,
    ) -> buck2_error::Result<OwnedFrozenValueTyped<StarlarkAnyComplex<FrozenInterpreterExtraValue>>>
    {
        module
            .owned_extra_value()
            .internal_error("Extra value is missing")?
            .downcast()
            .ok()
            .internal_error("Extra value had wrong type")
    }
}
