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
use buck2_error::internal_error;
use starlark::StarlarkPagable;
use starlark::StarlarkPagablePanic;
use starlark::any::ProvidesStaticType;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::values::FreezeBranded;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::OwnedFrozen;
use starlark::values::Trace;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;
use starlark::values::any_complex::StarlarkAnyComplex;

use crate::interpreter::package_file_extra::FrozenPackageFileExtra;
use crate::interpreter::package_file_extra::PackageFileExtra;

/// `Module.extra_value` when evaluating build, bzl, package, and bxl files.
#[derive(
    Default,
    Debug,
    ProvidesStaticType,
    Allocative,
    Trace,
    StarlarkPagablePanic
)]
pub(crate) struct InterpreterExtraValue<'v> {
    /// Set when evaluating `PACKAGE` files.
    pub(crate) package_extra: OnceCell<PackageFileExtra<'v>>,
}

#[derive(Debug, ProvidesStaticType, Allocative, StarlarkPagable)]
pub(crate) struct FrozenInterpreterExtraValue<'v> {
    pub(crate) package_extra: Option<FrozenPackageFileExtra<'v>>,
}

starlark::register_starlark_any_complex!(InterpreterExtraValue<'_>, frozen FrozenInterpreterExtraValue<'_>);

impl<'v> FreezeBranded for InterpreterExtraValue<'v> {
    type Frozen<'fv> = FrozenInterpreterExtraValue<'fv>;

    fn freeze<'fv>(self, freezer: &Freezer<'fv>) -> FreezeResult<Self::Frozen<'fv>> {
        Ok(FrozenInterpreterExtraValue {
            package_extra: FreezeBranded::freeze(self.package_extra, freezer)?,
        })
    }
}

impl<'v> InterpreterExtraValue<'v> {
    pub(crate) fn get(module: &Module<'v>) -> buck2_error::Result<&'v InterpreterExtraValue<'v>> {
        Ok(&module
            .extra_value()
            .ok_or_else(|| internal_error!("Extra value is missing"))?
            .downcast_ref::<StarlarkAnyComplex<InterpreterExtraValue>>()
            .ok_or_else(|| internal_error!("Extra value had wrong type"))?
            .value)
    }
}

/// A frozen module's extra value, kept alive by that module's heap.
pub(crate) type OwnedFrozenInterpreterExtraValue =
    OwnedFrozen<ValueTyped<'static, StarlarkAnyComplex<FrozenInterpreterExtraValue<'static>>>>;

impl FrozenInterpreterExtraValue<'_> {
    pub(crate) fn get(
        module: &FrozenModule,
    ) -> buck2_error::Result<OwnedFrozenInterpreterExtraValue> {
        module
            .extra_value_owned()
            .ok_or_else(|| internal_error!("Extra value is missing"))?
            .maybe_map::<ValueTyped<'static, StarlarkAnyComplex<FrozenInterpreterExtraValue>>, _>(
                |v| ValueTyped::new(v),
            )
            .ok_or_else(|| internal_error!("Extra value had wrong type"))
    }
}
