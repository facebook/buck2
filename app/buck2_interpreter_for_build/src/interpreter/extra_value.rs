/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::OnceCell;

use allocative::Allocative;
use starlark::any::ProvidesStaticType;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::values::any_complex::StarlarkAnyComplex;
use starlark::values::Freeze;
use starlark::values::Freezer;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::Trace;
use starlark::values::ValueLike;

use crate::interpreter::package_file_extra::FrozenPackageFileExtra;
use crate::interpreter::package_file_extra::PackageFileExtra;

#[derive(buck2_error::Error, Debug)]
enum ExtraValueError {
    #[error("Extra value is missing (internal error)")]
    Missing,
    #[error("Extra value had wrong type (internal error)")]
    WrongType,
}

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

    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
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
    pub(crate) fn get(module: &'v Module) -> anyhow::Result<&'v InterpreterExtraValue<'v>> {
        Ok(&module
            .extra_value()
            .ok_or(ExtraValueError::Missing)?
            .downcast_ref::<StarlarkAnyComplex<InterpreterExtraValue>>()
            .ok_or(ExtraValueError::WrongType)?
            .value)
    }
}

impl FrozenInterpreterExtraValue {
    pub(crate) fn get(
        module: &FrozenModule,
    ) -> anyhow::Result<OwnedFrozenValueTyped<StarlarkAnyComplex<FrozenInterpreterExtraValue>>>
    {
        Ok(module
            .owned_extra_value()
            .ok_or(ExtraValueError::Missing)?
            .downcast()
            .map_err(|_| ExtraValueError::WrongType)?)
    }
}
