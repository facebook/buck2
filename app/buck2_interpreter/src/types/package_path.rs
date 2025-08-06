/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::hash::Hash;

use allocative::Allocative;
use buck2_core::package::PackageLabel;
use derive_more::Display;
use derive_more::From;
use dupe::Dupe;
use serde::Serialize;
use starlark::any::ProvidesStaticType;
use starlark::collections::StarlarkHasher;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueError;
use starlark::values::ValueLike;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;

#[derive(
    Clone,
    Dupe,
    Debug,
    Hash,
    Display,
    PartialEq,
    Eq,
    From,
    ProvidesStaticType,
    Serialize,
    Allocative
)]
#[serde(transparent)]
pub struct StarlarkPackagePath {
    pkg: PackageLabel,
}

starlark_simple_value!(StarlarkPackagePath);

impl StarlarkPackagePath {
    pub fn new(pkg: PackageLabel) -> Self {
        StarlarkPackagePath { pkg }
    }
}

#[starlark_value(type = "PackagePath")]
impl<'v> StarlarkValue<'v> for StarlarkPackagePath {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(package_path_methods)
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> starlark::Result<()> {
        self.hash(hasher);
        Ok(())
    }

    fn equals(&self, other: Value<'v>) -> starlark::Result<bool> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.pkg == other.pkg)
        } else {
            Ok(false)
        }
    }

    fn compare(&self, other: Value<'v>) -> starlark::Result<std::cmp::Ordering> {
        if let Some(other) = other.downcast_ref::<Self>() {
            Ok(self.pkg.cmp(&other.pkg))
        } else {
            ValueError::unsupported_with(self, "compare", other)
        }
    }
}

/// A PackagePath represents a Buck package like package `root//foo/bar`
#[starlark_module]
fn package_path_methods(builder: &mut MethodsBuilder) {
    /// Returns the name of the cell of the package
    ///
    /// # Examples
    ///
    /// ```python
    /// # For a package (root//foo/bar):
    /// package_path.cell  # Returns: "root"
    /// ```
    #[starlark(attribute)]
    fn cell<'v>(this: &'v StarlarkPackagePath) -> starlark::Result<&'v str> {
        Ok(this.pkg.cell_name().as_str())
    }
}

#[starlark_module]
pub fn register_package_path(globals: &mut GlobalsBuilder) {
    const PackagePath: StarlarkValueAsType<StarlarkPackagePath> = StarlarkValueAsType::new();
}
