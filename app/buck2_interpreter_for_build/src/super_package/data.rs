/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use starlark::values::OwnedFrozenValue;
use starlark_map::small_map::SmallMap;

#[derive(Default, Debug, Allocative)]
pub(crate) struct SuperPackageData {
    package_values: SmallMap<String, OwnedFrozenValue>,
}

/// Contents of a `PACKAGE` file merged with contents of containing `PACKAGE` files.
/// This object exists even for non-existent `PACKAGE` files.
#[derive(Default, Debug, Allocative, Clone, Dupe)]
pub(crate) struct SuperPackage(Arc<SuperPackageData>);

impl SuperPackage {
    pub(crate) fn new(package_values: SmallMap<String, OwnedFrozenValue>) -> SuperPackage {
        SuperPackage(Arc::new(SuperPackageData { package_values }))
    }

    pub(crate) fn package_values(&self) -> &SmallMap<String, OwnedFrozenValue> {
        &self.0.package_values
    }
}

impl PartialEq for SuperPackage {
    fn eq(&self, other: &Self) -> bool {
        let SuperPackageData {
            package_values: this_values,
        } = &*self.0;
        let SuperPackageData {
            package_values: other_values,
        } = &*other.0;
        // If either package values are not empty, we cannot compare them
        // because we cannot reliably compare arbitrary Starlark values.
        // So if either package values are not empty, we consider super package not equal.
        this_values.is_empty() && other_values.is_empty()
    }
}
