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

#[derive(Default, Debug, Allocative)]
struct SuperPackageData {}

/// Contents of a `PACKAGE` file merged with contents of containing `PACKAGE` files.
/// This object exists even for non-existent `PACKAGE` files.
#[derive(Default, Debug, Allocative, Clone, Dupe)]
pub(crate) struct SuperPackage(Arc<SuperPackageData>);

impl PartialEq for SuperPackage {
    fn eq(&self, other: &Self) -> bool {
        let SuperPackageData {} = &*self.0;
        let SuperPackageData {} = &*other.0;
        // It won't be `Eq` later when package-local Starlark values are added.
        true
    }
}
