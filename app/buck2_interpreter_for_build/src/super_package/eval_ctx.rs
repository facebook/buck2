/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::super_package::data::SuperPackage;

#[derive(Debug)]
pub(crate) struct PackageFileEvalCtx {
    /// Parent file context.
    /// When evaluating root `PACKAGE` file, parent is still defined.
    pub(crate) parent: SuperPackage,
}

impl PackageFileEvalCtx {
    pub(crate) fn build_super_package(self) -> SuperPackage {
        let _ignore = self.parent;
        SuperPackage::default()
    }
}
