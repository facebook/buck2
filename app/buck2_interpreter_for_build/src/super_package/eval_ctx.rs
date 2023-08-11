/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;

use buck2_core::metadata_key::MetadataKey;
use buck2_interpreter::paths::package::PackageFilePath;
use buck2_node::super_package::SuperPackage;
use buck2_node::visibility::VisibilitySpecification;
use buck2_node::visibility::WithinViewSpecification;
use starlark_map::small_map::SmallMap;

#[derive(Debug, Default)]
pub(crate) struct PackageFileVisibilityFields {
    pub(crate) visibility: VisibilitySpecification,
    pub(crate) within_view: WithinViewSpecification,
    pub(crate) inherit: bool,
}

#[derive(Debug)]
pub(crate) struct PackageFileEvalCtx {
    pub(crate) path: PackageFilePath,
    /// Parent file context.
    /// When evaluating root `PACKAGE` file, parent is still defined.
    pub(crate) parent: SuperPackage,
    /// Package values set in this file. Does not include values from parent files.
    pub(crate) package_values: RefCell<SmallMap<MetadataKey, serde_json::Value>>,
    pub(crate) visibility: RefCell<Option<PackageFileVisibilityFields>>,
}

impl PackageFileEvalCtx {
    pub(crate) fn build_super_package(self) -> SuperPackage {
        let mut merged_package_values = self.parent.package_values().clone();
        merged_package_values.extend(self.package_values.into_inner());

        let PackageFileVisibilityFields {
            visibility,
            within_view,
            inherit,
        } = self.visibility.into_inner().unwrap_or_default();

        let (visibility, within_view) = if inherit {
            (
                self.parent.visibility().extend_with(&visibility),
                self.parent.within_view().extend_with(&within_view),
            )
        } else {
            (visibility, within_view)
        };

        SuperPackage::new(merged_package_values, visibility, within_view)
    }
}
