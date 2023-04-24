/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;

use buck2_node::visibility::VisibilitySpecification;
use buck2_node::visibility::WithinViewSpecification;
use starlark::values::OwnedFrozenValue;
use starlark_map::small_map::SmallMap;

use crate::super_package::data::SuperPackage;

#[derive(Debug, Default)]
pub(crate) struct PackageFileVisibilityFields {
    pub(crate) visibility: VisibilitySpecification,
    pub(crate) within_view: WithinViewSpecification,
    pub(crate) inherit: bool,
}

#[derive(Debug)]
pub(crate) struct PackageFileEvalCtx {
    /// Parent file context.
    /// When evaluating root `PACKAGE` file, parent is still defined.
    pub(crate) parent: SuperPackage,
    pub(crate) visibility: RefCell<Option<PackageFileVisibilityFields>>,
}

impl PackageFileEvalCtx {
    pub(crate) fn build_super_package(
        self,
        package_values: SmallMap<String, OwnedFrozenValue>,
    ) -> SuperPackage {
        let mut merged_package_values = self.parent.package_values().clone();
        merged_package_values.extend(package_values);

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
