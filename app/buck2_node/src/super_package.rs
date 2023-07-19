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
use starlark_map::small_map::SmallMap;

use crate::visibility::VisibilitySpecification;
use crate::visibility::WithinViewSpecification;

#[derive(Default, Debug, Allocative)]
pub(crate) struct SuperPackageData {
    package_values: SmallMap<String, serde_json::Value>,
    visibility: VisibilitySpecification,
    within_view: WithinViewSpecification,
}

/// Contents of a `PACKAGE` file merged with contents of containing `PACKAGE` files.
/// This object exists even for non-existent `PACKAGE` files.
#[derive(Default, Debug, Allocative, Clone, Dupe)]
pub struct SuperPackage(Arc<SuperPackageData>);

impl SuperPackage {
    pub fn new(
        package_values: SmallMap<String, serde_json::Value>,
        visibility: VisibilitySpecification,
        within_view: WithinViewSpecification,
    ) -> SuperPackage {
        SuperPackage(Arc::new(SuperPackageData {
            package_values,
            visibility,
            within_view,
        }))
    }

    pub fn package_values(&self) -> &SmallMap<String, serde_json::Value> {
        &self.0.package_values
    }

    pub fn visibility(&self) -> &VisibilitySpecification {
        &self.0.visibility
    }

    pub fn within_view(&self) -> &WithinViewSpecification {
        &self.0.within_view
    }
}

impl PartialEq for SuperPackage {
    fn eq(&self, other: &Self) -> bool {
        let SuperPackageData {
            package_values: this_values,
            visibility: this_visibility,
            within_view: this_within_view,
        } = &*self.0;
        let SuperPackageData {
            package_values: other_values,
            visibility: other_visibility,
            within_view: other_within_view,
        } = &*other.0;
        (this_visibility, this_within_view) == (other_visibility, other_within_view) && {
            // If either package values are not empty, we cannot compare them
            // because we cannot reliably compare arbitrary Starlark values.
            // So if either package values are not empty, we consider super package not equal.
            this_values.is_empty() && other_values.is_empty()
        }
    }
}
