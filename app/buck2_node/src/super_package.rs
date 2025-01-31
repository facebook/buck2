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

use crate::cfg_constructor::CfgConstructorImpl;
use crate::metadata::key::MetadataKeyRef;
use crate::metadata::super_package_values::SuperPackageValues;
use crate::modifiers::PackageCfgModifiersValue;
use crate::modifiers::MODIFIER_METADATA_KEY;
use crate::visibility::VisibilitySpecification;
use crate::visibility::WithinViewSpecification;

#[derive(Debug, Allocative)]
pub(crate) struct SuperPackageData {
    package_values: Arc<dyn SuperPackageValues>,
    visibility: VisibilitySpecification,
    within_view: WithinViewSpecification,
    /// Set only for the repo root package.
    cfg_constructor: Option<Arc<dyn CfgConstructorImpl>>,
    // we have package cfg modifiers in package_values as a starlark value
    // we store it here separately as rust value because we are going to use it
    // in a special attribute and cfg constractor
    cfg_modifiers: Option<PackageCfgModifiersValue>,
}

impl SuperPackageData {
    pub fn new(
        package_values: Arc<dyn SuperPackageValues>,
        visibility: VisibilitySpecification,
        within_view: WithinViewSpecification,
        cfg_constructor: Option<Arc<dyn CfgConstructorImpl>>,
    ) -> buck2_error::Result<SuperPackageData> {
        let modifier_key =
            MetadataKeyRef::new(MODIFIER_METADATA_KEY).map_err(buck2_error::Error::from)?;
        let cfg_modifiers = package_values
            .get_package_value_json(modifier_key)?
            .map(PackageCfgModifiersValue::new);
        Ok(SuperPackageData {
            package_values,
            visibility,
            within_view,
            cfg_constructor,
            cfg_modifiers,
        })
    }
}

/// Contents of a `PACKAGE` file merged with contents of containing `PACKAGE` files.
/// This object exists even for non-existent `PACKAGE` files.
#[derive(Debug, Allocative, Clone, Dupe)]
pub struct SuperPackage(Arc<SuperPackageData>);

impl SuperPackage {
    pub fn new(
        package_values: Arc<dyn SuperPackageValues>,
        visibility: VisibilitySpecification,
        within_view: WithinViewSpecification,
        cfg_constructor: Option<Arc<dyn CfgConstructorImpl>>,
    ) -> buck2_error::Result<SuperPackage> {
        Ok(SuperPackage(Arc::new(SuperPackageData::new(
            package_values,
            visibility,
            within_view,
            cfg_constructor,
        )?)))
    }

    pub fn empty<T: SuperPackageValues + Default>() -> buck2_error::Result<SuperPackage> {
        SuperPackage::new(
            Arc::new(T::default()),
            VisibilitySpecification::default(),
            WithinViewSpecification::default(),
            None,
        )
    }

    pub fn package_values(&self) -> &Arc<dyn SuperPackageValues> {
        &self.0.package_values
    }

    pub fn visibility(&self) -> &VisibilitySpecification {
        &self.0.visibility
    }

    pub fn within_view(&self) -> &WithinViewSpecification {
        &self.0.within_view
    }

    pub fn cfg_constructor(&self) -> Option<&Arc<dyn CfgConstructorImpl>> {
        self.0.cfg_constructor.as_ref()
    }

    pub fn cfg_modifiers(&self) -> Option<&PackageCfgModifiersValue> {
        self.0.cfg_modifiers.as_ref()
    }
}

impl PartialEq for SuperPackage {
    fn eq(&self, other: &Self) -> bool {
        let SuperPackageData {
            package_values: this_values,
            visibility: this_visibility,
            within_view: this_within_view,
            cfg_constructor: this_cfg_constructor,
            cfg_modifiers: _, // cfg_modifiers are already contained in package_values
        } = &*self.0;
        let SuperPackageData {
            package_values: other_values,
            visibility: other_visibility,
            within_view: other_within_view,
            cfg_constructor: other_cfg_constructor,
            cfg_modifiers: _, // cfg_modifiers are already contained in package_values
        } = &*other.0;
        (this_visibility, this_within_view) == (other_visibility, other_within_view) && {
            // If either package values are not empty, we cannot compare them
            // because we cannot reliably compare arbitrary Starlark values.
            // So if either package values are not empty, we consider super package not equal.
            this_values.is_empty() && other_values.is_empty()
                &&
                // Same logic for cfg constructors.
                this_cfg_constructor.is_none() && other_cfg_constructor.is_none()
        }
    }
}
