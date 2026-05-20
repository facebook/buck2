/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cell::RefCell;
use std::sync::Arc;

use buck2_interpreter::paths::package::PackageFilePath;
use buck2_node::cfg_constructor::CfgConstructorImpl;
use buck2_node::super_package::SuperPackage;
use buck2_node::visibility::VisibilityPatternList;
use buck2_node::visibility::VisibilitySpecification;
use buck2_node::visibility::WithinViewSpecification;
use dupe::Dupe;
use starlark::values::OwnedFrozenValue;
use starlark_map::small_map::SmallMap;

use crate::interpreter::package_file_extra::MAKE_CFG_CONSTRUCTOR;
use crate::interpreter::package_file_extra::OwnedFrozenPackageFileExtra;
use crate::super_package::package_value::OwnedFrozenStarlarkPackageValue;
use crate::super_package::package_value::SuperPackageValuesImpl;

#[derive(Debug, Default)]
pub(crate) struct PackageFileVisibilityFields {
    pub(crate) visibility: VisibilitySpecification,
    pub(crate) within_view: WithinViewSpecification,
    pub(crate) inherit: bool,
    /// `true` iff the user passed a non-`None` `visibility=` list. Omitted
    /// or `visibility=None` are both `false` and contribute nothing to the
    /// cap (unlike `visibility=[]`, which is a non-None empty list).
    pub(crate) visibility_was_set: bool,
}

#[derive(Debug)]
pub struct PackageFileEvalCtx {
    pub path: PackageFilePath,
    /// Parent file context.
    /// When evaluating root `PACKAGE` file, parent is still defined.
    pub(crate) parent: SuperPackage,
    pub(crate) visibility: RefCell<Option<PackageFileVisibilityFields>>,
    pub(crate) test_config_unification_rollout: RefCell<Option<bool>>,
    /// `true` iff this PACKAGE called `enforce_visibility_intersection()`.
    pub(crate) enforces_visibility_intersection: RefCell<bool>,
}

impl PackageFileEvalCtx {
    fn cfg_constructor(
        extra: Option<&OwnedFrozenPackageFileExtra>,
    ) -> buck2_error::Result<Option<Arc<dyn CfgConstructorImpl>>> {
        let Some(extra) = extra else {
            return Ok(None);
        };
        let package_extra = extra.package_extra();
        let Some(cfg_constructor) = package_extra.cfg_constructor else {
            return Ok(None);
        };
        let cfg_constructor = unsafe {
            // SAFETY: field belongs to the same heap.
            OwnedFrozenValue::new(extra.owner().dupe(), cfg_constructor)
        };
        let make_cfg_constructor = MAKE_CFG_CONSTRUCTOR.get()?;
        Ok(Some(make_cfg_constructor(cfg_constructor)?))
    }

    pub(crate) fn build_super_package(
        self,
        extra: Option<OwnedFrozenPackageFileExtra>,
    ) -> buck2_error::Result<SuperPackage> {
        let cfg_constructor = Self::cfg_constructor(extra.as_ref())?;

        let package_values = match &extra {
            None => SmallMap::new(),
            Some(extra) => {
                let package_extra = extra.package_extra();
                let mut values = SmallMap::with_capacity(package_extra.package_values.len());
                for (name, value) in &package_extra.package_values {
                    let value = unsafe {
                        // SAFETY: using the same heap.
                        OwnedFrozenStarlarkPackageValue::new(extra.owner().dupe(), *value)
                    };
                    values.insert(name.clone(), value);
                }
                values
            }
        };

        let merged_package_values =
            SuperPackageValuesImpl::merge(self.parent.package_values(), package_values)?;

        let visibility_fields = self.visibility.into_inner();

        // Captured before `inherit=True` is applied. `None` when omitted —
        // omitted must NOT contribute an empty list to the cap.
        let explicit_visibility: Option<VisibilityPatternList> = visibility_fields
            .as_ref()
            .filter(|f| f.visibility_was_set)
            .map(|f| f.visibility.0.dupe());

        let (visibility, within_view) = match visibility_fields {
            Some(package_visibility) => {
                if package_visibility.inherit {
                    (
                        self.parent
                            .visibility()
                            .extend_with(&package_visibility.visibility)?,
                        self.parent
                            .within_view()
                            .extend_with(&package_visibility.within_view)?,
                    )
                } else {
                    (
                        package_visibility.visibility,
                        package_visibility.within_view,
                    )
                }
            }
            None => {
                // If the package file does not specify any visibility, default to the parent visibility.
                (
                    self.parent.visibility().to_owned(),
                    self.parent.within_view().to_owned(),
                )
            }
        };

        let test_config_unification_rollout =
            match self.test_config_unification_rollout.into_inner() {
                Some(test_config_unification_rollout) => test_config_unification_rollout,
                None => self.parent.test_config_unification_rollout(),
            };

        // Extend the inherited cap with this PACKAGE's contribution
        // (no-op if it didn't opt in or didn't pass `package(visibility=...)`).
        let visibility_cap = match (
            self.enforces_visibility_intersection.into_inner(),
            explicit_visibility,
        ) {
            (true, Some(raw)) => self.parent.visibility_cap().intersect_with(&raw),
            _ => self.parent.visibility_cap().dupe(),
        };

        SuperPackage::new(
            merged_package_values,
            visibility,
            within_view,
            visibility_cap,
            cfg_constructor,
            test_config_unification_rollout,
        )
    }
}
