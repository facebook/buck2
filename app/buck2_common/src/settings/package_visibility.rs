/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use pagable::Pagable;
use serde::Deserialize;
use serde::Serialize;

use crate::settings::metadata::SectionMetadata;
use crate::settings::metadata::SettingKey;
use crate::settings::metadata::SettingKeyMetadata;

/// Controls whether ordinary `package(visibility=...)` declarations
/// contribute to the visibility intersection by default.
///
/// * `off` (default): legacy behavior; only `enforce_visibility_intersection()`
///   boundaries contribute.
/// * `audit`: the default intersection is shadow-evaluated; would-block edges
///   are reported, not fatal.
/// * `enforce`: ordinary PACKAGE visibility is enforced by default; the marker
///   is redundant.
#[derive(
    Clone,
    Copy,
    Debug,
    Default,
    PartialEq,
    Eq,
    Serialize,
    Deserialize,
    Allocative,
    Pagable
)]
#[serde(rename_all = "snake_case")]
pub enum PackageVisibilityDefaultIntersection {
    #[default]
    Off,
    Audit,
    Enforce,
}

const DEFAULT_INTERSECTION: SettingKey<PackageVisibilityDefaultIntersection> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "default_intersection",
        // Enforcement control: overrides must not be able to weaken it.
        overridable_in: &[],
    },
    internal_default: Some(PackageVisibilityDefaultIntersection::Off),
    oss_default: Some(PackageVisibilityDefaultIntersection::Off),
};

#[derive(Debug, Default, Deserialize, Serialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
pub(super) struct PackageVisibilitySectionData {
    default_intersection: Option<PackageVisibilityDefaultIntersection>,
}

/// Settings controlling PACKAGE visibility intersection.
#[derive(
    Clone,
    Dupe,
    Debug,
    Default,
    Serialize,
    Deserialize,
    PartialEq,
    Eq,
    Allocative
)]
#[serde(transparent)]
pub struct PackageVisibilitySection(pub(super) Arc<PackageVisibilitySectionData>);

impl PackageVisibilitySection {
    /// Bump when the section's settings schema or semantics change.
    pub(crate) const METADATA: SectionMetadata = SectionMetadata {
        section_name: "package_visibility",
        section_version: 0,
        settings: &[DEFAULT_INTERSECTION.metadata],
    };

    pub fn default_intersection(&self) -> PackageVisibilityDefaultIntersection {
        DEFAULT_INTERSECTION
            .resolve(self.0.default_intersection)
            .unwrap_or_default()
    }
}
