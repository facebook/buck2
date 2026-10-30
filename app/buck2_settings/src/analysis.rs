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
use serde::Deserialize;
use serde::Serialize;

use crate::metadata::OverrideSource;
use crate::metadata::SectionMetadata;
use crate::metadata::SettingKey;
use crate::metadata::SettingKeyMetadata;

const RECORD_REQUESTED_ANON_TARGETS: SettingKey<bool> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "record_requested_anon_targets",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    internal_default: Some(true),
    oss_default: Some(true),
};

#[derive(Debug, Default, Deserialize, Serialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
pub(super) struct AnalysisSectionData {
    record_requested_anon_targets: Option<bool>,
}

/// Settings controlling what analysis records beyond its providers.
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
pub struct AnalysisSection(pub(super) Arc<AnalysisSectionData>);

impl AnalysisSection {
    /// Bump when the section's settings schema or semantics change.
    pub(crate) const METADATA: SectionMetadata = SectionMetadata {
        section_name: "analysis",
        section_version: 1,
        settings: &[RECORD_REQUESTED_ANON_TARGETS.metadata],
    };

    /// Whether each analysis keeps the list of anon targets it requested.
    pub fn record_requested_anon_targets(&self) -> bool {
        RECORD_REQUESTED_ANON_TARGETS
            .resolve(self.0.record_requested_anon_targets)
            .expect("record_requested_anon_targets should have a default")
    }
}
