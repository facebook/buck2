/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use dupe::Dupe;
use serde::Deserialize;
use serde::Serialize;

use crate::metadata::OverrideSource;
use crate::metadata::SectionMetadata;
use crate::metadata::SettingKey;
use crate::metadata::SettingKeyMetadata;

const ENABLED: SettingKey<bool> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "enabled",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    internal_default: Some(false),
    oss_default: Some(false),
};

/// Skip final build output materialization for direct agent calls that do not request outputs.
#[derive(
    Clone,
    Copy,
    Dupe,
    Debug,
    Default,
    Serialize,
    Deserialize,
    PartialEq,
    Eq,
    Allocative
)]
#[serde(default, deny_unknown_fields)]
pub struct AgentMaterializationSettings {
    enabled: Option<bool>,
}

impl AgentMaterializationSettings {
    /// Bump when the section's settings schema or semantics change.
    pub(crate) const METADATA: SectionMetadata = SectionMetadata {
        section_name: "agent_materialization",
        section_version: 0,
        settings: &[ENABLED.metadata],
    };

    pub fn enabled(&self) -> bool {
        ENABLED
            .resolve(self.enabled)
            .expect("Agent materialization policy has a default")
    }
}
