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

pub use crate::settings::agent_advice::AgentAdviceSection;
use crate::settings::agent_advice::AgentAdviceSectionData;
pub use crate::settings::agent_advice::BuildIntentMode;
use crate::settings::agent_interactive::AgentInteractiveSettings;
use crate::settings::agent_materialization::AgentMaterializationSettings;
pub use crate::settings::analysis::AnalysisSection;
use crate::settings::analysis::AnalysisSectionData;
pub use crate::settings::hydration::HydrationSection;
use crate::settings::hydration::HydrationSectionData;
pub use crate::settings::log_download::LogDownloadSection;
use crate::settings::log_download::LogDownloadSectionData;
use crate::settings::metadata::SectionMetadata;
pub use crate::settings::package_visibility::PackageVisibilityDefaultIntersection;
pub use crate::settings::package_visibility::PackageVisibilitySection;
use crate::settings::package_visibility::PackageVisibilitySectionData;

pub(crate) static ALL_SECTION_METADATA: &[SectionMetadata] = &[
    AgentAdviceSection::METADATA,
    AgentInteractiveSettings::METADATA,
    AgentMaterializationSettings::METADATA,
    AnalysisSection::METADATA,
    HydrationSection::METADATA,
    LogDownloadSection::METADATA,
    PackageVisibilitySection::METADATA,
];

#[derive(Debug, Default, Deserialize, Serialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
pub(crate) struct BuckSettingsData {
    #[serde(default)]
    agent_advice: AgentAdviceSectionData,
    #[serde(default)]
    agent_interactive: AgentInteractiveSettings,
    #[serde(default)]
    agent_materialization: AgentMaterializationSettings,
    #[serde(default)]
    analysis: AnalysisSectionData,
    #[serde(default)]
    hydration: HydrationSectionData,
    #[serde(default)]
    log_download: LogDownloadSectionData,
    #[serde(default)]
    package_visibility: PackageVisibilitySectionData,
}

#[derive(Clone, Dupe, Debug, Serialize, Deserialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
pub struct BuckSettings {
    #[serde(default)]
    pub agent_advice: AgentAdviceSection,
    #[serde(default)]
    pub agent_interactive: AgentInteractiveSettings,
    #[serde(default)]
    pub agent_materialization: AgentMaterializationSettings,
    #[serde(default)]
    pub analysis: AnalysisSection,
    #[serde(default)]
    pub hydration: HydrationSection,
    #[serde(default)]
    pub log_download: LogDownloadSection,
    #[serde(default)]
    pub package_visibility: PackageVisibilitySection,
}

impl From<BuckSettingsData> for BuckSettings {
    fn from(data: BuckSettingsData) -> Self {
        Self {
            agent_advice: AgentAdviceSection(Arc::new(data.agent_advice)),
            agent_interactive: data.agent_interactive,
            agent_materialization: data.agent_materialization,
            analysis: AnalysisSection(Arc::new(data.analysis)),
            hydration: HydrationSection(Arc::new(data.hydration)),
            log_download: LogDownloadSection(Arc::new(data.log_download)),
            package_visibility: PackageVisibilitySection(Arc::new(data.package_visibility)),
        }
    }
}

impl Default for BuckSettings {
    fn default() -> Self {
        BuckSettingsData::default().into()
    }
}

impl BuckSettings {
    pub fn empty() -> Self {
        Self::default()
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use serde::Deserialize;

    #[derive(Debug, Deserialize, PartialEq)]
    #[serde(deny_unknown_fields)]
    pub struct TestBuckSettingsData {
        pub test_section: Option<TestSection>,
    }

    #[derive(Debug, Deserialize, PartialEq)]
    #[serde(deny_unknown_fields)]
    pub struct TestSection {
        pub test_flag: Option<bool>,
        pub test_value: Option<String>,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use super::*;

    #[test]
    fn test_all_settings_are_registered() {
        let serialized = serde_json::to_value(BuckSettingsData::default())
            .expect("`BuckSettingsData` should serialize");
        for section in ALL_SECTION_METADATA {
            let fields: BTreeSet<_> = serialized[section.section_name]
                .as_object()
                .expect("Every registered section must exist in `BuckSettingsData`")
                .keys()
                .map(String::as_str)
                .collect();
            let registered: BTreeSet<_> = section
                .settings
                .iter()
                .map(|setting| setting.name)
                .collect();
            assert_eq!(
                section.settings.len(),
                registered.len(),
                "Setting names must be unique in {}",
                section.section_name
            );
            assert_eq!(
                fields, registered,
                "Every setting in {} must be registered, and vice versa",
                section.section_name
            );
        }
    }

    #[test]
    fn test_all_sections_are_registered() {
        let serialized = serde_json::to_value(BuckSettingsData::default())
            .expect("`BuckSettingsData` should serialize");
        let sections: BTreeSet<_> = serialized
            .as_object()
            .expect("settings data should serialize to a JSON object")
            .iter()
            .map(|(name, value)| {
                assert!(
                    value.is_object(),
                    "Every top-level `BuckSettingsData` field must be a settings section"
                );
                name.as_str()
            })
            .collect();
        let registered: BTreeSet<_> = ALL_SECTION_METADATA
            .iter()
            .map(|metadata| metadata.section_name)
            .collect();

        assert_eq!(
            ALL_SECTION_METADATA.len(),
            registered.len(),
            "Section metadata names must be unique"
        );
        assert_eq!(
            sections, registered,
            "Every `BuckSettingsData` section must be registered in `ALL_SECTION_METADATA`, and vice versa"
        );
    }
}
