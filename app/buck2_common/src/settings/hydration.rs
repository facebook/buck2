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
use buck2_fs::paths::file_name::FileName;
use dice::EvictionPolicy;
use dice::PageOutPolicy;
use dupe::Dupe;
use serde::Deserialize;
use serde::Serialize;

use crate::invocation_paths::DEFAULT_ISOLATION_DIR;
use crate::settings::metadata::OverrideSource;
use crate::settings::metadata::SectionMetadata;
use crate::settings::metadata::SettingKey;
use crate::settings::metadata::SettingKeyMetadata;

const ENABLE_PAGING: SettingKey<bool> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "enable_paging",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    // Absence must remain distinct from `false` while legacy buckconfig is the fallback.
    internal_default: None,
    oss_default: None,
};

const DEFER_FIELD_READS: SettingKey<bool> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "defer_field_reads",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    // Rolled out only through the section's versioned stanza, which binaries at
    // an earlier version never select, so the key never reaches one that would
    // reject it.
    internal_default: Some(false),
    oss_default: Some(false),
};

const PAGE_OUT_ON_IDLE: SettingKey<bool> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "page_out_on_idle",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    // Absence must remain distinct from `false` while legacy buckconfig is the fallback.
    internal_default: None,
    oss_default: None,
};

#[derive(
    Allocative,
    Clone,
    Copy,
    Debug,
    Default,
    Deserialize,
    Serialize,
    PartialEq,
    Eq
)]
#[serde(rename_all = "snake_case")]
enum PageOutOnIdleIsolationDirScope {
    NonDefault,
    #[default]
    All,
}

const PAGE_OUT_ON_IDLE_ISOLATION_DIR_SCOPE: SettingKey<PageOutOnIdleIsolationDirScope> =
    SettingKey {
        metadata: SettingKeyMetadata {
            name: "page_out_on_idle_isolation_dir_scope",
            overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
        },
        internal_default: Some(PageOutOnIdleIsolationDirScope::All),
        oss_default: Some(PageOutOnIdleIsolationDirScope::All),
    };

const PAGE_OUT_POLICY: SettingKey<PageOutPolicy> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "page_out_policy",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    internal_default: Some(PageOutPolicy::OncePerValue),
    oss_default: Some(PageOutPolicy::OncePerValue),
};

const EVICTION_POLICY: SettingKey<EvictionPolicy> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "eviction_policy",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    internal_default: Some(EvictionPolicy::OnMemoryPressure),
    oss_default: Some(EvictionPolicy::OnMemoryPressure),
};

#[derive(Debug, Default, Deserialize, Serialize, PartialEq, Eq, Allocative)]
#[serde(deny_unknown_fields)]
pub(super) struct HydrationSectionData {
    enable_paging: Option<bool>,
    defer_field_reads: Option<bool>,
    page_out_on_idle: Option<bool>,
    page_out_on_idle_isolation_dir_scope: Option<PageOutOnIdleIsolationDirScope>,
    page_out_policy: Option<PageOutPolicy>,
    eviction_policy: Option<EvictionPolicy>,
}

/// Settings controlling hydration/paging behavior.
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
pub struct HydrationSection(pub(super) Arc<HydrationSectionData>);

impl HydrationSection {
    /// Bump when the section's settings schema or semantics change. A rollout
    /// stanza is selected only by the exact version, so a bump must ship with a
    /// copy of every key from the previous version's stanza.
    pub(crate) const METADATA: SectionMetadata = SectionMetadata {
        section_name: "hydration",
        section_version: 3,
        settings: &[
            ENABLE_PAGING.metadata,
            DEFER_FIELD_READS.metadata,
            PAGE_OUT_ON_IDLE.metadata,
            PAGE_OUT_ON_IDLE_ISOLATION_DIR_SCOPE.metadata,
            PAGE_OUT_POLICY.metadata,
            EVICTION_POLICY.metadata,
        ],
    };

    /// Returns `None` when legacy buckconfig should determine the behavior.
    pub fn enable_paging(&self) -> Option<bool> {
        ENABLE_PAGING.resolve(self.0.enable_paging)
    }

    /// Materialize deferred Starlark fields on first access after page-in.
    pub fn defer_field_reads(&self) -> bool {
        DEFER_FIELD_READS
            .resolve(self.0.defer_field_reads)
            .expect("Deferred field reads should have a default")
    }

    /// Returns `None` when legacy buckconfig should determine the behavior.
    pub fn page_out_on_idle(&self) -> Option<bool> {
        PAGE_OUT_ON_IDLE.resolve(self.0.page_out_on_idle)
    }

    /// Which values a page-out writes; see [`PageOutPolicy`].
    pub fn page_out_policy(&self) -> PageOutPolicy {
        PAGE_OUT_POLICY
            .resolve(self.0.page_out_policy)
            .expect("Page-out policy should have a default")
    }

    /// When the values a page-out writes leave memory; see [`EvictionPolicy`].
    pub fn eviction_policy(&self) -> EvictionPolicy {
        EVICTION_POLICY
            .resolve(self.0.eviction_policy)
            .expect("Eviction policy should have a default")
    }

    pub(crate) fn page_out_on_idle_applies_to_isolation_dir(
        &self,
        isolation_dir: &FileName,
    ) -> bool {
        match PAGE_OUT_ON_IDLE_ISOLATION_DIR_SCOPE
            .resolve(self.0.page_out_on_idle_isolation_dir_scope)
            .expect("Idle page-out isolation dir scope should have a default")
        {
            PageOutOnIdleIsolationDirScope::NonDefault => {
                isolation_dir.as_str() != DEFAULT_ISOLATION_DIR
            }
            PageOutOnIdleIsolationDirScope::All => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::settings::BuckSettings;
    use crate::settings::parser::resolve_setting_flags;
    use crate::settings::parser::table;

    #[test]
    fn test_default_hydration_settings() {
        let hydration = BuckSettings::empty().hydration;
        assert_eq!(hydration.enable_paging(), None);
        assert!(!hydration.defer_field_reads());
        assert_eq!(hydration.page_out_on_idle(), None);
        assert_eq!(hydration.page_out_policy(), PageOutPolicy::OncePerValue);
        assert_eq!(
            hydration.eviction_policy(),
            EvictionPolicy::OnMemoryPressure
        );
        assert!(
            hydration.page_out_on_idle_applies_to_isolation_dir(
                FileName::new(DEFAULT_ISOLATION_DIR)
                    .expect("The default isolation dir should be valid")
            )
        );
        assert!(hydration.page_out_on_idle_applies_to_isolation_dir(
            FileName::new("custom").expect("The test isolation dir should be valid")
        ));
    }

    #[test]
    fn test_hydration_settings() -> buck2_error::Result<()> {
        let settings = resolve_setting_flags(vec![table(
            "[hydration]\nenable_paging = true\ndefer_field_reads = true\npage_out_on_idle = false\npage_out_on_idle_isolation_dir_scope = \"non_default\"\npage_out_policy = \"once_per_key\"\neviction_policy = \"on_idle\"",
        )])?;
        assert_eq!(settings.hydration.enable_paging(), Some(true));
        assert!(settings.hydration.defer_field_reads());
        assert_eq!(settings.hydration.page_out_on_idle(), Some(false));
        assert_eq!(
            settings.hydration.page_out_policy(),
            PageOutPolicy::OncePerKey
        );
        assert_eq!(settings.hydration.eviction_policy(), EvictionPolicy::OnIdle);
        assert!(
            !settings
                .hydration
                .page_out_on_idle_applies_to_isolation_dir(
                    FileName::new(DEFAULT_ISOLATION_DIR)
                        .expect("The default isolation dir should be valid")
                )
        );
        assert!(
            settings
                .hydration
                .page_out_on_idle_applies_to_isolation_dir(
                    FileName::new("custom").expect("The test isolation dir should be valid")
                )
        );
        Ok(())
    }
}
