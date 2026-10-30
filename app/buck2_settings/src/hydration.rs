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
use buck2_wrapper_common::DEFAULT_ISOLATION_DIR;
use dice::EvictionPolicy;
use dice::PagableStorageBackend;
use dice::PageOutPolicy;
use dupe::Dupe;
use serde::Deserialize;
use serde::Serialize;

use crate::metadata::OverrideSource;
use crate::metadata::SectionMetadata;
use crate::metadata::SettingKey;
use crate::metadata::SettingKeyMetadata;

const ENABLE_PAGING: SettingKey<bool> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "enable_paging",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    internal_default: Some(false),
    oss_default: Some(false),
};

const DEFER_FIELD_READS: SettingKey<bool> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "defer_field_reads",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    internal_default: Some(true),
    oss_default: Some(true),
};

const PAGE_OUT_ON_IDLE: SettingKey<bool> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "page_out_on_idle",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    internal_default: Some(false),
    oss_default: Some(false),
};

const PAGE_OUT_MIN_FREE_DISK_GB: SettingKey<u64> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "page_out_min_free_disk_gb",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    internal_default: Some(100),
    oss_default: Some(100),
};

const PAGABLE_STORAGE_BACKEND: SettingKey<PagableStorageBackend> = SettingKey {
    metadata: SettingKeyMetadata {
        name: "pagable_storage_backend",
        overridable_in: &[OverrideSource::CommandLine, OverrideSource::LocalSettings],
    },
    internal_default: Some(PagableStorageBackend::Sqlite),
    oss_default: Some(PagableStorageBackend::Sqlite),
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
    page_out_min_free_disk_gb: Option<u64>,
    page_out_policy: Option<PageOutPolicy>,
    eviction_policy: Option<EvictionPolicy>,
    pagable_storage_backend: Option<PagableStorageBackend>,
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
        section_version: 4,
        settings: &[
            ENABLE_PAGING.metadata,
            DEFER_FIELD_READS.metadata,
            PAGE_OUT_ON_IDLE.metadata,
            PAGE_OUT_ON_IDLE_ISOLATION_DIR_SCOPE.metadata,
            PAGE_OUT_MIN_FREE_DISK_GB.metadata,
            PAGE_OUT_POLICY.metadata,
            EVICTION_POLICY.metadata,
            PAGABLE_STORAGE_BACKEND.metadata,
        ],
    };

    /// Whether DICE values can be paged out to on-disk storage. Implied by
    /// [`Self::page_out_on_idle`].
    pub fn enable_paging(&self) -> bool {
        ENABLE_PAGING
            .resolve(self.0.enable_paging)
            .expect("Enable paging should have a default")
    }

    /// Materialize deferred Starlark fields on first access after page-in.
    pub fn defer_field_reads(&self) -> bool {
        DEFER_FIELD_READS
            .resolve(self.0.defer_field_reads)
            .expect("Deferred field reads should have a default")
    }

    /// Whether the daemon pages DICE out when it goes idle.
    pub fn page_out_on_idle(&self) -> bool {
        PAGE_OUT_ON_IDLE
            .resolve(self.0.page_out_on_idle)
            .expect("Idle page-out should have a default")
    }

    /// Idle page-out runs only when at least this many GiB of disk are free.
    pub fn page_out_min_free_disk_gb(&self) -> u64 {
        PAGE_OUT_MIN_FREE_DISK_GB
            .resolve(self.0.page_out_min_free_disk_gb)
            .expect("Idle page-out disk headroom should have a default")
    }

    /// The on-disk backend for pagable DICE storage.
    pub fn pagable_storage_backend(&self) -> PagableStorageBackend {
        PAGABLE_STORAGE_BACKEND
            .resolve(self.0.pagable_storage_backend)
            .expect("Pagable storage backend should have a default")
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

    pub fn page_out_on_idle_applies_to_isolation_dir(&self, isolation_dir: &FileName) -> bool {
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
    use crate::BuckSettings;
    use crate::parser::resolve_setting_flags;
    use crate::parser::table;

    #[test]
    fn test_default_hydration_settings() {
        let hydration = BuckSettings::empty().daemon.hydration;
        assert!(!hydration.enable_paging());
        assert!(hydration.defer_field_reads());
        assert!(!hydration.page_out_on_idle());
        assert_eq!(hydration.page_out_min_free_disk_gb(), 100);
        assert_eq!(
            hydration.pagable_storage_backend(),
            PagableStorageBackend::Sqlite
        );
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
            "[hydration]\nenable_paging = true\ndefer_field_reads = true\npage_out_on_idle = false\npage_out_on_idle_isolation_dir_scope = \"non_default\"\npage_out_min_free_disk_gb = 0\npage_out_policy = \"once_per_key\"\neviction_policy = \"on_idle\"\npagable_storage_backend = \"sled\"",
        )])?;
        assert!(settings.daemon.hydration.enable_paging());
        assert!(settings.daemon.hydration.defer_field_reads());
        assert!(!settings.daemon.hydration.page_out_on_idle());
        assert_eq!(settings.daemon.hydration.page_out_min_free_disk_gb(), 0);
        assert_eq!(
            settings.daemon.hydration.pagable_storage_backend(),
            PagableStorageBackend::Sled
        );
        assert_eq!(
            settings.daemon.hydration.page_out_policy(),
            PageOutPolicy::OncePerKey
        );
        assert_eq!(
            settings.daemon.hydration.eviction_policy(),
            EvictionPolicy::OnIdle
        );
        assert!(
            !settings
                .daemon
                .hydration
                .page_out_on_idle_applies_to_isolation_dir(
                    FileName::new(DEFAULT_ISOLATION_DIR)
                        .expect("The default isolation dir should be valid")
                )
        );
        assert!(
            settings
                .daemon
                .hydration
                .page_out_on_idle_applies_to_isolation_dir(
                    FileName::new("custom").expect("The test isolation dir should be valid")
                )
        );
        Ok(())
    }
}
