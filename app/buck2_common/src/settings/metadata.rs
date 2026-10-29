/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

/// Source categories used to define a setting's override policy.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum OverrideSource {
    CommandLine,
    LocalSettings,
}

/// Classifies a setting's source for policy validation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum SettingSource {
    /// Repo-root `.bucksettings.toml`
    Base,
    /// Wrapper-cached rollout settings.
    Rollout,
    Override(OverrideSource),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SettingKeyRef<'a> {
    pub(crate) section: &'a str,
    pub(crate) name: &'a str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SettingKeyMetadata {
    pub(crate) name: &'static str,
    pub(crate) overridable_in: &'static [OverrideSource],
}

impl SettingKeyMetadata {
    pub(super) fn allows_source(&self, source: SettingSource) -> bool {
        match source {
            SettingSource::Base | SettingSource::Rollout => true,
            SettingSource::Override(source) => self.overridable_in.contains(&source),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct SectionMetadata {
    pub(crate) section_name: &'static str,
    pub(crate) section_version: u32,
    pub(crate) settings: &'static [SettingKeyMetadata],
}

pub(super) struct SettingKey<T> {
    pub(super) metadata: SettingKeyMetadata,
    pub(super) internal_default: Option<T>,
    pub(super) oss_default: Option<T>,
}

impl<T: Clone> SettingKey<T> {
    pub(super) fn default_value(&self) -> Option<T> {
        if cfg!(fbcode_build) {
            self.internal_default.clone()
        } else {
            self.oss_default.clone()
        }
    }

    pub(super) fn resolve(&self, value: Option<T>) -> Option<T> {
        value.or_else(|| self.default_value())
    }
}

pub(crate) fn find_setting_metadata<'a>(
    sections: &'a [SectionMetadata],
    key: SettingKeyRef<'_>,
) -> Option<&'a SettingKeyMetadata> {
    sections
        .iter()
        .find(|section| section.section_name == key.section)?
        .settings
        .iter()
        .find(|metadata| metadata.name == key.name)
}
