/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_settings_derive::buck_settings_section;

/// Where the daemon's event logs go.
#[buck_settings_section(name = "log_download", version = 0, scope = daemon)]
pub struct LogDownload {
    /// Whether event logs are kept in Manifold. Unset in Meta-internal builds
    /// leaves the decision to the `buck2.log_use_manifold` buckconfig.
    #[setting(oss_default = false, overridable_in(CommandLine, LocalSettings))]
    log_use_manifold: Option<bool>,
    /// Where event logs are uploaded when not kept in Manifold. Unset leaves
    /// the decision to the `buck2.log_url` buckconfig.
    #[setting(overridable_in(CommandLine, LocalSettings))]
    log_url: Option<String>,
}

#[cfg(test)]
mod tests {
    use crate::BuckSettings;
    use crate::metadata::SettingKeyRef;
    use crate::metadata::find_setting_metadata;
    use crate::parser::resolve_setting_flags;
    use crate::parser::table;
    use crate::settings::ALL_SECTION_METADATA;

    #[test]
    fn test_default_log_use_manifold() {
        let expected = if cfg!(fbcode_build) {
            None
        } else {
            Some(false)
        };
        assert_eq!(
            BuckSettings::empty().daemon.log_download.log_use_manifold(),
            expected
        );
    }

    #[test]
    fn test_log_use_manifold() -> buck2_error::Result<()> {
        let settings =
            resolve_setting_flags(vec![table("[log_download]\nlog_use_manifold = false")])?;
        assert_eq!(settings.daemon.log_download.log_use_manifold(), Some(false));
        Ok(())
    }

    #[test]
    fn test_log_url() -> buck2_error::Result<()> {
        let settings =
            resolve_setting_flags(vec![table("[log_download]\nlog_url = \"test.com\"")])?;
        assert_eq!(
            settings.daemon.log_download.log_url().as_deref(),
            Some("test.com")
        );
        Ok(())
    }

    #[test]
    fn test_find_setting_metadata() {
        let find = |section, name| {
            find_setting_metadata(ALL_SECTION_METADATA, SettingKeyRef { section, name })
                .map(|metadata| metadata.name)
        };
        assert_eq!(
            find("log_download", "log_use_manifold"),
            Some("log_use_manifold")
        );
        assert_eq!(find("log_download", "log_use_maniflod"), None);
        assert_eq!(find("buck2", "log_url"), None);
    }
}
