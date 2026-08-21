/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Selection of settings rollouts from the versioned TOML cache.

use buck2_error::buck2_error;

use crate::settings::settings::ALL_SECTION_METADATA;
use crate::settings::settings::BuckSettingsData;
use crate::settings::settings::SectionMetadata;

/// Selects the exact compiled version of every registered settings section.
///
/// `Ok(None)` means no rollout matches a registered section and version.
/// `Err` means a registered section or its selected version is malformed, or
/// the selected layer does not match the compiled settings schema.
pub(super) fn select_rollout_table(
    versioned_settings: toml::Table,
) -> buck2_error::Result<Option<toml::Table>> {
    let Some(layer) = select_rollout_layer_with_registry(versioned_settings, ALL_SECTION_METADATA)?
    else {
        return Ok(None);
    };
    validate_rollout_layer(&layer)?;
    Ok(Some(layer))
}

fn select_rollout_layer_with_registry(
    mut versioned_settings: toml::Table,
    sections: &[SectionMetadata],
) -> buck2_error::Result<Option<toml::Table>> {
    let mut layer = toml::Table::new();

    for section in sections {
        let Some(versions) = versioned_settings.remove(section.section_name) else {
            continue;
        };
        let toml::Value::Table(mut versions) = versions else {
            return Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "Buck settings rollout section `{}` must contain a table of versions",
                section.section_name,
            ));
        };

        let version = section.section_version.to_string();
        let Some(settings) = versions.remove(&version) else {
            continue;
        };
        let toml::Value::Table(settings) = settings else {
            return Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "Buck settings rollout section `{}` version `{version}` must contain a table of settings",
                section.section_name,
            ));
        };

        layer.insert(
            section.section_name.to_owned(),
            toml::Value::Table(settings),
        );
    }

    Ok((!layer.is_empty()).then_some(layer))
}

fn validate_rollout_layer(layer: &toml::Table) -> buck2_error::Result<()> {
    let _validated: BuckSettingsData =
        toml::Value::Table(layer.clone())
            .try_into()
            .map_err(|error| {
                buck2_error!(
                    buck2_error::ErrorTag::Input,
                    "Validating settings rollout layer: {error}"
                )
            })?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const SECTION: &str = "test_section";
    const SECTION_METADATA: SectionMetadata = SectionMetadata {
        section_name: SECTION,
        section_version: 0,
    };

    fn table(content: &str) -> toml::Table {
        toml::from_str(content).expect("Test input should be valid TOML")
    }

    fn select(content: &str) -> buck2_error::Result<Option<toml::Table>> {
        select_rollout_layer_with_registry(table(content), &[SECTION_METADATA])
    }

    #[test]
    fn selects_version() -> buck2_error::Result<()> {
        let selected = select(
            r#"
                [test_section.0]
                test_flag = true
                test_value = "compiled"

                [test_section.1]
                test_flag = false
                test_value = "future"
            "#,
        )?
        .expect("The compiled section version is present");

        assert_eq!(
            selected,
            table(
                r#"
                    [test_section]
                    test_flag = true
                    test_value = "compiled"
                "#,
            ),
        );
        Ok(())
    }

    #[test]
    fn skips_missing_version() -> buck2_error::Result<()> {
        assert_eq!(select("[test_section.1]\ntest_flag = true")?, None);
        Ok(())
    }

    #[test]
    fn ignores_unknown_section() -> buck2_error::Result<()> {
        assert_eq!(
            select(
                r#"
                    unknown_section = false

                    [test_section.0]
                    test_flag = true
                "#,
            )?,
            Some(table(
                r#"
                    [test_section]
                    test_flag = true
                "#,
            )),
        );
        Ok(())
    }

    #[test]
    fn rejects_non_table_section() {
        select("test_section = false")
            .expect_err("A registered section must contain version tables");
    }

    #[test]
    fn rejects_non_table_version() {
        select("[test_section]\n0 = false")
            .expect_err("The selected version must contain a settings table");
    }

    #[test]
    fn accepts_valid_settings() -> buck2_error::Result<()> {
        let selected = select_rollout_table(table(
            r#"
                [log_download.0]
                log_use_manifold = true
                log_url = "https://example.com"
            "#,
        ))?
        .expect("The production section version is present");

        assert_eq!(
            selected,
            table(
                r#"
                    [log_download]
                    log_use_manifold = true
                    log_url = "https://example.com"
                "#,
            ),
        );
        Ok(())
    }

    #[test]
    fn rejects_unknown_setting_in_selected_version() {
        select_rollout_table(table(
            r#"
                [log_download.0]
                not_a_setting = true
            "#,
        ))
        .expect_err("An unknown setting must fail standalone validation");
    }

    #[test]
    fn rejects_wrong_value_type_in_selected_version() {
        select_rollout_table(table(
            r#"
                [log_download.0]
                log_use_manifold = 1
            "#,
        ))
        .expect_err("A setting with the wrong type must fail standalone validation");
    }
}
