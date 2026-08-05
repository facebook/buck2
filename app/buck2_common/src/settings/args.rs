/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
enum SettingsArgumentError {
    #[error("Could not find equals sign (`=`) in setting `{0}`")]
    NoEqualsSeparator(String),
    #[error("Expected setting in the form `key=value` or `section.key=value`, but got `{0}`")]
    MissingData(String),
    #[error("Setting key contains whitespace in `{0}`")]
    WhitespaceInKey(String),
    #[error("Setting key must contain at most one section separator (`.`) in `{0}`")]
    TooManySectionSeparators(String),
    #[error("Expected one key-value pair, but got `{0}`")]
    MultipleKeyValuePairs(String),
}

/// A settings layer parsed from a single `--setting` argument.
#[derive(Clone, Debug, PartialEq)]
pub struct SettingOverride(toml::Table);

impl SettingOverride {
    /// Converts override into underlying TOML table.
    pub fn into_table(self) -> toml::Table {
        self.0
    }
}

/// Parses one `--setting` argument in the format `key=value` or `section.key=value`.
pub fn parse_setting_flag_arg(raw: &str) -> buck2_error::Result<SettingOverride> {
    let (raw_section_and_key, raw_value) = raw
        .split_once('=')
        .ok_or_else(|| SettingsArgumentError::NoEqualsSeparator(raw.to_owned()))?;

    if raw_value.is_empty() {
        return Err(SettingsArgumentError::MissingData(raw.to_owned()).into());
    }
    let (section, key) = parse_setting_section_and_key(raw_section_and_key, raw)?;

    let value = parse_setting_value(raw_value, raw)?;

    let mut table = toml::Table::new();
    if let Some(section) = section {
        let mut section_table = toml::Table::new();
        section_table.insert(key.to_owned(), value);
        table.insert(section.to_owned(), toml::Value::Table(section_table));
    } else {
        table.insert(key.to_owned(), value);
    }
    Ok(SettingOverride(table))
}

fn parse_setting_section_and_key<'a>(
    raw_section_and_key: &'a str,
    raw_arg: &str,
) -> buck2_error::Result<(Option<&'a str>, &'a str)> {
    if raw_section_and_key.chars().any(char::is_whitespace) {
        return Err(SettingsArgumentError::WhitespaceInKey(raw_arg.to_owned()).into());
    }

    let (section, key) = match raw_section_and_key.split_once('.') {
        Some((section, key)) => (Some(section), key),
        None => (None, raw_section_and_key),
    };
    if key.contains('.') {
        return Err(SettingsArgumentError::TooManySectionSeparators(raw_arg.to_owned()).into());
    }
    if section.is_some_and(str::is_empty) || key.is_empty() {
        return Err(SettingsArgumentError::MissingData(raw_arg.to_owned()).into());
    }

    Ok((section, key))
}

fn parse_setting_value(raw_value: &str, raw_arg: &str) -> buck2_error::Result<toml::Value> {
    let Ok(mut table) = toml::from_str::<toml::Table>(&format!("value = {raw_value}")) else {
        return Ok(toml::Value::String(raw_value.to_owned()));
    };
    if table.len() != 1 {
        return Err(SettingsArgumentError::MultipleKeyValuePairs(raw_arg.to_owned()).into());
    }
    Ok(table
        .remove("value")
        .expect("temporary TOML table must contain `value`"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::settings::parser::table;

    #[test]
    fn test_parse_setting_flag_arg_valid_formats() -> buck2_error::Result<()> {
        assert_eq!(
            parse_setting_flag_arg("test_flag=false")?,
            SettingOverride(table("test_flag = false"))
        );
        assert_eq!(
            parse_setting_flag_arg("test_section.test_value=cli")?,
            SettingOverride(table("[test_section]\ntest_value = \"cli\""))
        );
        assert_eq!(
            parse_setting_flag_arg("x=90")?,
            SettingOverride(table("x = 90"))
        );
        Ok(())
    }

    #[test]
    fn test_parse_setting_flag_arg_whitespace() -> buck2_error::Result<()> {
        assert_eq!(
            parse_setting_flag_arg("x= value with whitespace  ")?,
            SettingOverride(table("x = \" value with whitespace  \""))
        );
        assert!(parse_setting_flag_arg("test flag=true").is_err());
        assert!(parse_setting_flag_arg("test_section. test_flag=true").is_err());
        Ok(())
    }

    #[test]
    fn test_parse_setting_flag_arg_invalid_formats() {
        for raw in [
            "test_flag",
            "=true",
            "test_flag=",
            ".test_flag=true",
            "test_section.=true",
            "a.b.c=true",
            "a=false\nb=true",
        ] {
            assert!(parse_setting_flag_arg(raw).is_err(), "accepted `{raw}`");
        }
    }
}
