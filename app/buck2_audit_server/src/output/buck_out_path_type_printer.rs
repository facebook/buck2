/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io::Write;

use indexmap::IndexMap;
use regex::RegexSet;

use super::buck_out_path_parser::BuckOutPathType;

pub(crate) struct BuckOutPathTypePrinter {
    json: bool,
    attributes: Option<RegexSet>,
}

impl BuckOutPathTypePrinter {
    pub(crate) fn new(json: bool, attributes: &Vec<String>) -> buck2_error::Result<Self> {
        let attributes = if attributes.is_empty() {
            None
        } else {
            Some(RegexSet::new(attributes)?)
        };

        Ok(BuckOutPathTypePrinter { json, attributes })
    }

    pub(crate) fn print(
        &self,
        path_type: &BuckOutPathType,
        mut stdout: impl Write,
    ) -> buck2_error::Result<()> {
        if self.json {
            writeln!(
                &mut stdout,
                "{}",
                serde_json::to_string_pretty(&self.printable_attributes(path_type))?
            )?;
        } else {
            self.printable_attributes(path_type)
                .values()
                .try_for_each(|a| writeln!(&mut stdout, "{}", a))?;
        }
        Ok(())
    }

    fn printable_attributes(&self, path_type: &BuckOutPathType) -> IndexMap<String, String> {
        let all_attributes = self.all_attributes(path_type);

        if let Some(attributes) = &self.attributes {
            all_attributes
                .into_iter()
                .filter(|(k, _)| attributes.is_match(k))
                .collect()
        } else {
            all_attributes
        }
    }

    fn all_attributes(&self, path_type: &BuckOutPathType) -> IndexMap<String, String> {
        // Deterministic order
        let mut attributes = IndexMap::new();

        match path_type {
            BuckOutPathType::BxlOutput {
                bxl_function_label,
                common_attrs,
            } => {
                attributes.insert(
                    "bxl_function_label".to_owned(),
                    bxl_function_label.to_string(),
                );
                attributes.insert("config_hash".to_owned(), common_attrs.config_hash.clone());
                attributes.insert(
                    "full_artifact_path_no_hash".to_owned(),
                    common_attrs.raw_path_to_output.to_string(),
                );
            }
            BuckOutPathType::AnonOutput {
                path,
                target_label,
                attr_hash,
                common_attrs,
            } => {
                attributes.insert("cell_path".to_owned(), path.to_string());
                attributes.insert("target_label".to_owned(), target_label.to_string());
                attributes.insert("attr_hash".to_owned(), attr_hash.clone());
                attributes.insert("config_hash".to_owned(), common_attrs.config_hash.clone());
                attributes.insert(
                    "full_artifact_path_no_hash".to_owned(),
                    common_attrs.raw_path_to_output.to_string(),
                );
            }
            BuckOutPathType::RuleOutput {
                path,
                target_label,
                path_after_target_name,
                common_attrs,
            } => {
                attributes.insert("cell_path".to_owned(), path.to_string());
                attributes.insert("target_label".to_owned(), target_label.to_string());
                attributes.insert(
                    "short_artifact_path".to_owned(),
                    path_after_target_name.to_string(),
                );
                attributes.insert("config_hash".to_owned(), common_attrs.config_hash.clone());
                attributes.insert(
                    "full_artifact_path_no_hash".to_owned(),
                    common_attrs.raw_path_to_output.to_string(),
                );
            }
            BuckOutPathType::TestOutput { path, common_attrs } => {
                attributes.insert("cell_path".to_owned(), path.to_string());
                attributes.insert("config_hash".to_owned(), common_attrs.config_hash.clone());
                attributes.insert(
                    "full_artifact_path_no_hash".to_owned(),
                    common_attrs.raw_path_to_output.to_string(),
                );
            }
            BuckOutPathType::TmpOutput {
                path,
                target_label,
                common_attrs,
            } => {
                attributes.insert("cell_path".to_owned(), path.to_string());
                attributes.insert("target_label".to_owned(), target_label.to_string());
                attributes.insert("config_hash".to_owned(), common_attrs.config_hash.clone());
                attributes.insert(
                    "full_artifact_path_no_hash".to_owned(),
                    common_attrs.raw_path_to_output.to_string(),
                );
            }
        }

        attributes
    }
}
