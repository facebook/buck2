/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::soft_error;
use thiserror::Error;

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
pub struct CommonAttributeArgs {
    /// Output all attributes, equivalent of --output-attribute ''.
    ///
    /// Avoid using this flag in automation because it may be expensive
    /// to produce certain attributes, and because it makes harder to track
    /// which special attributes are used.
    #[clap(
        short = 'A',
        long,
        group = "output_attribute_flags",
        name = "output_all_attributes"
    )]
    output_all_attributes: bool,

    /// Output basic attributes, namely those the user can supply, plus rule type and package name.
    #[clap(
        short = 'B',
        long,
        group = "output_attribute_flags",
        name = "output_basic_attributes"
    )]
    output_basic_attributes: bool,

    /// Regular expressions to match attributes. Regular expressions are used in "search" mode,
    /// so for example empty string matches all attributes including special attributes.
    ///
    /// When using in automation, please specify the regular expression to match the attribute
    /// precisely, for example `--output-attribute '^headers$'` to make it easier to track
    /// which special attributes are used.
    #[clap(
         short = 'a',
         long,
         group = "output_attribute_flags",
         value_name = "ATTRIBUTE",
         // without limiting number_of_values, clap will read all space-separated values
         // after the flag, we want to require that each value be preceded individually by the flag.
         number_of_values = 1,
         // If the output_all_attributes flag (-A) is set, use "" to select all
         default_value_if("output_all_attributes", None, Some("")),
         default_value_if("output_basic_attributes", None, Some("^(buck\\.package|buck\\.type|[^\\.]*)$")),
     )]
    output_attribute: Vec<String>,

    /// Deprecated: Use `--output-attribute` instead.
    ///
    /// List of space-separated attributes to output, --output-attributes attr1 attr2.
    #[clap(
        long,
        multiple_values = true,
        value_name = "ATTRIBUTE",
        group = "output_attribute_flags"
    )]
    output_attributes: Vec<String>,
}

#[derive(Error, Debug)]
enum ArgErrors {
    #[error("`--output-attributes` is deprecated, use `--output-attribute` instead")]
    OutputAttributesDeprecated,
    #[error(
        "Passed both `--output-attribute` and `--output-attributes`, use only `--output-attribute`"
    )]
    BothOutputAttributes,
}

impl CommonAttributeArgs {
    pub fn get(&self) -> anyhow::Result<Vec<String>> {
        if !self.output_attributes.is_empty() {
            soft_error!(
                "output_attributes",
                ArgErrors::OutputAttributesDeprecated.into()
            )?;
        }

        if self.output_attributes.is_empty() {
            Ok(self.output_attribute.clone())
        } else if self.output_attribute.is_empty() {
            Ok(self.output_attributes.clone())
        } else {
            Err(ArgErrors::BothOutputAttributes.into())
        }
    }
}
