/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::QueryOutputFormat;
use buck2_core::soft_error;
use dupe::Dupe;
use thiserror::Error;

#[derive(
    Debug,
    Clone,
    Dupe,
    clap::ArgEnum,
    serde::Serialize,
    serde::Deserialize
)]
#[clap(rename_all = "snake_case")]
enum QueryOutputFormatArg {
    Dot,
    Json,
    DotCompact,
}

#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
pub struct CommonAttributeArgs {
    /// Output all attributes, equivalent of --output-attribute ''.
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

    /// List of attributes to output, --output-attribute attr1. Attributes can be
    /// regular expressions. Multiple attributes may be selected by specifying this option
    /// multiple times.
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

/// Args common to all the query commands
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(group = clap::ArgGroup::new("output_attribute_flags").multiple(false))]
pub struct CommonQueryArgs {
    #[clap(name = "QUERY", help = "the query to evaluate")]
    query: String,

    #[clap(flatten)]
    pub attributes: CommonAttributeArgs,

    #[clap(long, help = "Output in JSON format")]
    json: bool,

    #[clap(long, help = "Output in Graphviz Dot format")]
    dot: bool,

    #[clap(long, help = "Output in a more compact format than Graphviz Dot")]
    dot_compact: bool,

    #[clap(long, help = "Show target call stacks")]
    pub target_call_stacks: bool,

    #[clap(
        long,
        ignore_case = true,
        help = "Output format (default: list).",
        long_help = "Output format (default: list). \n
           dot -  dot graph format. \n
           dot_compact - compact alternative to dot format. \n
           json - JSON format.
         ",
        value_name = "dot|dot_compact|json",
        arg_enum
    )]
    output_format: Option<QueryOutputFormatArg>,

    #[clap(
        name = "QUERY_ARGS",
        help = "list of literals for a multi-query (one containing `%s` or `%Ss`)"
    )]
    query_args: Vec<String>,
}

impl CommonQueryArgs {
    fn args_as_set(args: &[String]) -> String {
        let mut s = "set(".to_owned();
        for (i, v) in args.iter().enumerate() {
            if i != 0 {
                s += " ";
            }
            s += "'";
            s += v;
            s += "'";
        }
        s += ")";
        s
    }

    pub fn output_format(&self) -> QueryOutputFormat {
        match self.output_format {
            Some(QueryOutputFormatArg::Json) => QueryOutputFormat::Json,
            Some(QueryOutputFormatArg::Dot) => QueryOutputFormat::Dot,
            Some(QueryOutputFormatArg::DotCompact) => QueryOutputFormat::DotCompact,
            None => {
                if self.json {
                    QueryOutputFormat::Json
                } else if self.dot {
                    QueryOutputFormat::Dot
                } else if self.dot_compact {
                    QueryOutputFormat::DotCompact
                } else {
                    QueryOutputFormat::Default
                }
            }
        }
    }

    pub fn get_query(&self) -> (String, Vec<String>) {
        if self.query.contains("%Ss") {
            let replacement = Self::args_as_set(&self.query_args);
            (self.query.replace("%Ss", &replacement), vec![])
        } else {
            (self.query.clone(), self.query_args.clone())
        }
    }
}
