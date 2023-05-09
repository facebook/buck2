/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_cli_proto::QueryOutputFormat;
use buck2_client_ctx::query_args::CommonAttributeArgs;
use buck2_query_parser::placeholder::QUERY_PERCENT_SS_PLACEHOLDER;
use dupe::Dupe;

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

/// Args common to all the query commands
#[derive(Debug, clap::Parser, serde::Serialize, serde::Deserialize)]
#[clap(group = clap::ArgGroup::new("output_attribute_flags").multiple(false))]
pub(crate) struct CommonQueryArgs {
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
        if self.query.contains(QUERY_PERCENT_SS_PLACEHOLDER) {
            let replacement = Self::args_as_set(&self.query_args);
            (
                self.query
                    .replace(QUERY_PERCENT_SS_PLACEHOLDER, &replacement),
                vec![],
            )
        } else {
            (self.query.clone(), self.query_args.clone())
        }
    }
}
