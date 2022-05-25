/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::exit_result::ExitResult;
use clap::arg_enum;
use cli_proto::{QueryOutputFormat, UqueryRequest};
use structopt::{clap, StructOpt};

use crate::{
    commands::common::{
        value_name_variants, CommonConfigOptions, CommonConsoleOptions, CommonEventLogOptions,
    },
    daemon::client::BuckdClient,
    CommandContext, StreamingCommand,
};

structopt::clap::arg_enum! {
    #[derive(Debug)]
    enum QueryOutputFormatArg {
        Dot,
        Json
    }
}

/// Perform the target graph query
// Workaround for https://github.com/TeXitoi/structopt/issues/333
#[cfg_attr(not(doc), allow(missing_docs))]
#[cfg_attr(
    doc,
    doc = r#"
Perform the target graph query
"#
)]
#[derive(Debug, StructOpt)]
pub struct CommonQueryArgs {
    #[structopt(name = "QUERY", help = "the query to evaluate")]
    query: String,

    #[structopt(
        long,
        help = "List of attributes to output, --output-attribute attr1. Attributes can be \
        regular expressions. Multiple attributes may be selected by specifying this option \
        multiple times.",
        // without limiting number_of_values, clap will read all space-separated values
        // after the flag, we want to require that each value be preceded individually by the flag.
        number_of_values = 1
    )]
    output_attribute: Vec<String>,

    /// Deprecated: Use `--output-attribute` instead.
    ///
    /// List of space-separated attributes to output, --output-attributes attr1 attr2.
    #[structopt(long)]
    output_attributes: Vec<String>,

    #[structopt(long, help = "Output in JSON format")]
    json: bool,

    #[structopt(long, help = "Output in Graphviz Dot format")]
    dot: bool,

    #[structopt(long, help = "Show target call stacks")]
    pub(crate) target_call_stacks: bool,

    #[structopt(
        long,
        possible_values = &QueryOutputFormatArg::variants(),
        value_name = value_name_variants(&QueryOutputFormatArg::variants()),
        case_insensitive = true,
        help = "Output format (default: list).",
        long_help =
        "Output format (default: list). \n
           dot -  dot graph format. \n
           json - JSON format.
         ")]
    output_format: Option<QueryOutputFormatArg>,

    #[structopt(
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

    pub fn output_attributes(&self) -> &[String] {
        if !self.output_attribute.is_empty() {
            &self.output_attribute
        } else {
            &self.output_attributes
        }
    }

    pub fn output_format(&self) -> QueryOutputFormat {
        match self.output_format {
            Some(QueryOutputFormatArg::Json) => QueryOutputFormat::Json,
            Some(QueryOutputFormatArg::Dot) => QueryOutputFormat::Dot,
            None => {
                if self.json {
                    QueryOutputFormat::Json
                } else if self.dot {
                    QueryOutputFormat::Dot
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

#[derive(Debug, StructOpt)]
#[structopt(
    name = "uquery",
    about = "provides facilities to query information about the target node graph"
)]
pub struct UqueryCommand {
    #[structopt(flatten)]
    config_opts: CommonConfigOptions,

    #[structopt(flatten)]
    console_opts: CommonConsoleOptions,

    #[structopt(flatten)]
    event_log_opts: CommonEventLogOptions,

    #[structopt(flatten)]
    query_common: CommonQueryArgs,
}

#[async_trait]
impl StreamingCommand for UqueryCommand {
    const COMMAND_NAME: &'static str = "uquery";

    async fn exec_impl(
        mut self,
        mut buckd: BuckdClient,
        matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> ExitResult {
        let (query, query_args) = self.query_common.get_query();
        let unstable_output_format = self.query_common.output_format() as i32;
        let output_attributes = self.query_common.output_attributes().to_vec();

        let response = buckd
            .uquery(UqueryRequest {
                query,
                query_args,
                context: Some(ctx.client_context(&self.config_opts, matches)?),
                output_attributes,
                unstable_output_format,
                target_call_stacks: self.query_common.target_call_stacks,
            })
            .await??;

        for message in &response.error_messages {
            crate::eprintln!("{}", message)?;
        }

        if !response.error_messages.is_empty() {
            ExitResult::failure()
        } else {
            ExitResult::success()
        }
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.console_opts
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.event_log_opts
    }
}
