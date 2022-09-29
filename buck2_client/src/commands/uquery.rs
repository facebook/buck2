/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::soft_error;
use cli_proto::QueryOutputFormat;
use cli_proto::UqueryRequest;
use gazebo::dupe::Dupe;

use crate::client_ctx::ClientCommandContext;
use crate::commands::streaming::StreamingCommand;
use crate::common::CommonBuildConfigurationOptions;
use crate::common::CommonConsoleOptions;
use crate::common::CommonDaemonCommandOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::exit_result::ExitResult;

#[derive(Debug, Clone, Dupe, clap::ArgEnum)]
#[clap(rename_all = "snake_case")]
enum QueryOutputFormatArg {
    Dot,
    Json,
    DotCompact,
}

#[derive(Debug, clap::Parser)]
pub(crate) struct CommonAttributeArgs {
    #[clap(
        short = 'A',
        long,
        group = "output_attribute_flags",
        name = "output_all_attributes",
        help = "Output all attributes, equivalent of --output-attribute ''"
    )]
    output_all_attributes: bool,

    #[clap(
         short = 'a',
         long,
         group = "output_attribute_flags",
         value_name = "ATTRIBUTE",
         help = "List of attributes to output, --output-attribute attr1. Attributes can be \
         regular expressions. Multiple attributes may be selected by specifying this option \
         multiple times.",
         // without limiting number_of_values, clap will read all space-separated values
         // after the flag, we want to require that each value be preceded individually by the flag.
         number_of_values = 1,
         // If the output_all_attributes flag (-A) is set, use "" to select all
         default_value_if("output_all_attributes", None, Some("")),
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

impl CommonAttributeArgs {
    pub(crate) fn get(&self) -> anyhow::Result<Vec<String>> {
        if !self.output_attributes.is_empty() {
            soft_error!(
                "output_attributes",
                anyhow::anyhow!(
                    "`--output-attributes` is deprecated, use `--output-attribute` instead"
                )
            )?;
        }

        if !self.output_attribute.is_empty() {
            Ok(self.output_attribute.clone())
        } else {
            Ok(self.output_attributes.clone())
        }
    }
}

/// Args common to all the query commands
#[derive(Debug, clap::Parser)]
#[clap(group = clap::ArgGroup::new("output_attribute_flags").multiple(false))]
pub struct CommonQueryArgs {
    #[clap(name = "QUERY", help = "the query to evaluate")]
    query: String,

    #[clap(flatten)]
    pub(crate) attributes: CommonAttributeArgs,

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

/// Perform queries on the unconfigured target graph.
///
/// The unconfigured target graph consists of the targets as they are defined in the build
/// files. In this graph, each target appears exactly once and `select()`s are in the unresolved
/// form. For large queries, the unconfigured graph may be much smaller than the configured
/// graph and queries can be much more efficiently performed there.
///
/// When querying the unconfigured graph, dependencies appearing in all branches of `select()`
/// dictionaries will be treated as dependencies.
///
/// Run `buck2 docs uquery` for more documentation about the functions available in cquery
/// expressions.
///
/// Examples:
///
/// Print all the attributes of a target
///
/// `buck2 uquery //java/com/example/app:amazing --output-all-attributes
///
/// List the deps of a target (special characters in a target will require quotes):
/// `buck2 uquery 'deps("//java/com/example/app:amazing+more")'`
///
/// select() encoding:
///
/// When printed, values with `select()`s use a special json encoding.
///
/// `1 + select({"//:a": 1, "DEFAULT": 2})` will be encoded as:
///
/// `{"__type": "concat", "items": [1, {"__type": "selector", "entries": {"//:a": 1, "DEFAULT": 2}}]}`
#[derive(Debug, clap::Parser)]
#[clap(name = "uquery")]
pub struct UqueryCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(flatten)]
    query_common: CommonQueryArgs,
}

#[async_trait]
impl StreamingCommand for UqueryCommand {
    const COMMAND_NAME: &'static str = "uquery";

    async fn exec_impl(
        mut self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        let (query, query_args) = self.query_common.get_query();
        let unstable_output_format = self.query_common.output_format() as i32;
        let output_attributes = self.query_common.attributes.get()?;
        let context = ctx.client_context(&self.config_opts, matches)?;

        let response = buckd
            .with_flushing()
            .uquery(
                UqueryRequest {
                    query,
                    query_args,
                    context: Some(context),
                    output_attributes,
                    unstable_output_format,
                    target_call_stacks: self.query_common.target_call_stacks,
                },
                ctx.stdin().console_interaction_stream(&self.console_opts),
            )
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

    fn event_log_opts(&self) -> &CommonDaemonCommandOptions {
        &self.event_log_opts
    }

    fn common_opts(&self) -> &CommonBuildConfigurationOptions {
        &self.config_opts
    }
}
