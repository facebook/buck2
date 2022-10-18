mod markdown;

use async_trait::async_trait;
use buck2_client::client_ctx::ClientCommandContext;
use buck2_client::commands::streaming::StreamingCommand;
use buck2_client::common::CommonBuildConfigurationOptions;
use buck2_client::common::CommonConsoleOptions;
use buck2_client::common::CommonDaemonCommandOptions;
use buck2_client::daemon::client::BuckdClientConnector;
use buck2_client::exit_result::ExitResult;
use cli_proto::UnstableDocsRequest;
use gazebo::dupe::Dupe;
use gazebo::prelude::*;
use starlark::values::docs::Doc;

use crate::commands::docs::starlark::markdown::generate_markdown_files;
use crate::commands::docs::starlark::markdown::MarkdownFileOptions;

#[derive(Debug, Clone, Dupe, clap::ArgEnum)]
#[clap(rename_all = "snake_case")]
enum DocsOutputFormatArg {
    Json,
    MarkdownFiles,
}

#[derive(Debug, clap::Parser)]
#[clap(
    name = "docs-starlark",
    about = "Print documentation of user-defined starlark symbols"
)]
pub(crate) struct DocsStarlarkCommand {
    #[clap(flatten)]
    pub config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(flatten)]
    markdown_file_opts: MarkdownFileOptions,

    #[clap(
        long = "format",
        help = "how to format the returned documentation",
        default_value = "json",
        arg_enum,
        ignore_case = true
    )]
    format: DocsOutputFormatArg,

    #[clap(
        long = "builtins",
        help = "get documentation for built in functions, rules, and providers"
    )]
    builtins: bool,

    #[clap(
        long = "prelude",
        help = "get documentation for the prelude, if present"
    )]
    prelude: bool,

    #[clap(
        name = "SYMBOL_PATTERNS",
        help = "Patterns to interpret. //foo:bar.bzl is 'every symbol in //foo:bar.bzl', //foo:bar.bzl:baz only returns the documentation for the symbol 'baz' in //foo:bar.bzl"
    )]
    patterns: Vec<String>,
}

#[async_trait]
impl StreamingCommand for DocsStarlarkCommand {
    const COMMAND_NAME: &'static str = "docs starlark";
    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        let client_context =
            ctx.client_context(&self.config_opts, matches, self.sanitized_argv())?;

        let response = buckd
            .with_flushing()
            .unstable_docs(
                UnstableDocsRequest {
                    context: Some(client_context),
                    symbol_patterns: self.patterns.clone(),
                    retrieve_builtins: self.builtins,
                    retrieve_prelude: self.prelude,
                },
                ctx.stdin().console_interaction_stream(&self.console_opts),
            )
            .await??;

        let docs: Vec<Doc> = response
            .docs
            .try_map(|doc_item| serde_json::from_str(&doc_item.json))?;
        match self.format {
            DocsOutputFormatArg::Json => {
                serde_json::to_writer_pretty(std::io::stdout(), &docs)?;
            }
            DocsOutputFormatArg::MarkdownFiles => {
                generate_markdown_files(&self.markdown_file_opts, docs)?;
            }
        }

        ExitResult::success()
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
