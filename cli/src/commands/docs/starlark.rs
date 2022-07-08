mod markdown;

use async_trait::async_trait;
use buck2_core::exit_result::ExitResult;
use cli_proto::UnstableDocsRequest;
use futures::FutureExt;
use gazebo::dupe::Dupe;
use starlark::values::docs::Doc;

use crate::commands::common::CommonBuildConfigurationOptions;
use crate::commands::common::CommonConsoleOptions;
use crate::commands::common::CommonDaemonCommandOptions;
use crate::commands::docs::starlark::markdown::generate_markdown_files;
use crate::commands::docs::starlark::markdown::MarkdownFileOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::CommandContext;
use crate::StreamingCommand;

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
        ctx: CommandContext,
    ) -> ExitResult {
        let client_context = ctx.client_context(&self.config_opts, matches)?;

        let response = buckd
            .with_flushing(|client| {
                client
                    .unstable_docs(UnstableDocsRequest {
                        context: Some(client_context),
                        symbol_patterns: self.patterns.clone(),
                        retrieve_builtins: self.builtins,
                    })
                    .boxed()
            })
            .await???;

        let docs: Vec<Doc> = serde_json::from_str(&response.docs_json)?;
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
