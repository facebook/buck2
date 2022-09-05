use async_trait::async_trait;
use cli_proto::BxlRequest;

use crate::client_ctx::ClientCommandContext;
use crate::command_outcome::CommandOutcome;
use crate::commands::build::print_build_result;
use crate::commands::build::FinalArtifactMaterializations;
use crate::commands::build::MaterializationsToProto;
use crate::commands::streaming::StreamingCommand;
use crate::common::CommonBuildConfigurationOptions;
use crate::common::CommonBuildOptions;
use crate::common::CommonConsoleOptions;
use crate::common::CommonDaemonCommandOptions;
use crate::daemon::client::BuckdClientConnector;
use crate::exit_result::ExitResult;

#[derive(Debug, clap::Parser)]
#[clap(name = "bxl", about = "Run BXL scripts")]
pub struct BxlCommand {
    #[clap(flatten)]
    config_opts: CommonBuildConfigurationOptions,

    #[clap(flatten)]
    console_opts: CommonConsoleOptions,

    #[clap(flatten)]
    event_log_opts: CommonDaemonCommandOptions,

    #[clap(flatten)]
    build_opts: CommonBuildOptions,

    #[clap(
        long = "materializations",
        help = "Materialize (or skip) the final artifacts, bypassing buckconfig.",
        ignore_case = true,
        arg_enum
    )]
    materializations: Option<FinalArtifactMaterializations>,

    #[clap(
        name = "BXL label",
        help = "The bxl function to execute as defined by the label of form `<cell>//path/file.bxl:<function>`"
    )]
    bxl_label: String,

    #[clap(
        name = "BXL INPUT ARGS",
        help = "Arguments passed to the bxl script",
        raw = true
    )]
    bxl_args: Vec<String>,
}

#[async_trait]
impl StreamingCommand for BxlCommand {
    const COMMAND_NAME: &'static str = "bxl";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        mut ctx: ClientCommandContext,
    ) -> ExitResult {
        let context = ctx.client_context(&self.config_opts, matches)?;
        let result = buckd
            .with_flushing()
            .bxl(
                BxlRequest {
                    context: Some(context),
                    bxl_label: self.bxl_label,
                    bxl_args: self.bxl_args,
                    build_opts: Some(self.build_opts.to_proto()),
                    final_artifact_materializations: self.materializations.to_proto() as i32,
                },
                ctx.stdin().console_interaction_stream(),
            )
            .await;
        let success = match &result {
            Ok(CommandOutcome::Success(response)) => response.error_messages.is_empty(),
            _ => false,
        };

        let console = self.console_opts.final_console();

        if success {
            console.print_success("BXL SUCCEEDED")?;
        } else {
            console.print_error("BXL FAILED")?;
        }

        // Action errors will have already been printed, but any other type
        // of error will be printed below the FAILED line here.
        let response = result??;

        print_build_result(&console, &response.error_messages)?;

        if !success {
            return ExitResult::failure();
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
