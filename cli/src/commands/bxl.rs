use async_trait::async_trait;
use buck2_core::exit_result::ExitResult;
use cli_proto::BxlRequest;
use futures::FutureExt;
use structopt::{clap, StructOpt};

use crate::{
    commands::{
        build::{print_build_result, FinalArtifactMaterializations, MaterializationsToProto},
        common::{value_name_variants, CommonBuildOptions},
    },
    daemon::client::{BuckdClientConnector, CommandOutcome},
    CommandContext, CommonConfigOptions, CommonConsoleOptions, CommonEventLogOptions,
    StreamingCommand,
};

#[derive(Debug, StructOpt)]
#[structopt(name = "bxl", about = "Runs bxl scripts")]
pub struct BxlCommand {
    #[structopt(flatten)]
    config_opts: CommonConfigOptions,

    #[structopt(flatten)]
    console_opts: CommonConsoleOptions,

    #[structopt(flatten)]
    event_log_opts: CommonEventLogOptions,

    #[structopt(flatten)]
    build_opts: CommonBuildOptions,

    #[structopt(
    long = "materializations",
    help = "Materialize (or skip) the final artifacts, bypassing buckconfig.",
    possible_values = &FinalArtifactMaterializations::variants(),
    value_name = value_name_variants(&FinalArtifactMaterializations::variants()),
    case_insensitive = true
    )]
    materializations: Option<FinalArtifactMaterializations>,

    #[structopt(flatten)]
    bxl_core: BxlCoreOpts,
}

// TODO(bobyf) merge this when we delete the bxl binary
#[derive(Debug, StructOpt)]
pub struct BxlCoreOpts {
    #[structopt(
        name = "BXL label",
        help = "The bxl function to execute as defined by the label of form `<cell>//path/file.bxl:<function>`"
    )]
    pub bxl_label: String,

    #[structopt(
        short = "-",
        name = "BXL INPUT ARGS",
        help = "Arguments passed to the bxl script",
        raw = true
    )]
    pub bxl_args: Vec<String>,
}

#[async_trait]
impl StreamingCommand for BxlCommand {
    const COMMAND_NAME: &'static str = "bxl";

    async fn exec_impl(
        self,
        mut buckd: BuckdClientConnector,
        matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> ExitResult {
        let ctx = ctx.client_context(&self.config_opts, matches)?;
        let result = buckd
            .with_flushing(|client| {
                client
                    .bxl(BxlRequest {
                        context: Some(ctx),
                        bxl_label: self.bxl_core.bxl_label,
                        bxl_args: self.bxl_core.bxl_args,
                        build_opts: Some(self.build_opts.to_proto()),
                        final_artifact_materializations: self.materializations.to_proto() as i32,
                    })
                    .boxed()
            })
            .await;
        let success = match &result {
            Ok(Ok(CommandOutcome::Success(response))) => response.error_messages.is_empty(),
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
        let response = result???;

        print_build_result(&console, &response.error_messages)?;

        if !success {
            return ExitResult::failure();
        }

        ExitResult::success()
    }

    fn console_opts(&self) -> &CommonConsoleOptions {
        &self.console_opts
    }

    fn event_log_opts(&self) -> &CommonEventLogOptions {
        &self.event_log_opts
    }
}
