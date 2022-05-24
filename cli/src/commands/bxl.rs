use std::str::FromStr;

use async_trait::async_trait;
use buck2_core::exit_result::ExitResult;
use buck2_data::BxlFunctionLabel;
use cli_proto::{
    build_request::{Materializations, ResponseOptions},
    BxlRequest,
};
use structopt::{clap, StructOpt};
use thiserror::Error;

use crate::{
    commands::{
        build::{print_build_result, print_outputs, FinalArtifactMaterializations},
        common::{value_name_variants, CommonBuildOptions},
    },
    daemon::client::CommandOutcome,
    BuckdClient, CommandContext, CommonConfigOptions, CommonConsoleOptions, CommonEventLogOptions,
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
        long = "show-all-outputs",
        help = "Print the output paths relative to the cell"
    )]
    pub show_all_outputs: bool,

    #[structopt(
        long = "show-all-outputs-format",
        help = "Indicates the output format that should be used when using the show all outputs functionality (default: json).\n json - JSON format with relative paths.\n full_json - JSON format with absolute paths.\n",
        default_value = "json"
    )]
    pub show_all_outputs_format: ShowAllOutputsFormat,

    #[structopt(name = "BXL File", help = "The bxl file containing the bxl function")]
    pub bxl_path: String,

    #[structopt(name = "BXL Function", help = "The bxl Function to run")]
    pub bxl_fn: String,

    #[structopt(
        short = "-",
        name = "BXL INPUT ARGS",
        help = "Arguments passed to the bxl script",
        raw = true
    )]
    pub bxl_args: Vec<String>,
}

#[derive(Debug)]
pub enum ShowAllOutputsFormat {
    Json,
    FullJson,
}

#[derive(Debug, Error)]
#[error("Unknown show outputs format `{0}`. Must be one of `json` or `full_json`")]
pub struct UnknownShowOutputsFormat(String);

impl FromStr for ShowAllOutputsFormat {
    type Err = UnknownShowOutputsFormat;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "json" => Ok(Self::Json),
            "full_json" => Ok(Self::FullJson),
            _ => Err(UnknownShowOutputsFormat(s.to_owned())),
        }
    }
}

#[async_trait]
impl StreamingCommand for BxlCommand {
    const COMMAND_NAME: &'static str = "bxl";

    async fn exec_impl(
        self,
        mut buckd: BuckdClient,
        matches: &clap::ArgMatches,
        ctx: CommandContext,
    ) -> ExitResult {
        let result = buckd
            .bxl(BxlRequest {
                context: Some(ctx.client_context(&self.config_opts, matches)?),
                bxl_function: Some(BxlFunctionLabel {
                    bxl_path: self.bxl_core.bxl_path,
                    name: self.bxl_core.bxl_fn,
                }),
                bxl_args: self.bxl_core.bxl_args,
                response_options: Some(ResponseOptions {
                    return_outputs: self.bxl_core.show_all_outputs,
                }),
                build_opts: Some(self.build_opts.to_proto()),
                final_artifact_materializations: match self.materializations {
                    Some(FinalArtifactMaterializations::All) => {
                        Materializations::Materialize as i32
                    }
                    Some(FinalArtifactMaterializations::None) => Materializations::Skip as i32,
                    None => Materializations::Default as i32,
                },
            })
            .await;
        let success = match &result {
            Ok(CommandOutcome::Success(response)) => response.error_messages.is_empty(),
            Ok(CommandOutcome::Failure(_)) => false,
            Err(_) => false,
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

        if !response.serialized_build_report.is_empty() {
            console.print_stdout(&response.serialized_build_report)?;
        }

        if !success {
            return ExitResult::failure();
        }

        if self.bxl_core.show_all_outputs {
            print_outputs(
                &console,
                response.build_targets,
                match self.bxl_core.show_all_outputs_format {
                    ShowAllOutputsFormat::Json => None,
                    ShowAllOutputsFormat::FullJson => Some(response.project_root),
                },
                true,
                true, // we treat bxl outputs as "other_outputs", so always show all outputs
            )?;
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
