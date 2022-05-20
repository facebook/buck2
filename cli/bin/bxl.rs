/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This binary executes bxl. Temporarily here for fast iteration and debugging without the
//! buck2 daemon

#![feature(box_syntax)]
// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

use std::{collections::HashMap, convert::TryFrom, fs::File, path::Path, str::FromStr, sync::Arc};

use anyhow::Context;
use buck2_build_api::{
    actions::{
        build_listener,
        build_listener::{BuildSignalSender, SetBuildSignals},
        run::knobs::HasRunActionKnobs,
    },
    artifact_groups::ArtifactGroup,
    build::{
        materialize_artifact_group, BuildProviderType, MaterializationContext, ProviderArtifacts,
    },
    bxl::{
        calculation::BxlCalculation,
        common::CliResolutionCtx,
        eval::{get_bxl_callable, resolve_cli_args},
        result::BxlResult,
        starlark_defs::{configure_bxl_file_globals, context::build::StarlarkBuildResult},
        BxlFunctionLabel, BxlKey,
    },
    calculation::Calculation,
    context::SetBuildContextData,
    execute::{
        blocking::{BuckBlockingExecutor, SetBlockingExecutor},
        commands::{
            dice_data::{set_fallback_executor_config, SetCommandExecutor},
            re::{client::RemoteExecutionStaticMetadata, manager::ReConnectionManager},
        },
        materializer::{deferred::DeferredMaterializer, SetMaterializer},
        ActionExecutorConfig, LocalExecutorOptions,
    },
    interpreter::context::{
        configure_build_file_globals, configure_extension_file_globals, fbcode_prelude,
        BuildInterpreterConfiguror,
    },
};
use buck2_common::{
    dice::{
        cells::HasCellResolver,
        data::{HasIoProvider, SetIoProvider},
    },
    legacy_configs::{dice::HasLegacyConfigs, BuckConfigBasedCells},
};
use buck2_core::{
    cells::CellResolver,
    exit_result::ExitResult,
    fs::{
        paths::{AbsPathBuf, ForwardRelativePathBuf},
        project::{ProjectFilesystem, ProjectRelativePath, ProjectRelativePathBuf},
    },
    package::Package,
};
use buck2_interpreter::{
    common::{BxlFilePath, StarlarkModulePath},
    dice::interpreter_setup::setup_interpreter_basic,
    extra::InterpreterHostPlatform,
    parse_import::{parse_import_with_config, ParseImportOptions},
};
use cli::daemon::{
    build::{
        results::{
            build_report::BuildReportCollector, result_report::ResultReporter, BuildOwner,
            BuildResultCollector,
        },
        BuildTargetResult,
    },
    common::{parse_concurrency, CommandExecutorFactory},
};
use cli_proto::{common_build_options::ExecutionStrategy, BuildTarget};
use dice::{cycles::DetectCycles, Dice, DiceComputations, DiceTransaction, UserComputationData};
use events::{dispatch::EventDispatcher, TraceId};
use fbinit::FacebookInit;
use futures::FutureExt;
use gazebo::prelude::*;
use host_sharing::{HostSharingBroker, HostSharingStrategy};
use itertools::Itertools;
use structopt::{clap::AppSettings, StructOpt};
use thiserror::Error;
use tokio::runtime::Builder;

#[cfg_attr(all(unix, not(fbcode_build)), global_allocator)]
#[cfg(all(unix, not(fbcode_build)))]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "buck-bxl",
    about = "executes bxl files",
    global_settings(&[AppSettings::ColoredHelp]),
    setting = structopt::clap::AppSettings::TrailingVarArg
)]
pub struct Opt {
    #[structopt(long = "dir", help = "cd to here.")]
    dir: Option<String>,

    #[structopt(
        long = "detect_cycles",
        help = "detect cycles in dice. unstable",
        default_value = "DISABLED"
    )]
    detect_cycles: DetectCycles,

    #[structopt(name = "BXL File", help = "The bzl file containing the BXL function")]
    path: String,

    #[structopt(name = "BXL Function", help = "The BXL Function to test")]
    bxl_fn: String,

    #[structopt(
        long = "show-all-outputs",
        help = "Print the output paths relative to the cell"
    )]
    show_all_outputs: bool,

    #[structopt(
        long = "show-all-outputs-format",
        help = "Indicates the output format that should be used when using the show all outputs functionality (default: json).\n json - JSON format with relative paths.\n full_json - JSON format with absolute paths.\n",
        default_value = "json"
    )]
    show_all_outputs_format: ShowAllOutputsFormat,

    /// Print a build report
    ///
    /// --build-report=- will print the build report to stdout
    /// --build-report=<filepath> will write the build report to the file
    #[structopt(long = "build-report", value_name = "PATH")]
    build_report: Option<String>,

    #[structopt(
        short = "-",
        name = "BXL INPUT ARGS",
        help = "Arguments passed to the bxl script",
        raw = true
    )]
    bxl_args: Vec<String>,
}

#[derive(Debug)]
enum ShowAllOutputsFormat {
    Json,
    FullJson,
}

#[derive(Debug, Error)]
#[error("Unknown show outputs format `{0}`. Must be one of `json` or `full_json`")]
struct UnknownShowOutputsFormat(String);

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

fn parse_label_from_cli(
    cwd: &ProjectRelativePath,
    path: &str,
    bxl_fn: &str,
    cell_resolver: &CellResolver,
) -> anyhow::Result<BxlFunctionLabel> {
    let current_cell = cell_resolver.get_cell_path(cwd)?;

    // Targets with cell aliases should be resolved against the cell mapping
    // as defined the cell derived from the cwd.
    let cell_alias_resolver = cell_resolver
        .get(current_cell.cell())
        .unwrap()
        .cell_alias_resolver();

    const OPTS: ParseImportOptions = ParseImportOptions {
        allow_missing_at_symbol: true,
        allow_relative_imports: true,
    };
    let import_path = parse_import_with_config(cell_alias_resolver, &current_cell, path, &OPTS)?;

    Ok(BxlFunctionLabel {
        bxl_path: BxlFilePath::new(import_path)?,
        name: bxl_fn.to_owned(),
    })
}

#[fbinit::main]
fn main(fb: fbinit::FacebookInit) -> ExitResult {
    let rt = Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("Error creating Tokio runtime")?;

    rt.block_on(async move {
        build_listener::scope(EventDispatcher::null(), |build_signals| async move {
            async_main(fb, build_signals).await
        })
        .await
    })?;

    ExitResult::success()
}

async fn async_main(
    fb: fbinit::FacebookInit,
    build_signal_sender: BuildSignalSender,
) -> anyhow::Result<()> {
    let args = std::env::args().collect::<Vec<String>>();

    let opt = Opt::from_iter(args);

    let (cwd, dice) = setup(fb, build_signal_sender, &opt)?;

    let cell_resolver = dice.get_cell_resolver().await;

    let io = dice.global_data().get_io_provider();
    let fs = io.fs();
    let artifact_fs = dice.get_artifact_fs().await;

    let bxl_label = parse_label_from_cli(&cwd, &opt.path, &opt.bxl_fn, &cell_resolver)?;

    let cur_package = Package::from_cell_path(&cell_resolver.get_cell_path(&cwd)?);
    let cell_name = cell_resolver.find(&cwd)?;

    // Targets with cell aliases should be resolved against the cell mapping
    // as defined the cell derived from the cwd.
    let cell = cell_resolver
        .get(cell_name)
        .with_context(|| format!("Cell does not exist: `{}`", cell_name))?
        .dupe();

    // The same goes for target aliases.
    let config = dice
        .get_legacy_config_for_cell(cell_name)
        .await
        .with_context(|| format!("No configuration for cell: `{}`", cell_name))?;

    let target_alias_resolver = config.target_alias_resolver();

    let bxl_module = dice
        .get_loaded_module(StarlarkModulePath::BxlFile(&bxl_label.bxl_path))
        .await?;

    let frozen_callable = get_bxl_callable(&bxl_label, &bxl_module);
    let cli_ctx = CliResolutionCtx {
        target_alias_resolver,
        cell_resolver: cell.cell_alias_resolver().dupe(),
        relative_dir: cur_package,
    };

    let bxl_args = Arc::new(resolve_cli_args(
        &bxl_label,
        &cli_ctx,
        opt.bxl_args,
        &frozen_callable,
    )?);

    let result = dice
        .eval_bxl(BxlKey::new(bxl_label.clone(), bxl_args))
        .await?;

    let build_result = ensure_artifacts(&dice, result).await;

    match build_result {
        None => {}
        Some(build_result) => {
            // TODO(T116849868) reuse the same as build command when moved to daemon
            let trace_id = TraceId::new();

            let mut build_report_collector = if opt.build_report.is_some() {
                Some(BuildReportCollector::new(
                    &trace_id,
                    &artifact_fs,
                    &fs.root,
                    dice.parse_legacy_config_property(
                        cell_resolver.root_cell(),
                        "build_report",
                        "print_unconfigured_section",
                    )
                    .await?
                    .unwrap_or(true),
                    true,
                ))
            } else {
                None
            };
            let mut result_collector = ResultReporter::new(&artifact_fs, opt.show_all_outputs);

            let mut result_collectors = vec![
                Some(&mut result_collector as &mut dyn BuildResultCollector),
                build_report_collector
                    .as_mut()
                    .map(|v| v as &mut dyn BuildResultCollector),
            ]
            .into_iter()
            .flatten()
            .collect::<Vec<&mut dyn BuildResultCollector>>();

            for r in build_result {
                result_collectors.collect_result(&BuildOwner::Bxl(&bxl_label), &r);
            }

            if let Some(build_report_collector) = build_report_collector {
                let report = build_report_collector.into_report();
                if !opt.build_report.as_ref().unwrap().is_empty() {
                    let file = File::create(fs.resolve(&cwd).join(opt.build_report.unwrap()))?;
                    serde_json::to_writer_pretty(&file, &report)?
                } else {
                    println!("{}", serde_json::to_string(&report)?);
                };
            }

            let (build_targets, error_messages) = match result_collector.results() {
                Ok(targets) => (targets, Vec::new()),
                Err(errors) => {
                    let error_strings = errors
                        .errors
                        .iter()
                        .map(|e| format!("{:#}", e))
                        .unique()
                        .collect();
                    (vec![], error_strings)
                }
            };

            for error_message in error_messages {
                println!("{}", error_message)
            }

            print_all_outputs(
                build_targets,
                match opt.show_all_outputs_format {
                    ShowAllOutputsFormat::FullJson => Some(format!("{}", fs.root.display())),
                    ShowAllOutputsFormat::Json => None,
                },
                true,
            )?;
        }
    }

    Ok(())
}

async fn ensure_artifacts(
    ctx: &DiceComputations,
    bxl_result: Arc<BxlResult>,
) -> Option<Vec<BuildTargetResult>> {
    let materialization_ctx = MaterializationContext::Materialize {
        map: Arc::new(Default::default()),
        force: false,
    };

    match &*bxl_result {
        BxlResult::None => None,
        BxlResult::BuildsArtifacts {
            built, artifacts, ..
        } => {
            let mut report = vec![];

            let mut futs = vec![];

            built.iter().for_each(|res| match res {
                StarlarkBuildResult::Built {
                    providers,
                    run_args,
                    built,
                } => {
                    let mut output_futs = vec![];

                    built.iter().for_each(|res| match res {
                        Ok(artifacts) => {
                            for (artifact, _value) in artifacts.values.iter() {
                                output_futs.push(
                                    async {
                                        Ok(ProviderArtifacts {
                                            values: materialize_artifact_group(
                                                ctx,
                                                &ArtifactGroup::Artifact(artifact.dupe()),
                                                &materialization_ctx,
                                            )
                                            .await?,
                                            provider_type: BuildProviderType::DefaultOther,
                                        })
                                    }
                                    .boxed(),
                                )
                            }
                        }
                        Err(e) => output_futs.push(futures::future::ready(Err(e.dupe())).boxed()),
                    });

                    futs.push(
                        async move {
                            BuildTargetResult {
                                outputs: futures::future::join_all(output_futs).await,
                                providers: Some(providers.dupe()),
                                run_args: run_args.clone(),
                            }
                        }
                        .boxed(),
                    )
                }

                StarlarkBuildResult::None => {}
                StarlarkBuildResult::Error(e) => report.push(BuildTargetResult {
                    outputs: vec![Err(e.dupe())],
                    providers: None,
                    run_args: None,
                }),
            });

            let mut output_futs = vec![];
            artifacts.iter().for_each(|a| {
                output_futs.push(
                    async {
                        Ok(ProviderArtifacts {
                            values: materialize_artifact_group(
                                ctx,
                                &ArtifactGroup::Artifact(a.dupe()),
                                &materialization_ctx,
                            )
                            .await?,
                            provider_type: BuildProviderType::DefaultOther,
                        })
                    }
                    .boxed(),
                );
            });

            if !output_futs.is_empty() {
                futs.push(
                    async move {
                        BuildTargetResult {
                            outputs: futures::future::join_all(output_futs).await,
                            providers: None,
                            run_args: None,
                        }
                    }
                    .boxed(),
                );
            }

            Some(futures::future::join_all(futs).await)
        }
    }
}

// TODO(T116849868): remove this and use the src::commands::build::print_outputs once in the daemon
fn print_all_outputs(
    targets: Vec<BuildTarget>,
    root_path: Option<String>,
    as_json: bool,
) -> anyhow::Result<()> {
    let mut output_map = HashMap::new();
    let mut process_output = |target: &String, output: Option<String>| -> anyhow::Result<()> {
        let output = match output {
            Some(output) => match &root_path {
                Some(root) => Path::new(&root).join(output).to_string_lossy().into_owned(),
                None => output,
            },
            None => "".to_owned(),
        };
        if as_json {
            output_map
                .entry(target.clone())
                .or_insert_with(Vec::new)
                .push(output);
        } else {
            println!("{} {}", target, output);
        }

        Ok(())
    };

    for build_target in targets {
        let outputs = build_target.outputs.into_iter();
        // only print the unconfigured target for now until we migrate everything to support
        // also printing configurations
        for output in outputs {
            process_output(&build_target.target, Some(output.path))?;
        }
    }

    if as_json {
        println!(
            "{}",
            &serde_json::to_string_pretty(&output_map)
                .unwrap_or_else(|_| panic!("Error when converting output map to JSON.")),
        );
    }

    Ok(())
}

// some common setup that should go away when we move into daemon code
fn setup(
    fb: FacebookInit,
    build_signal_sender: BuildSignalSender,
    opt: &Opt,
) -> anyhow::Result<(ProjectRelativePathBuf, DiceTransaction)> {
    if let Some(d) = &opt.dir {
        assert!(std::env::set_current_dir(d).is_ok());
    }

    let cwd = AbsPathBuf::try_from(std::env::current_dir()?)?;
    let fs = ProjectFilesystem::new(cwd);

    let legacy_cells = BuckConfigBasedCells::parse(&fs)?;

    let (legacy_configs, cells) = (legacy_cells.configs_by_name, legacy_cells.cell_resolver);

    let io = buck2_common::io::create_io_provider(
        fb,
        Arc::new(fs.clone()),
        legacy_configs.get(cells.root_cell()).ok(),
    )?;

    let dice = {
        let mut builder = Dice::builder();
        builder.set_io_provider(io);
        builder.build(opt.detect_cycles)
    };

    let static_metadata = Arc::new(RemoteExecutionStaticMetadata::from_legacy_config(
        legacy_configs.get(cells.root_cell()).unwrap(),
    )?);
    let re_client_manager = Arc::new(ReConnectionManager::new(
        fb,
        false,
        10,
        static_metadata,
        None,
    ));
    let host_sharing_broker = HostSharingBroker::new(
        HostSharingStrategy::SmallerTasksFirst,
        parse_concurrency(0)?,
    );
    let blocking_executor = Arc::new(BuckBlockingExecutor::default_concurrency(fs.clone())?);
    // hard code to deferred materializer for now until we migrate to daemon
    let materializer = Arc::new(DeferredMaterializer::new(
        fs.root,
        re_client_manager.dupe(),
        blocking_executor.dupe(),
        true,
    ));

    let per_request_data = {
        let mut data = UserComputationData::new();
        set_fallback_executor_config(
            &mut data.data,
            ActionExecutorConfig::Local(LocalExecutorOptions {}),
        );
        data.data.set(EventDispatcher::null());
        data.set_command_executor(box CommandExecutorFactory::new(
            re_client_manager.get_re_connection(),
            host_sharing_broker,
            materializer.dupe(),
            ExecutionStrategy::Default.into(),
        ));
        data.set_blocking_executor(blocking_executor);
        data.set_materializer(materializer);
        data.set_build_signals(build_signal_sender);
        data.set_run_action_knobs(Default::default());

        data
    };
    let ctx = dice.with_ctx_data(per_request_data);

    let configuror = BuildInterpreterConfiguror::new(
        Some(fbcode_prelude()),
        InterpreterHostPlatform::Linux,
        false,
        configure_build_file_globals,
        configure_extension_file_globals,
        configure_bxl_file_globals,
    );

    setup_interpreter_basic(&ctx, cells.dupe(), configuror, legacy_configs);

    ctx.set_buck_out_path(Some(ForwardRelativePathBuf::unchecked_new(
        "buck-out/v2".to_owned(),
    )));

    let ctx = ctx.commit();

    // currently we always run at root for prototyping.
    Ok((ProjectRelativePathBuf::unchecked_new("".to_owned()), ctx))
}
