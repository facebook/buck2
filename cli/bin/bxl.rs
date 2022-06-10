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

use std::{convert::TryFrom, sync::Arc};

use anyhow::Context;
use buck2_build_api::{
    actions::{
        build_listener,
        build_listener::{BuildSignalSender, SetBuildSignals},
        run::knobs::HasRunActionKnobs,
    },
    build::MaterializationContext,
    bxl::{
        calculation::BxlCalculation,
        common::CliResolutionCtx,
        eval::{get_bxl_callable, resolve_cli_args},
        starlark_defs::configure_bxl_file_globals,
        BxlKey,
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
        CommandExecutorConfig, CommandExecutorKind, LocalExecutorOptions,
    },
    interpreter::context::{
        configure_build_file_globals, configure_extension_file_globals, fbcode_prelude,
        BuildInterpreterConfiguror,
    },
};
use buck2_common::{
    dice::{cells::HasCellResolver, data::SetIoProvider},
    legacy_configs::{dice::HasLegacyConfigs, BuckConfigBasedCells},
};
use buck2_core::{
    exit_result::ExitResult,
    fs::{
        paths::{AbsPathBuf, ForwardRelativePathBuf},
        project::{ProjectFilesystem, ProjectRelativePathBuf},
    },
    package::Package,
};
use buck2_interpreter::{
    common::StarlarkModulePath, dice::interpreter_setup::setup_interpreter_basic,
    extra::InterpreterHostPlatform,
};
use cli::{
    commands::bxl::BxlCoreOpts,
    daemon::{
        build::results::result_report::ResultReporter,
        bxl::{convert_bxl_build_result, ensure_artifacts},
        common,
        common::{parse_concurrency, CommandExecutorFactory},
    },
};
use cli_proto::common_build_options::ExecutionStrategy;
use dice::{cycles::DetectCycles, Dice, DiceTransaction, UserComputationData};
use events::dispatch::EventDispatcher;
use fbinit::FacebookInit;
use gazebo::prelude::*;
use host_sharing::{HostSharingBroker, HostSharingStrategy};
use structopt::{clap::AppSettings, StructOpt};
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

    #[structopt(flatten)]
    bxl_core: BxlCoreOpts,
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

    let artifact_fs = dice.get_artifact_fs().await;

    let bxl_label =
        common::parse_bxl_label_from_cli(&cwd, &opt.bxl_core.bxl_label, &cell_resolver)?;

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
        opt.bxl_core.bxl_args,
        &frozen_callable,
    )?);

    let result = dice
        .eval_bxl(BxlKey::new(bxl_label.clone(), bxl_args))
        .await?;

    let materialization_ctx = MaterializationContext::Materialize {
        map: Arc::new(Default::default()),
        force: false,
    };
    let build_result = ensure_artifacts(&dice, &materialization_ctx, result).await;

    let result_collector = ResultReporter::new(&artifact_fs, false);

    match build_result {
        None => {}
        Some(build_result) => {
            if let Some(error_messages) =
                convert_bxl_build_result(&bxl_label, result_collector, build_result)
            {
                for error_message in error_messages {
                    println!("{}", error_message)
                }
            }
        }
    };

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
            CommandExecutorConfig::new_with_default_path_separator(CommandExecutorKind::Local(
                LocalExecutorOptions {},
            )),
        );
        data.data.set(EventDispatcher::null());
        data.set_command_executor(box CommandExecutorFactory::new(
            re_client_manager.get_re_connection(),
            host_sharing_broker,
            materializer.dupe(),
            ExecutionStrategy::Default.into(),
            Default::default(),
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
