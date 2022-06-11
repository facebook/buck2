/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The query binary executes Buck Query Language files (aka .bql files).
//! It runs these files in the starlark interpreter with buck's query functions
//! available.

// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

use std::{borrow::Cow, convert::TryFrom, path::Path, sync::Arc};

use anyhow::Context as _;
use buck2_build_api::{
    bxl::starlark_defs::configure_bxl_file_globals,
    configure_dice::configure_dice_for_buck,
    interpreter::context::{
        configure_build_file_globals, configure_extension_file_globals, fbcode_prelude,
        BuildInterpreterConfiguror,
    },
};
use buck2_bxl::{bql::eval::eval_bql, bxl::calculation::BxlCalculationImpl};
use buck2_common::legacy_configs::BuckConfigBasedCells;
use buck2_core::{
    exit_result::ExitResult,
    fs::{paths::AbsPathBuf, project::ProjectFilesystem},
};
use buck2_interpreter::{
    dice::interpreter_setup::setup_interpreter_basic, extra::InterpreterHostPlatform,
};
use dice::{cycles::DetectCycles, data::DiceData, UserComputationData};
use events::dispatch::EventDispatcher;
use fbinit::FacebookInit;
use gazebo::prelude::*;
use structopt::{clap::AppSettings, StructOpt};

#[cfg_attr(all(unix, not(fbcode_build)), global_allocator)]
#[cfg(all(unix, not(fbcode_build)))]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "buck-bql",
    about = "executes bql files",
    global_settings(&[AppSettings::ColoredHelp]),
    setting = structopt::clap::AppSettings::TrailingVarArg
)]
pub struct Opt {
    #[structopt(long = "dir", help = "cd to here.")]
    dir: Option<String>,

    #[structopt(
        name = "QUERY_OR_BQL",
        help = "the query to evaluate or path to a .bql file to run"
    )]
    query_or_file: String,

    #[structopt(
        long = "exe",
        help = "used in shebang lines, allows trailing args without a `--`"
    )]
    exe: bool,

    #[structopt()]
    exe_dash_args: Vec<String>,

    #[structopt(
        long = "detect_cycles",
        help = "detect cycles in dice. unstable",
        default_value = "DISABLED"
    )]
    detect_cycles: DetectCycles,
}

#[fbinit::main]
fn main(fb: FacebookInit) -> ExitResult {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("Error creating Tokio runtime")?;

    rt.block_on(async {
        // structopt/clap have some bit of support for handling "--", but there doesn't
        // seem to be a way to configure it such that the "--" is required. Handle
        // it ourselves.
        let mut iter = std::env::args();

        let mut args = Vec::new();

        for v in &mut iter {
            if v == "--" {
                break;
            }
            args.push(v);
        }

        let opt = Opt::from_iter(args);

        let mut dash_args: Vec<_> = iter.collect();
        if opt.exe {
            assert!(dash_args.is_empty());
            dash_args = opt.exe_dash_args;
        } else {
            assert!(opt.exe_dash_args.is_empty());
        }

        if let Some(d) = opt.dir {
            assert!(std::env::set_current_dir(&d).is_ok());
        }

        let cwd = AbsPathBuf::try_from(std::env::current_dir()?)?;
        let fs = ProjectFilesystem::new(cwd.clone());

        let legacy_cells = BuckConfigBasedCells::parse(&fs)?;

        let (legacy_configs, cells) = (legacy_cells.configs_by_name, legacy_cells.cell_resolver);

        let io = buck2_common::io::create_io_provider(
            fb,
            Arc::new(fs.clone()),
            legacy_configs.get(cells.root_cell()).ok(),
        )?;

        let dice = configure_dice_for_buck(io, &BxlCalculationImpl, opt.detect_cycles);
        let per_request_data = {
            let mut data = DiceData::new();
            data.set(EventDispatcher::null());
            data
        };
        let ctx = dice.with_ctx_data(UserComputationData {
            data: per_request_data,
            ..Default::default()
        });

        let configuror = BuildInterpreterConfiguror::new(
            Some(fbcode_prelude()),
            InterpreterHostPlatform::Linux,
            false,
            configure_build_file_globals,
            configure_extension_file_globals,
            configure_bxl_file_globals,
        );

        setup_interpreter_basic(&ctx, cells.dupe(), configuror, legacy_configs);

        let ctx = ctx.commit();

        let bql = if Path::new(&opt.query_or_file).exists() {
            std::fs::read_to_string(&opt.query_or_file).unwrap()
        } else {
            opt.query_or_file
        };
        let global_target_platform = None;
        let working_dir = match fs.relativize(&cwd)? {
            Cow::Borrowed(p) => p.to_owned(),
            Cow::Owned(p) => p,
        };
        eval_bql(
            ctx,
            working_dir,
            cwd,
            global_target_platform,
            bql,
            dash_args,
        )
        .await?;

        ExitResult::success()
    })
}
