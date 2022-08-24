/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(async_closure)]
#![feature(box_syntax)]
// Plugins
#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]

use std::collections::HashSet;
use std::sync::Arc;

use anyhow::Context as _;
use buck2_build_api::bxl::calculation::BxlCalculationNoBxl;
use buck2_build_api::configure_dice::configure_dice_for_buck;
use buck2_build_api::interpreter::context::configure_build_file_globals;
use buck2_build_api::interpreter::context::configure_extension_file_globals;
use buck2_build_api::interpreter::context::prelude_path;
use buck2_build_api::interpreter::context::BuildInterpreterConfiguror;
use buck2_build_api::nodes::hacks::value_to_json;
use buck2_build_api::spawner::BuckSpawner;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
use buck2_common::pattern::resolve::resolve_target_patterns;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_common::result::SharedResult;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::package::Package;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::TargetPattern;
use buck2_events::dispatch::EventDispatcher;
use buck2_interpreter::dice::calculation::DiceCalculationDelegate;
use buck2_interpreter::dice::interpreter_setup::setup_interpreter;
use buck2_interpreter::dice::starlark_profiler::StarlarkProfilerConfiguration;
use buck2_interpreter::dice::HasCalculationDelegate;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_interpreter_for_build::interpreter::module_internals::EvaluationResult;
use buck2_interpreter_for_build::interpreter::module_internals::ModuleInternals;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::nodes::unconfigured::TargetNode;
use dice::cycles::DetectCycles;
use dice::data::DiceData;
use dice::DiceComputations;
use dice::UserComputationData;
use fbinit::FacebookInit;
use futures::future::BoxFuture;
use futures::stream::futures_unordered::FuturesUnordered;
use futures::FutureExt;
use futures_util::stream::StreamExt;
use gazebo::prelude::*;
use regex::Regex;
use structopt::clap::AppSettings;
use structopt::StructOpt;

#[cfg_attr(all(unix, not(fbcode_build)), global_allocator)]
#[cfg(all(unix, not(fbcode_build)))]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "buck-interpreter",
    about = "buck twice",
    global_settings(&[AppSettings::ColoredHelp]),
)]

pub struct Opt {
    #[structopt(long = "dir", help = "cd to here.")]
    dir: Option<String>,

    #[structopt(long = "json", help = "print targets json")]
    json: bool,

    #[structopt(long = "stats", help = "print stats")]
    stats: bool,

    #[structopt(
        long = "attrs",
        help = "regular expression matching attr keys to show, when printing targets json",
        default_value = ".*"
    )]
    attrs: String,

    #[structopt(name = "TARGET_PATTERNS", help = "Patterns to interpret")]
    patterns: Vec<String>,

    #[structopt(long = "detect_cycles", help = "detect cycles in dice. unstable")]
    detect_cycles: Option<DetectCycles>,

    #[structopt(
        long = "repeat",
        help = "number of times to repeat the execution",
        default_value = "1"
    )]
    repeat: usize,
}

trait TargetPrinter {
    fn begin(&mut self) {}
    fn end(&mut self) {}
    fn package(&mut self, _package: &Package) {}
    fn package_end(&mut self) {}
    fn target(&mut self, _package: &Package, _target_info: &TargetNode) {}
    fn err(&mut self, package: &Package, e: &anyhow::Error) {
        println!("Error parsing {}", package);
        println!("{:?}", e);
    }
}

struct JsonPrinter {
    attrs: Regex,
}

impl TargetPrinter for JsonPrinter {
    fn begin(&mut self) {
        println!("{{");
    }
    fn end(&mut self) {
        println!("}}");
    }

    fn package_end(&mut self) {
        println!("  }}")
    }

    fn package(&mut self, package: &Package) {
        println!("  \"{}\":", package);
    }

    fn target(&mut self, package: &Package, target_info: &TargetNode) {
        println!("    \"{}:{}\": {{", package, target_info.label().name());
        println!("      \"$type\": {}", target_info.rule_type());
        for (k, v) in target_info
            .attrs(AttrInspectOptions::All)
            .filter(|e| self.attrs.is_match(e.0))
        {
            println!("      \"{}\": {}", k, value_to_json(v).unwrap());
        }
        println!("    }}");
    }
}

#[derive(Debug)]
struct StatsPrinter {
    errors: u64,
    success: u64,
    targets: u64,
}

impl StatsPrinter {
    fn new() -> Self {
        Self {
            errors: 0,
            success: 0,
            targets: 0,
        }
    }
}

impl TargetPrinter for StatsPrinter {
    fn end(&mut self) {
        println!("{:?}", self)
    }

    fn package(&mut self, _package: &Package) {
        self.success += 1;
    }

    fn target(&mut self, _package: &Package, _target_info: &TargetNode) {
        self.targets += 1;
    }

    fn err(&mut self, package: &Package, e: &anyhow::Error) {
        self.errors += 1;
        println!("Error parsing {}", package);
        println!("{:?}", e);
    }
}

struct TargetNamePrinter {}
impl TargetPrinter for TargetNamePrinter {
    fn target(&mut self, package: &Package, target_info: &TargetNode) {
        println!("{}:{}", package, target_info.label().name());
    }
}

#[fbinit::main]
fn main(fb: FacebookInit) -> anyhow::Result<()> {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .context("Error creating Tokio runtime")?;

    rt.block_on(async {
        let opt = Opt::from_args();

        if let Some(d) = opt.dir {
            assert!(std::env::set_current_dir(&d).is_ok());
        }

        for _ in 0..opt.repeat {
            let cwd = AbsPathBuf::try_from(std::env::current_dir()?)?;
            let fs = ProjectRoot::new(cwd.clone());

            let legacy_cells = BuckConfigBasedCells::parse(&fs)?;
            let (legacy_configs, cells) =
                (legacy_cells.configs_by_name, legacy_cells.cell_resolver);
            let cell_alias_resolver = cells
                .get(cells.find(fs.relativize(&cwd).unwrap().as_ref()).unwrap())
                .unwrap()
                .cell_alias_resolver();

            let mut printer: Box<dyn TargetPrinter> = if opt.json {
                let attrs = Regex::new(&opt.attrs).unwrap();
                box JsonPrinter { attrs }
            } else if opt.stats {
                box StatsPrinter::new()
            } else {
                box TargetNamePrinter {}
            };

            let root_config = legacy_configs.get(cells.root_cell()).ok();
            let io = buck2_common::io::create_io_provider(fb, fs.dupe(), root_config).await?;

            let dice =
                configure_dice_for_buck(io, &BxlCalculationNoBxl, root_config, opt.detect_cycles)?;
            let dice_data = {
                let mut data = DiceData::new();
                data.set(EventDispatcher::null());
                data
            };
            let ctx = dice.with_ctx_data(UserComputationData {
                data: dice_data,
                spawner: Arc::new(BuckSpawner::default()),
                ..Default::default()
            });

            let configuror = BuildInterpreterConfiguror::new(
                Some(prelude_path(&cells)),
                InterpreterHostPlatform::Linux,
                InterpreterHostArchitecture::X86_64,
                false,
                configure_build_file_globals,
                configure_extension_file_globals,
                |_| {},
            );
            setup_interpreter(
                &ctx,
                cells.dupe(),
                configuror,
                legacy_configs,
                StarlarkProfilerConfiguration::None,
                false,
            )?;

            let ctx = ctx.commit();

            let dice_file_ops = Arc::new(ctx.file_ops());

            let parsed_patterns: Vec<ParsedPattern<TargetPattern>> = opt
                .patterns
                .try_map(|p| ParsedPattern::parse_precise(cell_alias_resolver, p))
                .context("Target pattern parsing failed.")?;

            let resolved_pattern =
                resolve_target_patterns(&cells, parsed_patterns.iter(), &*dice_file_ops)
                    .await
                    .context("Target pattern resolution failed.")?;

            parse_and_print_results(&ctx, &mut *printer, resolved_pattern).await?;
        }

        Ok(())
    })
}

async fn eval_package(
    calculation: &DiceCalculationDelegate<'_>,
    package: Package,
) -> SharedResult<EvaluationResult> {
    calculation
        .eval_build_file::<ModuleInternals>(
            &package,
            &mut StarlarkProfilerOrInstrumentation::disabled(),
        )
        .await
}

async fn parse_and_print_results(
    ctx: &DiceComputations,
    printer: &mut dyn TargetPrinter,
    spec: ResolvedPattern<TargetPattern>,
) -> anyhow::Result<()> {
    let mut futs: FuturesUnordered<
        BoxFuture<anyhow::Result<(Package, SharedResult<EvaluationResult>)>>,
    > = spec
        .specs
        .iter()
        .map(|(package, _s)| {
            let package = package.dupe();
            ctx.temporary_spawn(async move |ctx| {
                let calculation = ctx
                    .get_interpreter_calculator(
                        package.cell_name(),
                        &BuildFileCell::new(package.cell_name().clone()),
                    )
                    .await?;
                let res = eval_package(&calculation, package.dupe());
                Ok((package, res.await))
            })
            .boxed()
        })
        .collect();

    printer.begin();
    while let Some(res) = futs.next().await {
        let (package, result) = res?;
        match result {
            Ok(res) => {
                printer.package(&package);
                let spec = spec.specs.get(&package).unwrap();
                let filter = match spec {
                    PackageSpec::Targets(targets) => Some(targets.iter().collect::<HashSet<_>>()),
                    PackageSpec::All => None,
                };
                for target_info in res.targets().values() {
                    if filter
                        .as_ref()
                        .map_or(true, |t| t.contains(target_info.label().name()))
                    {
                        printer.target(&package, target_info)
                    }
                }
                printer.package_end();
            }
            Err(e) => {
                printer.err(&package, e.inner());
            }
        }
    }
    printer.end();

    Ok(())
}
