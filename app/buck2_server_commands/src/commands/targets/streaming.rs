/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Server-side implementation of `buck2 targets --streaming` command.

use std::collections::HashSet;
use std::io::Write;
use std::mem;
use std::sync::Arc;
use std::sync::Mutex;

use buck2_cli_proto::TargetsResponse;
use buck2_common::pattern::package_roots::find_package_roots_stream;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern_type::PatternType;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::ParsedPattern;
use buck2_interpreter_for_build::interpreter::calculation::InterpreterCalculation;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use dupe::IterDupedExt;
use futures::Stream;
use futures::StreamExt;
use gazebo::prelude::VecExt;
use starlark_map::small_set::SmallSet;

use crate::commands::targets::fmt::Stats;
use crate::commands::targets::fmt::TargetFormatter;
use crate::commands::targets::fmt::TargetInfo;
use crate::commands::targets::mk_error;
use crate::commands::targets::Outputter;
use crate::target_hash::TargetHashes;

pub(crate) async fn targets_streaming(
    server_ctx: &dyn ServerCommandContextTrait,
    stdout: &mut impl Write,
    dice: DiceTransaction,
    formatter: Arc<dyn TargetFormatter>,
    outputter: &mut Outputter,
    parsed_patterns: Vec<ParsedPattern<TargetPatternExtra>>,
    keep_going: bool,
    cached: bool,
    imports: bool,
    fast_hash: Option<bool>, // None = no hashing
) -> anyhow::Result<TargetsResponse> {
    #[derive(Default)]
    struct Res {
        stats: Stats,           // Stats to merge in
        stderr: Option<String>, // Print to stderr (and break)
        stdout: String,         // Print to stdout
    }

    let imported = Arc::new(Mutex::new(SmallSet::new()));

    let mut packages = stream_packages(&dice, parsed_patterns)
        .map(|x| {
            let formatter = formatter.dupe();
            let imported = imported.dupe();

            dice.temporary_spawn(move |dice| async move {
                let mut res = Res::default();
                let (package, spec) = x?;
                match load_targets(&dice, package.dupe(), spec, cached).await {
                    Ok((eval_result, targets)) => {
                        res.stats.success += 1;
                        if imports {
                            let eval_imports = eval_result.imports();
                            formatter.imports(
                                &eval_result.buildfile_path().path(),
                                eval_imports,
                                Some(package.dupe()),
                                &mut res.stdout,
                            );
                            imported
                                .lock()
                                .unwrap()
                                .extend(eval_imports.iter().cloned());
                        }
                        for (i, node) in targets.iter().enumerate() {
                            res.stats.targets += 1;
                            if imports || i != 0 {
                                formatter.separator(&mut res.stdout);
                            }
                            formatter.target(
                                TargetInfo {
                                    node,
                                    target_hash: fast_hash.map(|fast| {
                                        TargetHashes::compute_immediate_one(node, fast)
                                    }),
                                },
                                &mut res.stdout,
                            )
                        }
                    }
                    Err(e) => {
                        res.stats.errors += 1;
                        formatter.package_error(package.dupe(), &e, &mut res.stdout);
                        if !keep_going {
                            res.stderr = Some(format!("Error parsing {}\n{:?}", package, e));
                        } else {
                            // TODO(nga): When "keep going" and "target name" formatter is used,
                            //   error is printed nowhere.
                        }
                    }
                }
                anyhow::Ok(res)
            })
        })
        // Use unlimited parallelism - tokio will restrict us anyway
        .buffer_unordered(1000000);

    let mut buffer = String::new();
    formatter.begin(&mut buffer);
    let mut stats = Stats::default();
    let mut needs_separator = false;
    while let Some(res) = packages.next().await {
        let res = res?;
        stats.merge(&res.stats);
        if let Some(stderr) = res.stderr {
            writeln!(server_ctx.stderr()?, "{}", stderr)?;
            return Err(mk_error(stats.errors));
        }
        if !res.stdout.is_empty() {
            if needs_separator {
                formatter.separator(&mut buffer);
            }
            needs_separator = true;
            outputter.write2(stdout, &buffer, &res.stdout)?;
            buffer.clear();
        }
    }

    // Recursively chase down all imported paths
    let mut todo = mem::take(&mut *imported.lock().unwrap());
    let mut seen_imported = HashSet::new();
    while let Some(path) = todo.pop() {
        if seen_imported.insert(path.path().clone()) {
            // If these lead to an error, that's surpsing (we had a working module with it loaded)
            // so we should always propagate the error here (even with keep_going)
            if needs_separator {
                formatter.separator(&mut buffer);
            }
            needs_separator = true;
            // No need to parallelise these this step because it will already be on the DICE graph
            let loaded = dice.get_loaded_module_from_import_path(&path).await?;
            let imports = loaded.imports().cloned().collect::<Vec<_>>();
            formatter.imports(path.path(), &imports, None, &mut buffer);
            todo.extend(imports);
            outputter.write1(stdout, &buffer)?;
            buffer.clear();
        }
    }

    formatter.end(&stats, &mut buffer);
    Ok(TargetsResponse {
        error_count: stats.errors,
        serialized_targets_output: buffer,
    })
}

/// Given the patterns, separate into those which have an explicit package, and those which are recursive
fn stream_packages<T: PatternType>(
    dice: &DiceComputations,
    patterns: Vec<ParsedPattern<T>>,
) -> impl Stream<Item = anyhow::Result<(PackageLabel, PackageSpec<T>)>> {
    let mut spec = ResolvedPattern::<T>::new();
    let mut recursive_paths = Vec::new();

    for pattern in patterns {
        match pattern {
            ParsedPattern::Target(package, target_name, extra) => {
                spec.add_target(package.dupe(), target_name, extra);
            }
            ParsedPattern::Package(package) => {
                spec.add_package(package.dupe());
            }
            ParsedPattern::Recursive(package) => {
                recursive_paths.push(package);
            }
        }
    }

    futures::stream::iter(spec.specs.into_iter().map(Ok))
        .chain(find_package_roots_stream(dice, recursive_paths).map(|x| Ok((x?, PackageSpec::All))))
}

async fn load_targets(
    dice: &DiceComputations,
    package: PackageLabel,
    spec: PackageSpec<TargetPatternExtra>,
    cached: bool,
) -> anyhow::Result<(Arc<EvaluationResult>, Vec<TargetNode>)> {
    let result = if cached {
        dice.get_interpreter_results(package.dupe()).await?
    } else {
        dice.get_interpreter_results_uncached(package.dupe())
            .await?
    };

    let targets = match spec {
        PackageSpec::Targets(targets) => targets.into_try_map(|(target, TargetPatternExtra)| {
            anyhow::Ok(result.resolve_target(target.as_ref())?.dupe())
        })?,
        PackageSpec::All => result.targets().values().duped().collect(),
    };
    Ok((result, targets))
}
