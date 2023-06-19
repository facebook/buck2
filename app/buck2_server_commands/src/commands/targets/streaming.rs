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
use buck2_core::bzl::ImportPath;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern_type::PatternType;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::PackageSpec;
use buck2_core::pattern::ParsedPattern;
use buck2_core::target::name::TargetName;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::load_module::INTERPRETER_CALCULATION_IMPL;
use buck2_interpreter::path::PackageFilePath;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use dupe::IterDupedExt;
use futures::future::FutureExt;
use futures::Stream;
use futures::StreamExt;
use gazebo::prelude::VecExt;
use itertools::Either;
use itertools::Itertools;
use more_futures::spawn::spawn_cancellable;
use starlark_map::small_set::SmallSet;
use thiserror::Error;
use tokio::sync::Semaphore;

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
    threads: Option<usize>,
) -> anyhow::Result<TargetsResponse> {
    struct Res {
        stats: Stats,           // Stats to merge in
        package: PackageLabel,  // The package I was operating on
        stderr: Option<String>, // Print to stderr (and break unless keep_going is set)
        stdout: String,         // Print to stdout
    }

    let imported = Arc::new(Mutex::new(SmallSet::new()));
    let threads = Arc::new(Semaphore::new(threads.unwrap_or(Semaphore::MAX_PERMITS)));

    let mut packages = stream_packages(&dice, parsed_patterns)
        .map(|x| {
            let formatter = formatter.dupe();
            let imported = imported.dupe();
            let threads = threads.dupe();
            let ctx = dice.dupe();

            spawn_cancellable(
                |_cancellation| {
                    {
                        async move {
                            let (package, spec) = x?;
                            let mut res = Res {
                                stats: Stats::default(),
                                package: package.dupe(),
                                stderr: None,
                                stdout: String::new(),
                            };
                            let targets = {
                                // This bit of code is the heavy CPU stuff, so guard it with the threads
                                let _permit = threads.acquire().await.unwrap();
                                load_targets(&ctx, package.dupe(), spec, cached, keep_going).await
                            };
                            let mut show_err = |err| {
                                res.stats.errors += 1;
                                let mut stderr = String::new();
                                formatter.package_error(
                                    package.dupe(),
                                    err,
                                    &mut res.stdout,
                                    &mut stderr,
                                );
                                res.stderr = Some(stderr);
                            };
                            match targets {
                                Ok((eval_result, targets, err)) => {
                                    if let Some(err) = err {
                                        show_err(&err);
                                        formatter.separator(&mut res.stdout);
                                    }
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
                                Err(err) => {
                                    show_err(&err);
                                }
                            }
                            anyhow::Ok(res)
                        }
                    }
                    .boxed()
                },
                &*dice.per_transaction_data().spawner,
                dice.per_transaction_data(),
            )
            .into_drop_cancel()
        })
        // Use unlimited parallelism - tokio will restrict us anyway
        .buffer_unordered(1000000);

    let mut buffer = String::new();
    formatter.begin(&mut buffer);
    let mut stats = Stats::default();
    let mut needs_separator = false;
    let mut package_files_seen = SmallSet::new();
    while let Some(res) = packages.next().await {
        let res = res?;
        stats.merge(&res.stats);
        if let Some(stderr) = &res.stderr {
            server_ctx.stderr()?.write_all(stderr.as_bytes())?;
            if !keep_going {
                return Err(mk_error(stats.errors));
            }
        }
        if !res.stdout.is_empty() {
            if needs_separator {
                formatter.separator(&mut buffer);
            }
            needs_separator = true;
            outputter.write2(stdout, &buffer, &res.stdout)?;
            buffer.clear();
        }
        if imports {
            // Need to also find imports from PACKAGE files
            let mut path = Some(PackageFilePath::for_dir(res.package.as_cell_path()));
            while let Some(x) = path {
                if package_files_seen.contains(&x) {
                    break;
                }
                package_files_seen.insert(x.clone());
                // These aren't cached, but the cost is relatively low (Starlark parsing),
                // and there aren't many, so we just do it on the main thread.
                // We ignore errors as these will bubble up as BUCK file errors already.
                if let Ok(Some(imports)) = package_imports(&dice, &x).await {
                    if needs_separator {
                        formatter.separator(&mut buffer);
                    }
                    needs_separator = true;
                    formatter.imports(x.path(), &imports, None, &mut buffer);
                    outputter.write1(stdout, &buffer)?;
                    buffer.clear();
                    imported.lock().unwrap().extend(imports.into_iter());
                }
                path = x.parent_package_file();
            }
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
    dice: &DiceTransaction,
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

#[derive(Error, Debug)]
enum TargetsError {
    #[error(
        "Unknown targets {} from package `{0}`.",
        _1.iter().map(|x| format!("`{}`", x)).join(", ")
    )]
    MissingTargets(PackageLabel, Vec<TargetName>),
}

/// Load the targets from a package. If `keep_going` is specified then it may return a `Some` error in the triple.
async fn load_targets(
    dice: &DiceComputations,
    package: PackageLabel,
    spec: PackageSpec<TargetPatternExtra>,
    cached: bool,
    keep_going: bool,
) -> anyhow::Result<(
    Arc<EvaluationResult>,
    Vec<TargetNode>,
    Option<anyhow::Error>,
)> {
    let result = if cached {
        dice.get_interpreter_results(package.dupe()).await?
    } else {
        dice.get_interpreter_results_uncached(package.dupe())
            .await?
    };

    match spec {
        PackageSpec::Targets(targets) => {
            if keep_going {
                let (miss, targets): (Vec<_>, Vec<_>) =
                    targets
                        .into_iter()
                        .partition_map(|(target, TargetPatternExtra)| {
                            match result.targets().get(target.as_ref()) {
                                None => Either::Left(target),
                                Some(x) => Either::Right(x.dupe()),
                            }
                        });
                let err = if miss.is_empty() {
                    None
                } else {
                    Some(TargetsError::MissingTargets(package.dupe(), miss).into())
                };
                Ok((result, targets, err))
            } else {
                let targets = targets.into_try_map(|(target, TargetPatternExtra)| {
                    anyhow::Ok(result.resolve_target(target.as_ref())?.dupe())
                })?;
                Ok((result, targets, None))
            }
        }
        PackageSpec::All => {
            let targets = result.targets().values().duped().collect();
            Ok((result, targets, None))
        }
    }
}

/// Return `None` if the PACKAGE file doesn't exist
async fn package_imports(
    dice: &DiceComputations,
    path: &PackageFilePath,
) -> anyhow::Result<Option<Vec<ImportPath>>> {
    INTERPRETER_CALCULATION_IMPL
        .get()?
        .get_package_file_deps(dice, path)
        .await
}
