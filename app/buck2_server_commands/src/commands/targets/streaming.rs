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

use buck2_common::pattern::package_roots::find_package_roots_stream;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_core::bzl::ImportPath;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern::PackageSpec;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::PatternType;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::target::name::TargetName;
use buck2_futures::spawn::spawn_dropcancel;
use buck2_interpreter::load_module::InterpreterCalculation;
use buck2_interpreter::load_module::INTERPRETER_CALCULATION_IMPL;
use buck2_interpreter::paths::package::PackageFilePath;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_server_ctx::ctx::ServerCommandContextTrait;
use dice::DiceComputations;
use dice::DiceTransaction;
use dupe::Dupe;
use futures::future::FutureExt;
use futures::Stream;
use futures::StreamExt;
use gazebo::prelude::VecExt;
use itertools::Either;
use itertools::Itertools;
use starlark_map::small_set::SmallSet;
use tokio::sync::Semaphore;

use crate::commands::targets::fmt::Stats;
use crate::commands::targets::fmt::TargetFormatter;
use crate::commands::targets::fmt::TargetInfo;
use crate::target_hash::TargetHashes;

fn write_str(outputter: &mut dyn Write, s: &mut String) -> buck2_error::Result<()> {
    outputter.write_all(s.as_bytes())?;
    s.clear();
    Ok(())
}

/// Run the targets command in streaming mode.
///
/// # Arguments
///
/// `keep_going` - On loading errors, put buck.error in the output stream and continue
///                Passing from cli args `--keep-going` from `app/buck2_client/src/commands/targets.rs`.
/// * `imports` - Show the imports of each package/import. Shows an additional output per package/import (not per target), including implicit dependencies (e.g. the prelude) but only direct dependencies (not the transitive closure)
///               Passing from cli args `--imports` from `app/buck2_client/src/commands/targets.rs`.
pub(crate) async fn targets_streaming(
    server_ctx: &dyn ServerCommandContextTrait,
    mut dice: DiceTransaction,
    formatter: Arc<dyn TargetFormatter>,
    outputter: &mut (dyn Write + Send),
    parsed_patterns: Vec<ParsedPattern<TargetPatternExtra>>,
    keep_going: bool,
    cached: bool,
    imports: bool,
    fast_hash: Option<bool>, // None = no hashing
    threads: Option<usize>,
) -> buck2_error::Result<Stats> {
    let imported = Arc::new(Mutex::new(SmallSet::new()));
    let threads = Arc::new(Semaphore::new(threads.unwrap_or(Semaphore::MAX_PERMITS)));

    let cloned_dice = dice.clone();
    let mut packages = stream_packages(&cloned_dice, parsed_patterns)
        .map(|x| {
            let formatter = formatter.dupe();
            let imported = imported.dupe();
            let threads = threads.dupe();
            let mut ctx = cloned_dice.dupe();

            spawn_dropcancel(
                |_cancellation| {
                    {
                        async move {
                            let (package, spec) = x?;
                            let res = process_package(
                                &mut ctx, formatter, package, spec, cached, keep_going, imports,
                                fast_hash, threads, imported,
                            )
                            .await;
                            buck2_error::Ok(res)
                        }
                    }
                    .boxed()
                },
                &*cloned_dice.per_transaction_data().spawner,
                cloned_dice.per_transaction_data(),
            )
        })
        // Use unlimited parallelism - tokio will restrict us anyway
        .buffer_unordered(1000000);

    let mut buffer = String::new();
    formatter.begin(&mut buffer);
    let mut stats = Stats::default();
    let mut needs_separator = false;
    let mut package_files_seen = SmallSet::new();

    // Process package results and finally output the result
    while let Some(res) = packages.next().await {
        let mut res = res?;
        stats.merge(&res.stats);

        // Print the error to stderr if exists
        if let Some(stderr) = &res.stderr {
            server_ctx.stderr()?.write_all(stderr.as_bytes())?;
            if !keep_going {
                return Err(stats
                    .to_error()
                    .expect("Result only has a stderr if there were errors"));
            }
        }

        // Output `res.stdout` which has the targets and imports
        if !res.stdout.is_empty() {
            if needs_separator {
                formatter.separator(&mut buffer);
            }
            needs_separator = true;
            write_str(outputter, &mut buffer)?;
            write_str(outputter, &mut res.stdout)?;
        }

        // Output all parent packages's imports (including self), if requested
        if imports {
            // Need to also find imports from PACKAGE files
            let mut path = Some(res.package);

            while let Some(x) = path {
                if package_files_seen.contains(&x) {
                    break;
                }
                package_files_seen.insert(x);
                // These aren't cached, but the cost is relatively low (Starlark parsing),
                // and there aren't many, so we just do it on the main thread.
                // We ignore errors as these will bubble up as BUCK file errors already.
                if let Ok(Some((package_file_path, imports))) =
                    package_imports(&mut dice, x.dupe()).await
                {
                    if needs_separator {
                        formatter.separator(&mut buffer);
                    }
                    needs_separator = true;
                    formatter.imports(package_file_path.path(), &imports, None, &mut buffer);
                    write_str(outputter, &mut buffer)?;
                    imported.lock().unwrap().extend(imports.into_iter());
                }
                // TODO(nga): we should cross cell boundary:
                //   This is what we do when we evaluate `PACKAGE` files.
                //   https://fburl.com/code/qxl59b64
                path = x.parent();
            }
        }
    }

    // Recursively chase down all `imported` paths, and output them.
    // This will only be done if `imports` is set
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
            write_str(outputter, &mut buffer)?;
        }
    }

    formatter.end(&stats, &mut buffer);
    write_str(outputter, &mut buffer)?;
    Ok(stats)
}

struct PreparePackageResult {
    stats: Stats,           // Stats to merge in
    package: PackageLabel,  // The package I was operating on
    stderr: Option<String>, // Print to stderr (and break unless keep_going is set)
    stdout: String,         // Print to stdout
}

impl PreparePackageResult {
    fn from_package(package: PackageLabel) -> Self {
        Self {
            stats: Stats::default(),
            package,
            stderr: None,
            stdout: String::new(),
        }
    }

    fn append_successful_targets(
        &mut self,
        eval_result: Arc<EvaluationResult>,
        targets: Vec<TargetNode>,
        error: Option<buck2_error::Error>,
        formatter: &dyn TargetFormatter,
        imports_flag: bool,
        fast_hash: Option<bool>,
        imported: Arc<Mutex<SmallSet<ImportPath>>>,
    ) {
        if let Some(ref err) = error {
            self.record_error(err, formatter);
        }

        self.stats.success += 1;

        // if requested, save the imports in the result.output to be printed later
        // and add them to the imported set to be recursively imported later
        if imports_flag {
            if error.is_some() {
                formatter.separator(&mut self.stdout);
            }
            let eval_imports = eval_result.imports();
            formatter.imports(
                &eval_result.buildfile_path().path(),
                eval_imports,
                Some(self.package.dupe()),
                &mut self.stdout,
            );
            imported
                .lock()
                .unwrap()
                .extend(eval_imports.iter().cloned());
        }

        // save the target info in the result.output to be printed later
        for (i, node) in targets.iter().enumerate() {
            self.stats.targets += 1;
            if error.is_some() || imports_flag || i != 0 {
                formatter.separator(&mut self.stdout);
            }
            formatter.target(
                TargetInfo {
                    node: node.as_ref(),
                    target_hash: fast_hash
                        .map(|fast| TargetHashes::compute_immediate_one(node, fast)),
                    super_package: eval_result.super_package(),
                },
                &mut self.stdout,
            )
        }
    }

    fn record_error(&mut self, error: &buck2_error::Error, formatter: &dyn TargetFormatter) {
        self.stats.add_error(error);
        let mut stderr = String::new();
        formatter.package_error(self.package.dupe(), error, &mut self.stdout, &mut stderr);
        self.stderr = Some(stderr);
    }
}

async fn process_package(
    ctx: &mut DiceTransaction,
    formatter: Arc<dyn TargetFormatter>,
    package: PackageLabel,
    spec: PackageSpec<TargetPatternExtra>,
    cached: bool,
    keep_going: bool,
    imports_flag: bool,
    fast_hash: Option<bool>,
    threads: Arc<Semaphore>,
    imported: Arc<Mutex<SmallSet<ImportPath>>>,
) -> PreparePackageResult {
    let mut result = PreparePackageResult::from_package(package.dupe());
    let targets = {
        // This bit of code is the heavy CPU stuff, so guard it with the threads
        let _permit = threads.acquire().await.unwrap();
        load_targets(ctx, package.dupe(), spec, cached, keep_going).await
    };

    match targets {
        Ok((eval_result, targets, err)) => {
            result.append_successful_targets(
                eval_result,
                targets,
                err,
                formatter.as_ref(),
                imports_flag,
                fast_hash,
                imported,
            );
        }
        Err(err) => {
            result.record_error(&err, formatter.as_ref());
        }
    }

    result
}

/// Given the patterns, separate into those which have an explicit package, and those which are recursive
fn stream_packages<'a, T: PatternType>(
    dice: &'a DiceTransaction,
    patterns: Vec<ParsedPattern<T>>,
) -> impl Stream<Item = buck2_error::Result<(PackageLabel, PackageSpec<T>)>> + 'a {
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

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
enum TargetsError {
    #[error(
        "Unknown targets {} from package `{0}`.",
        _1.iter().map(|x| format!("`{}`", x)).join(", ")
    )]
    MissingTargets(PackageLabel, Vec<TargetName>),
}

/// Load the targets from a package. If `keep_going` is specified then it may return a `Some` error in the triple.
async fn load_targets(
    dice: &mut DiceComputations<'_>,
    package: PackageLabel,
    spec: PackageSpec<TargetPatternExtra>,
    cached: bool,
    keep_going: bool,
) -> buck2_error::Result<(
    Arc<EvaluationResult>,
    Vec<TargetNode>,
    Option<buck2_error::Error>,
)> {
    let result = if cached {
        dice.get_interpreter_results(package.dupe()).await?
    } else {
        dice.get_interpreter_results_uncached(package.dupe())
            .await
            .1?
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
                                Some(x) => Either::Right(x.to_owned()),
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
                    buck2_error::Ok(result.resolve_target(target.as_ref())?.to_owned())
                })?;
                Ok((result, targets, None))
            }
        }
        PackageSpec::All => {
            let targets = result.targets().values().map(|t| t.to_owned()).collect();
            Ok((result, targets, None))
        }
    }
}

/// Return `None` if the PACKAGE file doesn't exist
async fn package_imports(
    dice: &mut DiceComputations<'_>,
    path: PackageLabel,
) -> buck2_error::Result<Option<(PackageFilePath, Vec<ImportPath>)>> {
    INTERPRETER_CALCULATION_IMPL
        .get()?
        .get_package_file_deps(dice, path)
        .await
}
