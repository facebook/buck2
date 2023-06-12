/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::collections::HashSet;
use std::sync::Arc;

use buck2_common::pattern::package_roots::find_package_roots_stream;
use buck2_common::pattern::resolve::ResolvedPattern;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern_type::PatternType;
use buck2_core::pattern::ParsedPattern;
use buck2_core::target::name::TargetName;
use buck2_events::dispatch::console_message;
use dice::DiceComputations;
use dupe::Dupe;
use fnv::FnvBuildHasher;
use futures::future::BoxFuture;
use futures::stream::FuturesUnordered;
use futures::FutureExt;
use futures::Stream;
use futures::StreamExt;
use itertools::Itertools;

use crate::nodes::eval_result::EvaluationResult;
use crate::nodes::frontend::TargetGraphCalculation;
use crate::nodes::unconfigured::TargetNode;

#[derive(Debug, thiserror::Error)]
enum BuildErrors {
    #[error("Did not find package with name `{0}`.")]
    MissingPackage(PackageLabel),
}

async fn resolve_patterns_and_load_buildfiles<'c, T: PatternType>(
    ctx: &'c DiceComputations,
    parsed_patterns: Vec<ParsedPattern<T>>,
) -> anyhow::Result<(
    ResolvedPattern<T>,
    impl Stream<Item = (PackageLabel, anyhow::Result<Arc<EvaluationResult>>)> + 'c,
)> {
    let mut spec = ResolvedPattern::<T>::new();
    let mut recursive_packages = Vec::new();

    struct Builder<'c> {
        ctx: &'c DiceComputations,
        already_loading: HashSet<PackageLabel, FnvBuildHasher>,
        load_package_futs:
            FuturesUnordered<BoxFuture<'c, (PackageLabel, anyhow::Result<Arc<EvaluationResult>>)>>,
    }

    let mut builder = Builder {
        ctx,
        load_package_futs: FuturesUnordered::new(),
        already_loading: HashSet::default(),
    };

    impl<'c> Builder<'c> {
        fn load_package(&mut self, package: PackageLabel) {
            if !self.already_loading.insert(package.dupe()) {
                return;
            }

            // it's important that this is not async and the temporary spawn happens when the function is called as we don't immediately start polling these.
            self.load_package_futs.push(
                self.ctx
                    .get_interpreter_results(package.dupe())
                    .map(|res| (package, res))
                    .boxed(),
            )
        }
    }

    for pattern in parsed_patterns {
        match pattern {
            ParsedPattern::Target(package, target_name, extra) => {
                spec.add_target(package.dupe(), target_name, extra);
                builder.load_package(package.dupe());
            }
            ParsedPattern::Package(package) => {
                spec.add_package(package.dupe());
                builder.load_package(package.dupe());
            }
            ParsedPattern::Recursive(package) => {
                recursive_packages.push(package);
            }
        }
    }

    let mut recursive_pattern_packages = find_package_roots_stream(ctx, recursive_packages);
    while let Some(res) = recursive_pattern_packages.next().await {
        let package = res?;
        spec.add_package(package.dupe());
        builder.load_package(package);
    }

    Ok((spec, builder.load_package_futs))
}

pub struct LoadedPatterns<T: PatternType> {
    results: BTreeMap<PackageLabel, SharedResult<BTreeMap<(TargetName, T), TargetNode>>>,
}

impl<T: PatternType> LoadedPatterns<T> {
    pub fn iter(
        &self,
    ) -> impl Iterator<
        Item = (
            PackageLabel,
            &SharedResult<BTreeMap<(TargetName, T), TargetNode>>,
        ),
    > {
        self.results.iter().map(|(k, v)| (k.dupe(), v))
    }

    // Implementing IntoIterator requires explicitly specifying the iterator type, which seems higher cost than the value of doing it.
    #[allow(clippy::should_implement_trait)]
    pub fn into_iter(
        self,
    ) -> impl Iterator<
        Item = (
            PackageLabel,
            SharedResult<BTreeMap<(TargetName, T), TargetNode>>,
        ),
    > {
        self.results.into_iter()
    }

    pub fn iter_loaded_targets(&self) -> impl Iterator<Item = SharedResult<&TargetNode>> {
        self.results
            .values()
            .map(|result| match result {
                Ok(label_to_node) => Ok(label_to_node.values()),
                Err(e) => Err(e.dupe()),
            })
            .flatten_ok()
    }

    pub fn iter_loaded_targets_by_package(
        &self,
    ) -> impl Iterator<Item = (PackageLabel, anyhow::Result<Vec<TargetNode>>)> + '_ {
        self.results.iter().map(|(package, result)| {
            let targets = result
                .as_ref()
                .map(|label_to_node| label_to_node.values().map(|t| t.dupe()).collect::<Vec<_>>())
                .map_err(|e| anyhow::Error::new(e.dupe()));
            (package.dupe(), targets)
        })
    }
}

/// Option to skip missing targets instead of failing.
/// This is not a good option to use long term, but we need it now to deal with our legacy setup.
#[derive(Clone, Dupe, Copy, Eq, PartialEq, Debug)]
pub enum MissingTargetBehavior {
    /// Skip missing targets (but error on missing packages or evaluation errors).
    /// When skipping, we emit a warning to the console.
    Warn,
    Fail,
}

impl MissingTargetBehavior {
    pub fn from_skip(skip: bool) -> MissingTargetBehavior {
        if skip {
            MissingTargetBehavior::Warn
        } else {
            MissingTargetBehavior::Fail
        }
    }
}

/// Finds all the requested targets in `spec` from a map of loaded targets in `load_result`.
fn apply_spec<T: PatternType>(
    spec: ResolvedPattern<T>,
    load_results: BTreeMap<PackageLabel, SharedResult<Arc<EvaluationResult>>>,
    skip_missing_targets: MissingTargetBehavior,
) -> anyhow::Result<LoadedPatterns<T>> {
    let mut all_targets: BTreeMap<_, SharedResult<BTreeMap<(TargetName, T), _>>> = BTreeMap::new();
    for (pkg, pkg_spec) in spec.specs.into_iter() {
        let result = match load_results.get(&pkg) {
            Some(r) => r,
            None => return Err(anyhow::anyhow!(BuildErrors::MissingPackage(pkg))),
        };
        match result {
            Ok(res) => {
                let (label_to_node, missing) = res.apply_spec(pkg_spec);
                if let Some(missing) = missing {
                    match skip_missing_targets {
                        MissingTargetBehavior::Fail => return Err(missing.into_error()),
                        MissingTargetBehavior::Warn => {
                            console_message(missing.missing_targets_warning())
                        }
                    }
                };
                all_targets.insert(pkg, Ok(label_to_node));
            }
            Err(e) => {
                all_targets.insert(pkg, Err(e.dupe()));
            }
        }
    }

    Ok(LoadedPatterns {
        results: all_targets,
    })
}

pub async fn load_patterns<T: PatternType>(
    ctx: &DiceComputations,
    parsed_patterns: Vec<ParsedPattern<T>>,
    skip_missing_targets: MissingTargetBehavior,
) -> anyhow::Result<LoadedPatterns<T>> {
    let (spec, mut load_package_futs) =
        resolve_patterns_and_load_buildfiles(ctx, parsed_patterns).await?;

    let mut results: BTreeMap<PackageLabel, SharedResult<Arc<EvaluationResult>>> = BTreeMap::new();
    while let Some((pkg, load_res)) = load_package_futs.next().await {
        results.insert(pkg, load_res.shared_error());
    }

    apply_spec(spec, results, skip_missing_targets)
}
