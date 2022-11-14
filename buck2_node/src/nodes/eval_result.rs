/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::package::Package;
use buck2_core::target::TargetName;
use gazebo::prelude::*;
use itertools::Itertools;

use crate::nodes::unconfigured::TargetNode;
use crate::nodes::unconfigured::TargetsMap;

#[derive(Debug, thiserror::Error)]
enum EvalulationResultError {
    #[error(
        "Unknown target `{target}` from package `{package}`.\n\
Did you mean one of the {num_targets} targets in {buildfile_path}?{similar_targets}"
    )]
    UnknownTarget {
        target: TargetName,
        package: Package,
        num_targets: usize,
        buildfile_path: Arc<BuildFilePath>,
        similar_targets: SuggestedSimilarTargets,
    },
}

/// An EvaluationResult contains the list of targets resulting from evaluating a build file.
#[derive(Debug, Allocative)]
pub struct EvaluationResult {
    /// The buildfile path that corresponds to this result.
    /// unlike a .bzl file, a build file (BUCK, TARGETS, etc) will only be loaded in
    /// its own cell, so we don't need a full ImportPath here.
    buildfile_path: Arc<BuildFilePath>,
    imports: Vec<ImportPath>,
    targets: TargetsMap,
}

impl EvaluationResult {
    pub fn new(
        buildfile_path: Arc<BuildFilePath>,
        imports: Vec<ImportPath>,
        targets: TargetsMap,
    ) -> Self {
        Self {
            buildfile_path,
            imports,
            targets,
        }
    }

    pub fn buildfile_path(&self) -> &Arc<BuildFilePath> {
        &self.buildfile_path
    }

    pub fn package(&self) -> &Package {
        self.buildfile_path.package()
    }

    pub fn targets(&self) -> &TargetsMap {
        &self.targets
    }

    pub fn imports(&self) -> impl Iterator<Item = &ImportPath> + Clone {
        self.imports.iter()
    }

    pub fn resolve_target<'a>(&'a self, path: &TargetName) -> anyhow::Result<&'a TargetNode> {
        self.targets.get(path).ok_or_else(|| {
            EvalulationResultError::UnknownTarget {
                target: path.dupe(),
                package: self.package().dupe(),
                num_targets: self.targets.len(),
                buildfile_path: self.buildfile_path.dupe(),
                similar_targets: SuggestedSimilarTargets::suggest(
                    path,
                    self.package().dupe(),
                    self.targets.keys(),
                ),
            }
            .into()
        })
    }
}

#[derive(Debug)]
struct SuggestedSimilarTargets {
    package: Package,
    targets: Vec<TargetName>,
}

impl SuggestedSimilarTargets {
    pub fn suggest<'a>(
        target: &TargetName,
        package: Package,
        available_targets: impl Iterator<Item = &'a TargetName>,
    ) -> Self {
        const MAX_RESULTS: usize = 10;
        const MAX_LEVENSHTEIN_DISTANCE: usize = 5;
        let targets: Vec<TargetName> = available_targets
            .map(|t| (t, strsim::levenshtein(target.value(), t.value())))
            .filter(|(t, lev)| {
                lev <= &MAX_LEVENSHTEIN_DISTANCE
                    || target.value().starts_with(t.value())
                    || t.value().starts_with(target.value())
            })
            .sorted_by_key(|(_, lev)| *lev)
            .take(MAX_RESULTS)
            .map(|(v, _lev)| v.dupe())
            .collect();
        Self { package, targets }
    }
}

impl Display for SuggestedSimilarTargets {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.targets.is_empty() {
            let targets: Vec<String> = self
                .targets
                .map(|target| format!("  {}:{}", self.package, target));
            // Add a leading newline because this is used as a suffix in TargetsError.
            // For the same reason, print nothing when self.targets is empty.
            write!(
                f,
                "\nMaybe you meant one of these similar targets?\n{}",
                targets.join("\n")
            )?;
        }
        Ok(())
    }
}
