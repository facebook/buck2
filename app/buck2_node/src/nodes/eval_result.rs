/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::fmt::Display;
use std::fmt::Write;
use std::sync::Arc;

use allocative::Allocative;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::pattern_type::PatternType;
use buck2_core::pattern::PackageSpec;
use buck2_core::target::label::TargetLabel;
use buck2_core::target::name::TargetName;
use buck2_core::target::name::TargetNameRef;
use dupe::Dupe;
use dupe::IterDupedExt;
use gazebo::prelude::*;
use itertools::Itertools;

use crate::nodes::targets_map::TargetsMap;
use crate::nodes::unconfigured::TargetNode;

#[derive(Debug, thiserror::Error)]
enum EvalulationResultError {
    #[error(
        "Unknown target `{target}` from package `{package}`.\n\
Did you mean one of the {num_targets} targets in {buildfile_path}?{similar_targets}"
    )]
    UnknownTarget {
        target: TargetName,
        package: PackageLabel,
        num_targets: usize,
        buildfile_path: Arc<BuildFilePath>,
        similar_targets: SuggestedSimilarTargets,
    },
    #[error("Zero missing targets (internal error)")]
    ZeroMissingTargets,
}

#[derive(Debug)]
pub struct MissingTargets {
    missing_targets: Vec<TargetLabel>,
    package: PackageLabel,
    num_targets: usize,
    buildfile_path: Arc<BuildFilePath>,
    all_target_labels: Vec<TargetLabel>,
}

impl MissingTargets {
    /// Error message emitted when missing targets are not skipped.
    pub fn into_error(mut self) -> anyhow::Error {
        if self.missing_targets.is_empty() {
            return EvalulationResultError::ZeroMissingTargets.into();
        }
        let target = self.missing_targets.swap_remove(0);
        let similar_targets = SuggestedSimilarTargets::suggest(
            target.name(),
            self.package.dupe(),
            self.all_target_labels.iter().map(|x| x.name()),
        );
        EvalulationResultError::UnknownTarget {
            target: target.name().to_owned(),
            package: self.package,
            num_targets: self.num_targets,
            buildfile_path: self.buildfile_path,
            similar_targets,
        }
        .into()
    }

    fn gen_missing_target_warning(mut missing_targets: Vec<TargetLabel>) -> String {
        missing_targets.sort_unstable();
        let (head, middle, tail): (&[TargetLabel], &str, &[TargetLabel]) =
            if missing_targets.len() > 15 {
                let head = &missing_targets[..5];
                let tail = &missing_targets[missing_targets.len() - 5..];
                (head, "...", tail)
            } else {
                (&missing_targets, "", &[])
            };
        let mut message = String::new();
        writeln!(
            message,
            "Skipped {} missing targets:",
            missing_targets.len()
        )
        .unwrap();
        for target in head {
            writeln!(message, "  {}", target).unwrap();
        }
        if !middle.is_empty() {
            writeln!(message, "  {}", middle).unwrap();
        }
        for target in tail {
            writeln!(message, "  {}", target).unwrap();
        }
        message
    }

    /// Warning message emitted when missing targets are skipped.
    pub fn missing_targets_warning(self) -> String {
        Self::gen_missing_target_warning(self.missing_targets)
    }
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

    pub fn package(&self) -> PackageLabel {
        self.buildfile_path.package()
    }

    pub fn targets(&self) -> &TargetsMap {
        &self.targets
    }

    pub fn imports(&self) -> &[ImportPath] {
        &self.imports
    }

    pub fn get_target<'a>(&'a self, name: &TargetNameRef) -> Option<&'a TargetNode> {
        self.targets.get(name)
    }

    pub fn resolve_target<'a>(&'a self, path: &TargetNameRef) -> anyhow::Result<&'a TargetNode> {
        self.get_target(path).ok_or_else(|| {
            EvalulationResultError::UnknownTarget {
                target: path.to_owned(),
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

    pub fn apply_spec<T: PatternType>(
        &self,
        spec: PackageSpec<T>,
    ) -> (
        BTreeMap<(TargetName, T), TargetNode>,
        Option<MissingTargets>,
    ) {
        match spec {
            PackageSpec::All => {
                let mut label_to_node = BTreeMap::new();
                for target_info in self.targets().values() {
                    label_to_node.insert(
                        (target_info.label().name().to_owned(), T::default()),
                        target_info.dupe(),
                    );
                }
                (label_to_node, None)
            }
            PackageSpec::Targets(targets) => {
                let mut label_to_node = BTreeMap::new();
                let mut missing_targets = Vec::new();
                for (target_name, extra) in targets {
                    let node = self.get_target(target_name.as_ref());
                    match node {
                        Some(node) => {
                            label_to_node.insert((target_name, extra), node.dupe());
                        }
                        None => missing_targets
                            .push(TargetLabel::new(self.package(), target_name.as_ref())),
                    }
                }
                let missing_targets = if missing_targets.is_empty() {
                    None
                } else {
                    Some(MissingTargets {
                        missing_targets,
                        package: self.package(),
                        num_targets: self.targets.len(),
                        buildfile_path: self.buildfile_path().dupe(),
                        all_target_labels: self.targets.key_target_labels().duped().collect(),
                    })
                };
                (label_to_node, missing_targets)
            }
        }
    }
}

#[derive(Debug)]
struct SuggestedSimilarTargets {
    package: PackageLabel,
    targets: Vec<TargetName>,
}

impl SuggestedSimilarTargets {
    fn suggest<'a>(
        target: &TargetNameRef,
        package: PackageLabel,
        available_targets: impl Iterator<Item = &'a TargetNameRef>,
    ) -> Self {
        const MAX_RESULTS: usize = 10;
        const MAX_LEVENSHTEIN_DISTANCE: usize = 5;
        let targets: Vec<TargetName> = available_targets
            .map(|t| (t, strsim::levenshtein(target.as_str(), t.as_str())))
            .filter(|(t, lev)| {
                lev <= &MAX_LEVENSHTEIN_DISTANCE
                    || target.as_str().starts_with(t.as_str())
                    || t.as_str().starts_with(target.as_str())
            })
            .sorted_by_key(|(_, lev)| *lev)
            .take(MAX_RESULTS)
            .map(|(v, _lev)| v.to_owned())
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

#[cfg(test)]
mod tests {
    use buck2_core::target::label::TargetLabel;

    use crate::nodes::eval_result::MissingTargets;

    #[test]
    fn test_missing_targets_message() {
        let targets: Vec<TargetLabel> = (0..30)
            .map(|i| TargetLabel::testing_parse(&format!("aaa//bbb:{i:02}")))
            .collect();
        assert_eq!(
            "Skipped 1 missing targets:\n  aaa//bbb:00\n",
            MissingTargets::gen_missing_target_warning(targets[..1].to_vec()),
        );
        assert_eq!(
            "Skipped 20 missing targets:\
                \n  aaa//bbb:00\n  aaa//bbb:01\n  aaa//bbb:02\n  aaa//bbb:03\n  aaa//bbb:04\
                \n  ...\
                \n  aaa//bbb:15\n  aaa//bbb:16\n  aaa//bbb:17\n  aaa//bbb:18\n  aaa//bbb:19\
                \n",
            MissingTargets::gen_missing_target_warning(targets[..20].to_vec()),
        );

        for i in 0..targets.len() {
            // Test it does not panic.
            let _ignore = MissingTargets::gen_missing_target_warning(targets[..i].to_vec());
        }
    }
}
