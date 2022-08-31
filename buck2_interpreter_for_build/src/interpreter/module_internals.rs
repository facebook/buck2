/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::Cell;
use std::cell::RefCell;
use std::fmt::Display;
use std::sync::Arc;

use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::package::Package;
use buck2_core::target::TargetLabel;
use buck2_core::target::TargetName;
use buck2_interpreter::extra::ExtraContext;
use buck2_interpreter::package_imports::ImplicitImport;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::nodes::unconfigured::TargetsMap;
use gazebo::prelude::*;
use indexmap::map::Entry;
use itertools::Itertools;
use starlark::environment::FrozenModule;
use starlark::values::OwnedFrozenValue;

use crate::attrs::coerce::ctx::BuildAttrCoercionContext;

/// An EvaluationResult contains the list of targets resulting from evaluating a build file.
#[derive(Debug)]
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
            TargetsError::UnknownTarget {
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

impl From<ModuleInternals> for EvaluationResult {
    // TODO(cjhopman): Let's make this an `into_evaluation_result()` on ModuleInternals instead.
    fn from(internals: ModuleInternals) -> Self {
        let ModuleInternals {
            recorder,
            buildfile_path,
            imports,
            ..
        } = internals;
        EvaluationResult::new(buildfile_path, imports, recorder.take())
    }
}

/// ModuleInternals contains the module/package-specific information for
/// evaluating build files. Built-in functions that need access to
/// package-specific information or objects can get them by acquiring the
/// ModuleInternals.
pub struct ModuleInternals {
    attr_coercion_context: BuildAttrCoercionContext,
    buildfile_path: Arc<BuildFilePath>,
    /// Have you seen an oncall annotation yet
    seen_oncall: Cell<bool>,
    /// Directly imported modules.
    imports: Vec<ImportPath>,
    recorder: TargetsRecorder,
    package_implicits: Option<PackageImplicits>,
    default_visibility_to_public: bool,
    record_target_call_stacks: bool,
}

impl ExtraContext for ModuleInternals {
    type EvalResult = EvaluationResult;
}

pub struct PackageImplicits {
    import_spec: Arc<ImplicitImport>,
    env: FrozenModule,
}

impl PackageImplicits {
    pub fn new(import_spec: Arc<ImplicitImport>, env: FrozenModule) -> Self {
        Self { import_spec, env }
    }

    fn lookup(&self, name: &str) -> Option<OwnedFrozenValue> {
        self.env.get(self.import_spec.lookup_alias(name)).ok()
    }
}

impl ModuleInternals {
    pub fn new(
        attr_coercion_context: BuildAttrCoercionContext,
        buildfile_path: Arc<BuildFilePath>,
        imports: Vec<ImportPath>,
        package_implicits: Option<PackageImplicits>,
        default_visibility_to_public: bool,
        record_target_call_stacks: bool,
    ) -> Self {
        Self {
            attr_coercion_context,
            buildfile_path,
            seen_oncall: Cell::new(false),
            imports,
            package_implicits,
            recorder: TargetsRecorder::new(),
            default_visibility_to_public,
            record_target_call_stacks,
        }
    }

    pub fn attr_coercion_context(&self) -> &BuildAttrCoercionContext {
        &self.attr_coercion_context
    }

    pub fn recorder(&self) -> &TargetsRecorder {
        &self.recorder
    }

    pub fn has_seen_oncall(&self) -> bool {
        self.seen_oncall.get()
    }

    pub fn set_seen_oncall(&self) {
        self.seen_oncall.set(true)
    }

    pub fn target_exists(&self, name: &str) -> bool {
        (*self.recorder.targets.borrow()).contains_key(name)
    }

    pub fn buildfile_path(&self) -> &Arc<BuildFilePath> {
        &self.buildfile_path
    }

    pub fn get_package_implicit(&self, name: &str) -> Option<OwnedFrozenValue> {
        self.package_implicits
            .as_ref()
            .and_then(|implicits| implicits.lookup(name))
    }

    pub fn default_visibility_to_public(&self) -> bool {
        self.default_visibility_to_public
    }

    pub fn record_target_call_stacks(&self) -> bool {
        self.record_target_call_stacks
    }
}

// Records the targets declared when evaluating a build file.
pub struct TargetsRecorder {
    targets: RefCell<TargetsMap>,
}

#[derive(Debug, thiserror::Error)]
enum TargetsError {
    #[error("Attempted to register target {0} twice")]
    RegisteredTargetTwice(TargetLabel),
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

impl TargetsRecorder {
    fn new() -> Self {
        Self {
            targets: RefCell::new(TargetsMap::new()),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.targets.borrow().is_empty()
    }

    pub fn record(&self, target_node: TargetNode) -> anyhow::Result<()> {
        let mut rules = self.targets.borrow_mut();
        match rules.entry(target_node.label().name().dupe()) {
            Entry::Vacant(o) => {
                o.insert(target_node);
                Ok(())
            }
            Entry::Occupied(_) => {
                Err(TargetsError::RegisteredTargetTwice(target_node.label().dupe()).into())
            }
        }
    }

    /// Takes the recorded TargetsMap, resetting it to empty.
    fn take(self) -> TargetsMap {
        self.targets.into_inner()
    }
}

#[derive(Debug)]
pub struct SuggestedSimilarTargets {
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
