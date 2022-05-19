/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{cell::RefCell, rc::Rc, sync::Arc};

use buck2_core::{package::Package, target::TargetLabel};
use buck2_interpreter::{
    common::{BuildFilePath, ImportPath},
    extra::ExtraContext,
    package_imports::ImplicitImport,
};
use gazebo::prelude::*;
use indexmap::map::Entry;
use starlark::{environment::FrozenModule, values::OwnedFrozenValue};

use crate::{
    interpreter::rule_defs::attr::BuildAttrCoercionContext,
    nodes::unconfigured::{TargetNode, TargetsMap},
};

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
    /// Directly imported modules.
    imports: Vec<ImportPath>,
    recorder: TargetsRecorder,
    package_implicits: Option<PackageImplicits>,
    default_visibility_to_public: bool,
    record_target_call_stacks: bool,
}

impl ExtraContext for ModuleInternals {
    type EvalResult = EvaluationResult;

    fn package(&self) -> &Package {
        self.buildfile_path().package()
    }
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
        self.env.get(self.import_spec.lookup_alias(name))
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
#[derive(Clone, Dupe)]
pub struct TargetsRecorder {
    targets: Rc<RefCell<TargetsMap>>,
}

#[derive(Debug, thiserror::Error)]
enum TargetsError {
    #[error("Attempted to register target {0} twice")]
    RegisteredTargetTwice(TargetLabel),
}

impl TargetsRecorder {
    fn new() -> Self {
        Self {
            targets: Rc::new(RefCell::new(TargetsMap::new())),
        }
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
    fn take(&self) -> TargetsMap {
        self.targets.replace(TargetsMap::new())
    }
}
