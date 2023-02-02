/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::cell::RefMut;
use std::mem;
use std::sync::Arc;

use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::target::name::TargetNameRef;
use buck2_interpreter::extra::ExtraContext;
use buck2_interpreter::package_imports::ImplicitImport;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::targets_map::TargetsMap;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::package::Package;
use dupe::Dupe;
use starlark::environment::FrozenModule;
use starlark::values::OwnedFrozenValue;

use crate::attrs::coerce::ctx::BuildAttrCoercionContext;

impl From<ModuleInternals> for EvaluationResult {
    // TODO(cjhopman): Let's make this an `into_evaluation_result()` on ModuleInternals instead.
    fn from(internals: ModuleInternals) -> Self {
        let ModuleInternals {
            state,
            imports,
            buildfile_path,
            ..
        } = internals;
        let recorder = match state.into_inner() {
            State::BeforeTargets(_) => TargetsRecorder::new(),
            State::Targets(RecordingTargets { recorder, .. }) => recorder,
        };
        EvaluationResult::new(buildfile_path, imports, recorder.take())
    }
}

struct Oncall(Arc<String>);

struct RecordingTargets {
    package: Arc<Package>,
    recorder: TargetsRecorder,
}

enum State {
    /// No targets recorded yet, `oncall` call is allowed unless it was already called.
    BeforeTargets(Option<Oncall>),
    /// First target seen.
    Targets(RecordingTargets),
}

/// ModuleInternals contains the module/package-specific information for
/// evaluating build files. Built-in functions that need access to
/// package-specific information or objects can get them by acquiring the
/// ModuleInternals.
pub struct ModuleInternals {
    attr_coercion_context: BuildAttrCoercionContext,
    buildfile_path: Arc<BuildFilePath>,
    /// Have you seen an oncall annotation yet
    state: RefCell<State>,
    /// Directly imported modules.
    imports: Vec<ImportPath>,
    package_implicits: Option<PackageImplicits>,
    default_visibility_to_public: bool,
    record_target_call_stacks: bool,
}

impl ExtraContext for ModuleInternals {
    type EvalResult = EvaluationResult;
}

pub(crate) struct PackageImplicits {
    import_spec: Arc<ImplicitImport>,
    env: FrozenModule,
}

impl PackageImplicits {
    pub(crate) fn new(import_spec: Arc<ImplicitImport>, env: FrozenModule) -> Self {
        Self { import_spec, env }
    }

    fn lookup(&self, name: &str) -> Option<OwnedFrozenValue> {
        self.env
            .get_option(self.import_spec.lookup_alias(name))
            .ok()
            .flatten()
    }
}

#[derive(Debug, thiserror::Error)]
enum OncallErrors {
    #[error("Called `oncall` after one or more targets were declared, `oncall` must be first.")]
    OncallAfterTargets,
    #[error("Called `oncall` more than once in the file.")]
    DuplicateOncall,
}

impl ModuleInternals {
    pub(crate) fn new(
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
            state: RefCell::new(State::BeforeTargets(None)),
            imports,
            package_implicits,
            default_visibility_to_public,
            record_target_call_stacks,
        }
    }

    pub(crate) fn attr_coercion_context(&self) -> &BuildAttrCoercionContext {
        &self.attr_coercion_context
    }

    pub fn record(&self, target_node: TargetNode) -> anyhow::Result<()> {
        self.recording_targets().recorder.record(target_node)
    }

    pub(crate) fn set_oncall(&self, name: &str) -> anyhow::Result<()> {
        match &mut *self.state.borrow_mut() {
            State::BeforeTargets(Some(_)) => Err(OncallErrors::DuplicateOncall.into()),
            State::BeforeTargets(oncall) => {
                assert!(oncall.is_none());
                *oncall = Some(Oncall(Arc::new(name.to_owned())));
                Ok(())
            }
            State::Targets(..) => {
                // We require oncall to be first both so users can find it,
                // and so we can propagate it to all targets more easily.
                Err(OncallErrors::OncallAfterTargets.into())
            }
        }
    }

    fn recording_targets(&self) -> RefMut<RecordingTargets> {
        RefMut::map(self.state.borrow_mut(), |state| {
            loop {
                match state {
                    State::BeforeTargets(oncall) => {
                        let oncall = mem::take(oncall).map(|Oncall(name)| name);
                        *state = State::Targets(RecordingTargets {
                            package: Arc::new(Package {
                                buildfile_path: self.buildfile_path.dupe(),
                                oncall,
                                default_visibility_to_public: self.default_visibility_to_public,
                            }),
                            recorder: TargetsRecorder::new(),
                        });
                        continue;
                    }
                    State::Targets(r) => return r,
                }
            }
        })
    }

    pub(crate) fn target_exists(&self, name: &str) -> bool {
        self.recording_targets()
            .recorder
            .targets
            .contains_key(TargetNameRef::unchecked_new(name))
    }

    pub fn buildfile_path(&self) -> &Arc<BuildFilePath> {
        &self.buildfile_path
    }

    pub fn package(&self) -> Arc<Package> {
        self.recording_targets().package.dupe()
    }

    pub(crate) fn get_package_implicit(&self, name: &str) -> Option<OwnedFrozenValue> {
        self.package_implicits
            .as_ref()
            .and_then(|implicits| implicits.lookup(name))
    }

    pub fn record_target_call_stacks(&self) -> bool {
        self.record_target_call_stacks
    }
}

// Records the targets declared when evaluating a build file.
struct TargetsRecorder {
    targets: TargetsMap,
}

impl TargetsRecorder {
    fn new() -> Self {
        Self {
            targets: TargetsMap::new(),
        }
    }

    fn record(&mut self, target_node: TargetNode) -> anyhow::Result<()> {
        self.targets.record(target_node)
    }

    fn take(self) -> TargetsMap {
        self.targets
    }
}
