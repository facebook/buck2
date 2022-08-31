/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::fmt::Debug;
use std::sync::Arc;

use buck2_core::fs::paths::ForwardRelativePath;
use buck2_execute::base_deferred_key::BaseDeferredKey;
use buck2_execute::path::buck_out_path::BuckOutPath;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use derivative::Derivative;
use gazebo::prelude::*;
use indexmap::IndexSet;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::OwnedFrozenValue;
use starlark::values::Trace;
use starlark::values::Tracer;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueError;
use thiserror::Error;

use crate::actions::artifact::Artifact;
use crate::actions::artifact::DeclaredArtifact;
use crate::actions::artifact::OutputArtifact;
use crate::actions::ActionsRegistry;
use crate::actions::UnregisteredAction;
use crate::artifact_groups::registry::ArtifactGroupRegistry;
use crate::artifact_groups::ArtifactGroup;
use crate::deferred::types::BaseKey;
use crate::deferred::types::DeferredId;
use crate::deferred::types::DeferredRegistry;
use crate::dynamic::registry::DynamicRegistry;
use crate::interpreter::rule_defs::artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::artifact::StarlarkOutputArtifact;
use crate::interpreter::rule_defs::artifact::ValueAsArtifactLike;

#[derive(Derivative, Trace)]
#[derivative(Debug)]
pub struct AnalysisRegistry<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    deferred: DeferredRegistry,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    actions: ActionsRegistry,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    artifact_groups: ArtifactGroupRegistry,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    dynamic: DynamicRegistry,
    analysis_value_storage: AnalysisValueStorage<'v>,
}

#[derive(Error, Debug)]
enum DeclaredArtifactError {
    #[error("Can't declare an artifact with an empty filename component")]
    DeclaredEmptyFileName,
}

impl<'v> AnalysisRegistry<'v> {
    pub fn new_from_owner(
        owner: BaseDeferredKey,
        execution_platform: ExecutionPlatformResolution,
    ) -> Self {
        Self::new_from_owner_and_deferred(
            owner.dupe(),
            execution_platform,
            DeferredRegistry::new(BaseKey::Base(owner)),
        )
    }

    pub(crate) fn new_from_owner_and_deferred(
        owner: BaseDeferredKey,
        execution_platform: ExecutionPlatformResolution,
        deferred: DeferredRegistry,
    ) -> Self {
        AnalysisRegistry {
            deferred,
            actions: ActionsRegistry::new(owner.dupe(), execution_platform),
            artifact_groups: ArtifactGroupRegistry::new(),
            dynamic: DynamicRegistry::new(owner),
            analysis_value_storage: AnalysisValueStorage::new(),
        }
    }

    pub(crate) fn set_action_key(&mut self, action_key: Arc<str>) {
        self.actions.set_action_key(action_key);
    }

    /// Reserves a path in an output directory. Doesn't declare artifact,
    /// but checks that there is no previously declared artifact with a path
    /// which is in conflict with claimed `path`.
    pub(crate) fn claim_output_path(&mut self, path: &ForwardRelativePath) -> anyhow::Result<()> {
        self.actions.claim_output_path(path)
    }

    pub(crate) fn declare_dynamic_output(&mut self, path: BuckOutPath) -> DeclaredArtifact {
        self.actions.declare_dynamic_output(path)
    }

    pub(crate) fn declare_output(
        &mut self,
        prefix: Option<&str>,
        filename: &str,
    ) -> anyhow::Result<DeclaredArtifact> {
        // We want this artifact to be a file/directory inside the current context, which means
        // things like `..` and the empty path `.` can be bad ideas. The `::new` method checks for those
        // things and fails if they are present.

        if filename == "." || filename == "" {
            return Err(DeclaredArtifactError::DeclaredEmptyFileName.into());
        }

        let path = ForwardRelativePath::new(filename)?.to_owned();
        let prefix = match prefix {
            None => None,
            Some(x) => Some(ForwardRelativePath::new(x)?.to_owned()),
        };
        self.actions.declare_artifact(prefix, path)
    }

    /// Takes a string or artifact/output artifact and converts it into an output artifact
    ///
    /// This is handy for functions like `ctx.actions.write` where it's nice to just let
    /// the user give us a string if they want as the output name.
    ///
    /// The valid types for `value` and subsequent actions are as follows:
    ///  - `str`: a new file is declared with this name. The `StarlarkDeclaredArtifact` and
    ///         its `OutputArtifact` are returned
    ///  - `StarlarkArtifact`/`StarlarkDeclaredArtifact`: If the artifact has not been bound yet,
    ///         the original value and its `OutputArtifact` will be returned. If the artifact
    ///         has been bound, an error is returned
    ///  - `StarlarkOutputArtifact`: The artifact as a `StarlarkDeclaredArtifact` and its `OutputArtifact` are returned
    pub(crate) fn get_or_declare_output<'v2>(
        &mut self,
        eval: &Evaluator<'v2, '_>,
        value: Value<'v2>,
        param_name: &str,
    ) -> anyhow::Result<(Value<'v2>, OutputArtifact)> {
        if let Some(dest_str) = value.unpack_str() {
            let artifact = self.declare_output(None, dest_str)?;
            let output_artifact = artifact.as_output();
            let output_value = eval.heap().alloc(StarlarkDeclaredArtifact::new(
                eval.call_stack_top_location(),
                artifact,
            ));

            Ok((output_value, output_artifact))
        } else if let Some(output) = StarlarkOutputArtifact::unpack_value(value) {
            let output_artifact = output.artifact();
            let output_value = eval.heap().alloc(StarlarkDeclaredArtifact::new(
                eval.call_stack_top_location(),
                (*output_artifact).dupe(),
            ));
            Ok((output_value, output_artifact))
        } else if let Some(a) = value.as_artifact() {
            Ok((value, a.output_artifact()?))
        } else {
            Err(ValueError::IncorrectParameterTypeNamed(param_name.to_owned()).into())
        }
    }

    pub(crate) fn register_action<A: UnregisteredAction + 'static>(
        &mut self,
        inputs: IndexSet<ArtifactGroup>,
        outputs: IndexSet<OutputArtifact>,
        action: A,
        associated_value: Option<Value<'v>>,
    ) -> anyhow::Result<()> {
        let id = self
            .actions
            .register(&mut self.deferred, inputs, outputs, action)?;
        if let Some(value) = associated_value {
            self.analysis_value_storage.set_value(id, value);
        }
        Ok(())
    }

    pub(crate) fn create_transitive_set(
        &mut self,
        definition: Value<'v>,
        value: Option<Value<'v>>,
        children: Option<Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let set = self.artifact_groups.create_transitive_set(
            definition,
            value,
            children,
            &mut self.deferred,
            eval,
        )?;

        let key = set.key().deferred_key().id();
        let set = eval.heap().alloc_complex(set);

        self.analysis_value_storage.set_value(key, set);

        Ok(set)
    }

    pub(crate) fn register_dynamic_output(
        &mut self,
        dynamic: IndexSet<Artifact>,
        inputs: IndexSet<Artifact>,
        outputs: IndexSet<OutputArtifact>,
        attributes_lambda: Value<'v>,
    ) -> anyhow::Result<()> {
        let id = self
            .dynamic
            .register(dynamic, inputs, outputs, &mut self.deferred)?;
        self.analysis_value_storage.set_value(id, attributes_lambda);
        Ok(())
    }

    /// You MUST pass the same module to both the first function and the second one.
    /// It requires both to get the lifetimes to line up.
    pub fn finalize(
        self,
        env: &'v Module,
    ) -> impl FnOnce(Module) -> anyhow::Result<(FrozenModule, DeferredRegistry)> {
        let AnalysisRegistry {
            mut deferred,
            dynamic,
            actions,
            artifact_groups,
            analysis_value_storage,
        } = self;
        analysis_value_storage.write_to_module(env);
        move |env| {
            let frozen_env = env.freeze()?;
            let analysis_value_fetcher = AnalysisValueFetcher {
                frozen_module: Some(frozen_env.dupe()),
            };
            actions.ensure_bound(&mut deferred, &analysis_value_fetcher)?;
            artifact_groups.ensure_bound(&mut deferred, &analysis_value_fetcher)?;
            dynamic.ensure_bound(&mut deferred, &analysis_value_fetcher)?;
            Ok((frozen_env, deferred))
        }
    }
}

/// Store `Value<'v>` values for actions registered in an implementation function
///
/// Threading lifetimes through the various action registries is kind of a pain. So instead,
/// store the starlark values in this struct, using the `DeferredId` as the key.
///
/// These values eventually are written into the mutable `Module`, and a wrapper is
/// made available to get the `OwnedFrozenValue` back out after that `Module` is frozen.
///
/// Note that this object has internal mutation and is only expected to live for the duration
/// of impl function execution.
///
/// At the end of impl function execution, `write_to_module` should be called to ensure
/// that the values are written the top level of the `Module`.
#[derive(Debug)]
struct AnalysisValueStorage<'v> {
    values: HashMap<DeferredId, Value<'v>>,
}

unsafe impl<'v> Trace<'v> for AnalysisValueStorage<'v> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        for v in self.values.values_mut() {
            tracer.trace(v)
        }
    }
}

/// Simple fetcher that fetches the values written in `AnalysisValueStorage::write_to_module`
///
/// These values are pulled from the `FrozenModule` that results from `env.freeze()`.
/// This is used by the action registry to make an `OwnedFrozenValue` available to
/// Actions' register function.
#[derive(Default)]
pub(crate) struct AnalysisValueFetcher {
    frozen_module: Option<FrozenModule>,
}

impl<'v> AnalysisValueStorage<'v> {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    /// Write all of the values to `module` using an internal name
    fn write_to_module(&self, module: &'v Module) {
        for (id, v) in self.values.iter() {
            let starlark_key = format!("$action_key_{}", id);
            module.set(&starlark_key, *v);
        }
    }

    /// Add a value to the internal hash map that maps ids -> values
    fn set_value(&mut self, id: DeferredId, value: Value<'v>) {
        self.values.insert(id, value);
    }
}

impl AnalysisValueFetcher {
    /// Get the `OwnedFrozenValue` that corresponds to a `DeferredId`, if present
    pub(crate) fn get(&self, id: DeferredId) -> Option<OwnedFrozenValue> {
        let module = self.frozen_module.as_ref()?;
        let starlark_key = format!("$action_key_{}", id);
        module.get(&starlark_key).ok()
    }
}
