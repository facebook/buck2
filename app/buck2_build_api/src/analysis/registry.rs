/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cell::OnceCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt::Debug;
use std::marker::PhantomData;
use std::sync::Arc;

use allocative::Allocative;
use buck2_artifact::actions::key::ActionIndex;
use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::artifact::artifact_type::DeclaredArtifact;
use buck2_artifact::artifact::artifact_type::OutputArtifact;
use buck2_artifact::artifact::build_artifact::BuildArtifact;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::fs::buck_out_path::BuckOutPathKind;
use buck2_error::BuckErrorContext;
use buck2_error::internal_error;
use buck2_execute::execute::request::OutputType;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use derivative::Derivative;
use dupe::Dupe;
use indexmap::IndexSet;
use itertools::Itertools;
use starlark::any::ProvidesStaticType;
use starlark::codemap::FileSpan;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::DynStarlark;
use starlark::values::Freeze;
use starlark::values::FreezeError;
use starlark::values::FreezeResult;
use starlark::values::Freezer;
use starlark::values::FrozenHeap;
use starlark::values::FrozenHeapRef;
use starlark::values::FrozenValue;
use starlark::values::FrozenValueTyped;
use starlark::values::Heap;
use starlark::values::OwnedFrozenValue;
use starlark::values::OwnedFrozenValueTyped;
use starlark::values::OwnedRefFrozenRef;
use starlark::values::Trace;
use starlark::values::Tracer;
use starlark::values::Value;
use starlark::values::ValueTyped;
use starlark::values::ValueTypedComplex;
use starlark::values::any_complex::StarlarkAnyComplex;
use starlark::values::typing::FrozenStarlarkCallable;
use starlark::values::typing::StarlarkCallable;
use starlark_map::small_map::SmallMap;

use crate::actions::RegisteredAction;
use crate::actions::UnregisteredAction;
use crate::actions::registry::ActionsRegistry;
use crate::actions::registry::RecordedActions;
use crate::analysis::anon_promises_dyn::AnonPromisesDyn;
use crate::analysis::anon_targets_registry::ANON_TARGET_REGISTRY_NEW;
use crate::analysis::anon_targets_registry::AnonTargetsRegistryDyn;
use crate::analysis::extra_v::AnalysisExtraValue;
use crate::analysis::extra_v::FrozenAnalysisExtraValue;
use crate::artifact_groups::deferred::TransitiveSetIndex;
use crate::artifact_groups::deferred::TransitiveSetKey;
use crate::artifact_groups::promise::PromiseArtifact;
use crate::artifact_groups::promise::PromiseArtifactId;
use crate::deferred::calculation::ActionLookup;
use crate::dynamic::storage::DYNAMIC_LAMBDA_PARAMS_STORAGES;
use crate::dynamic::storage::DynamicLambdaParamsStorage;
use crate::dynamic::storage::FrozenDynamicLambdaParamsStorage;
use crate::interpreter::rule_defs::artifact::associated::AssociatedArtifacts;
use crate::interpreter::rule_defs::artifact::output_artifact_like::OutputArtifactArg;
use crate::interpreter::rule_defs::artifact::starlark_declared_artifact::StarlarkDeclaredArtifact;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValueRef;
use crate::interpreter::rule_defs::provider::collection::ProviderCollection;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSet;
use crate::interpreter::rule_defs::transitive_set::FrozenTransitiveSetDefinition;
use crate::interpreter::rule_defs::transitive_set::TransitiveSet;

#[derive(Derivative, Trace, Allocative)]
#[derivative(Debug)]
pub struct AnalysisRegistry<'v> {
    #[derivative(Debug = "ignore")]
    pub actions: ActionsRegistry<'v>,
    pub anon_targets: Box<DynStarlark<'v, dyn AnonTargetsRegistryDyn<'v>>>,
    pub analysis_value_storage: AnalysisValueStorage<'v>,
    pub short_path_assertions: HashMap<PromiseArtifactId, ForwardRelativePathBuf>,
    pub content_based_path_assertions: HashSet<PromiseArtifactId>,
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = Input)]
enum DeclaredArtifactError {
    #[error("Can't declare an artifact with an empty filename component")]
    DeclaredEmptyFileName,
    #[error(
        "Artifact `{0}` was declared with `has_content_based_path = {1}`, but is now being used with `has_content_based_path = {2}`"
    )]
    AlreadyDeclaredWithDifferentContentBasedPathHashing(String, bool, bool),
}

impl<'v> AnalysisRegistry<'v> {
    pub fn new_from_owner(
        owner: BaseDeferredKey,
        execution_platform: ExecutionPlatformResolution,
    ) -> buck2_error::Result<AnalysisRegistry<'v>> {
        Self::new_from_owner_and_deferred(execution_platform, DeferredHolderKey::Base(owner))
    }

    pub fn new_from_owner_and_deferred(
        execution_platform: ExecutionPlatformResolution,
        self_key: DeferredHolderKey,
    ) -> buck2_error::Result<Self> {
        Ok(AnalysisRegistry {
            actions: ActionsRegistry::new(self_key.dupe(), execution_platform.dupe()),
            anon_targets: (ANON_TARGET_REGISTRY_NEW.get()?)(PhantomData, execution_platform),
            analysis_value_storage: AnalysisValueStorage::new(self_key),
            short_path_assertions: HashMap::new(),
            content_based_path_assertions: HashSet::new(),
        })
    }

    /// Reserves a path in an output directory. Doesn't declare artifact,
    /// but checks that there is no previously declared artifact with a path
    /// which is in conflict with claimed `path`.
    pub fn claim_output_path(
        &mut self,
        eval: &Evaluator<'_, '_, '_>,
        path: &ForwardRelativePath,
    ) -> buck2_error::Result<()> {
        let declaration_location = eval.call_stack_top_location();
        self.actions.claim_output_path(path, declaration_location)
    }

    pub fn declare_dynamic_output(
        &mut self,
        artifact: &BuildArtifact,
        heap: Heap<'v>,
    ) -> buck2_error::Result<DeclaredArtifact<'v>> {
        self.actions.declare_dynamic_output(artifact, heap)
    }

    pub fn declare_output(
        &mut self,
        prefix: Option<&str>,
        filename: &str,
        output_type: OutputType,
        declaration_location: Option<FileSpan>,
        path_resolution_method: BuckOutPathKind,
        heap: Heap<'v>,
    ) -> buck2_error::Result<DeclaredArtifact<'v>> {
        // We don't allow declaring `` as an output, although technically there's nothing preventing
        // that
        if filename.is_empty() {
            return Err(DeclaredArtifactError::DeclaredEmptyFileName.into());
        }

        let path = ForwardRelativePath::new(filename)?.to_owned();
        let prefix = match prefix {
            None => None,
            Some(x) => Some(ForwardRelativePath::new(x)?.to_owned()),
        };
        self.actions.declare_artifact(
            prefix,
            path,
            output_type,
            declaration_location,
            path_resolution_method,
            heap,
        )
    }

    /// Takes a string or artifact/output artifact and converts it into an output artifact
    ///
    /// This is handy for functions like `ctx.actions.write` where it's nice to just let
    /// the user give us a string if they want as the output name.
    ///
    /// This function can declare new artifacts depending on the input.
    /// If there is no error, it returns a wrapper around the artifact (ArtifactDeclaration) and the corresponding OutputArtifact
    ///
    /// The valid types for `value` and subsequent actions are as follows:
    ///  - `str`: A new file is declared with this name.
    ///  - `StarlarkOutputArtifact`: The original artifact is returned
    ///  - `StarlarkArtifact`/`StarlarkDeclaredArtifact`: If the artifact is already bound, an error is raised. Otherwise we proceed with the original artifact.
    pub fn get_or_declare_output(
        &mut self,
        eval: &Evaluator<'v, '_, '_>,
        value: OutputArtifactArg<'v>,
        output_type: OutputType,
        has_content_based_path: Option<bool>,
    ) -> buck2_error::Result<(ArtifactDeclaration<'v>, OutputArtifact<'v>)> {
        let declaration_location = eval.call_stack_top_location();
        let heap = eval.heap();
        let declared_artifact = match value {
            OutputArtifactArg::Str(path) => {
                let artifact = self.declare_output(
                    None,
                    path,
                    output_type,
                    declaration_location.dupe(),
                    match has_content_based_path {
                        Some(true) => BuckOutPathKind::ContentHash,
                        Some(false) => BuckOutPathKind::Configuration,
                        None => BuckOutPathKind::default(),
                    },
                    heap,
                )?;
                heap.alloc_typed(StarlarkDeclaredArtifact::new(
                    declaration_location,
                    artifact,
                    AssociatedArtifacts::new(),
                ))
            }
            OutputArtifactArg::OutputArtifact(output) => output.inner(),
            OutputArtifactArg::DeclaredArtifact(artifact) => artifact,
            OutputArtifactArg::WrongArtifact(artifact) => {
                return Err(artifact.0.as_output_error());
            }
        };

        let output = declared_artifact.output_artifact();
        output.ensure_output_type(output_type)?;

        if let Some(has_content_based_path) = has_content_based_path {
            if has_content_based_path != output.has_content_based_path() {
                return Err(
                    DeclaredArtifactError::AlreadyDeclaredWithDifferentContentBasedPathHashing(
                        format!("{output}"),
                        output.has_content_based_path(),
                        has_content_based_path,
                    )
                    .into(),
                );
            }
        }

        Ok((
            ArtifactDeclaration {
                artifact: declared_artifact,
                heap,
            },
            output,
        ))
    }

    pub fn register_action<A: UnregisteredAction + 'static>(
        &mut self,
        outputs: IndexSet<OutputArtifact>,
        action: A,
        associated_value: Option<Value<'v>>,
        error_handler: Option<StarlarkCallable<'v>>,
    ) -> buck2_error::Result<()> {
        let id = self
            .actions
            .register(&self.analysis_value_storage.self_key, outputs, action)?;
        self.analysis_value_storage
            .set_action_data(id, (associated_value, error_handler))?;
        Ok(())
    }

    pub fn create_transitive_set(
        &mut self,
        definition: FrozenValueTyped<'v, FrozenTransitiveSetDefinition>,
        value: Option<Value<'v>>,
        children: Option<Value<'v>>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<ValueTyped<'v, TransitiveSet<'v>>> {
        Ok(self
            .analysis_value_storage
            .register_transitive_set(move |key| {
                let set =
                    TransitiveSet::new_from_values(key.dupe(), definition, value, children, eval)?;
                Ok(eval.heap().alloc_typed(set))
            })?)
    }

    pub(crate) fn take_promises(&mut self) -> Option<Box<dyn AnonPromisesDyn<'v>>> {
        self.anon_targets.take_promises()
    }

    pub fn consumer_analysis_artifacts(&self) -> Vec<PromiseArtifact> {
        self.anon_targets.consumer_analysis_artifacts()
    }

    pub fn record_short_path_assertion(
        &mut self,
        short_path: ForwardRelativePathBuf,
        promise_artifact_id: PromiseArtifactId,
    ) {
        self.short_path_assertions
            .insert(promise_artifact_id, short_path);
    }

    pub fn record_has_content_based_path_assertion(
        &mut self,
        promise_artifact_id: PromiseArtifactId,
    ) {
        self.content_based_path_assertions
            .insert(promise_artifact_id);
    }

    pub fn assert_no_promises(&self) -> buck2_error::Result<()> {
        self.anon_targets.assert_no_promises()
    }

    pub fn num_declared_actions(&self) -> u64 {
        self.actions.actions_len() as u64
    }

    pub fn num_declared_artifacts(&self) -> u64 {
        self.actions.artifacts_len() as u64
    }

    /// You MUST pass the same module to both the first function and the second one.
    /// It requires both to get the lifetimes to line up.
    pub fn finalize(
        self,
        env: &'v Module,
    ) -> buck2_error::Result<
        impl FnOnce(&FrozenModule) -> buck2_error::Result<RecordedAnalysisValues> + use<>,
    > {
        let AnalysisRegistry {
            actions,
            anon_targets: _,
            analysis_value_storage,
            short_path_assertions: _,
            content_based_path_assertions: _,
        } = self;

        let finalize_actions = actions.finalize()?;

        let self_key = analysis_value_storage.self_key.dupe();
        analysis_value_storage.write_to_module(env)?;
        Ok(move |frozen_env: &FrozenModule| {
            let analysis_value_fetcher = AnalysisValueFetcher {
                self_key,
                frozen_module: Some(frozen_env.dupe()),
            };
            let actions = (finalize_actions)(&analysis_value_fetcher)?;
            let recorded_values = analysis_value_fetcher.get_recorded_values(actions)?;

            Ok(recorded_values)
        })
    }

    pub fn execution_platform(&self) -> &ExecutionPlatformResolution {
        self.actions.execution_platform()
    }
}

pub struct ArtifactDeclaration<'v> {
    artifact: ValueTyped<'v, StarlarkDeclaredArtifact<'v>>,
    heap: Heap<'v>,
}

impl<'v> ArtifactDeclaration<'v> {
    pub fn into_declared_artifact(
        self,
        extra_associated_artifacts: AssociatedArtifacts,
    ) -> ValueTyped<'v, StarlarkDeclaredArtifact<'v>> {
        self.heap.alloc_typed(
            self.artifact
                .with_extended_associated_artifacts(extra_associated_artifacts),
        )
    }
}

/// Store `Value<'v>` values for actions registered in an implementation function
///
/// These values eventually are written into the mutable `Module`, and a wrapper is
/// made available to get the `OwnedFrozenValue` back out after that `Module` is frozen.
///
/// Note that this object has internal mutation and is only expected to live for the duration
/// of impl function execution.
///
/// At the end of impl function execution, `write_to_module` should be called
/// to write this object to `Module` extra value to get the values frozen.
#[derive(Debug, Allocative, ProvidesStaticType)]
pub struct AnalysisValueStorage<'v> {
    pub self_key: DeferredHolderKey,
    action_data: SmallMap<ActionIndex, (Option<Value<'v>>, Option<StarlarkCallable<'v>>)>,
    transitive_sets: Vec<ValueTyped<'v, TransitiveSet<'v>>>,
    pub lambda_params: Box<DynStarlark<'v, dyn DynamicLambdaParamsStorage<'v>>>,
    result_value: OnceCell<ValueTypedComplex<'v, ProviderCollection<'v>>>,
}

#[derive(Debug, Allocative, ProvidesStaticType)]
pub struct FrozenAnalysisValueStorage {
    pub self_key: DeferredHolderKey,
    action_data: SmallMap<ActionIndex, (Option<FrozenValue>, Option<FrozenStarlarkCallable>)>,
    transitive_sets: Vec<FrozenValueTyped<'static, FrozenTransitiveSet>>,
    pub lambda_params: Box<dyn FrozenDynamicLambdaParamsStorage>,
    result_value: Option<FrozenValueTyped<'static, FrozenProviderCollection>>,
}

unsafe impl<'v> Trace<'v> for AnalysisValueStorage<'v> {
    fn trace(&mut self, tracer: &Tracer<'v>) {
        let AnalysisValueStorage {
            action_data,
            transitive_sets,
            lambda_params,
            self_key,
            result_value,
        } = self;
        for (k, v) in action_data.iter_mut() {
            tracer.trace_static(k);
            v.trace(tracer);
        }
        for v in transitive_sets.iter_mut() {
            v.trace(tracer);
        }
        lambda_params.trace(tracer);
        tracer.trace_static(self_key);
        result_value.trace(tracer);
    }
}

impl<'v> Freeze for AnalysisValueStorage<'v> {
    type Frozen = FrozenAnalysisValueStorage;

    fn freeze(self, freezer: &Freezer) -> FreezeResult<Self::Frozen> {
        let AnalysisValueStorage {
            self_key,
            action_data,
            transitive_sets,
            lambda_params,
            result_value,
        } = self;

        Ok(FrozenAnalysisValueStorage {
            self_key,
            action_data: action_data
                .into_iter()
                .map(|(k, v)| Ok((k, v.freeze(freezer)?)))
                .collect::<FreezeResult<_>>()?,
            transitive_sets: transitive_sets
                .into_iter()
                .map(|v| {
                    FrozenValueTyped::new_err(v.to_value().freeze(freezer)?)
                        .map_err(|e| FreezeError::new(e.to_string()))
                })
                .collect::<FreezeResult<_>>()?,
            lambda_params: lambda_params.freeze(freezer)?,
            result_value: result_value.freeze(freezer)?,
        })
    }
}

/// Simple fetcher that fetches the values written in `AnalysisValueStorage::write_to_module`
///
/// These values are pulled from the `FrozenModule` that results from `env.freeze()`.
/// This is used by the action registry to make an `OwnedFrozenValue` available to
/// Actions' register function.
pub struct AnalysisValueFetcher {
    self_key: DeferredHolderKey,
    frozen_module: Option<FrozenModule>,
}

impl AnalysisValueFetcher {
    pub fn testing_new(self_key: DeferredHolderKey) -> Self {
        AnalysisValueFetcher {
            self_key,
            frozen_module: None,
        }
    }
}

impl<'v> AnalysisValueStorage<'v> {
    fn new(self_key: DeferredHolderKey) -> Self {
        Self {
            self_key: self_key.dupe(),
            action_data: SmallMap::new(),
            transitive_sets: Vec::new(),
            lambda_params: DYNAMIC_LAMBDA_PARAMS_STORAGES
                .get()
                .unwrap()
                .new_dynamic_lambda_params_storage(self_key),
            result_value: OnceCell::new(),
        }
    }

    /// Write self to `module` extra value.
    fn write_to_module(self, module: &'v Module) -> buck2_error::Result<()> {
        let extra_v = AnalysisExtraValue::get_or_init(module)?;
        let res = extra_v.analysis_value_storage.set(
            module
                .heap()
                .alloc_typed(StarlarkAnyComplex { value: self }),
        );
        if res.is_err() {
            return Err(internal_error!("analysis_value_storage is already set"));
        }
        Ok(())
    }

    pub(crate) fn register_transitive_set<
        F: FnOnce(TransitiveSetKey) -> buck2_error::Result<ValueTyped<'v, TransitiveSet<'v>>>,
    >(
        &mut self,
        func: F,
    ) -> buck2_error::Result<ValueTyped<'v, TransitiveSet<'v>>> {
        let key = TransitiveSetKey::new(
            self.self_key.dupe(),
            TransitiveSetIndex(self.transitive_sets.len().try_into()?),
        );
        let set = func(key.dupe())?;
        self.transitive_sets.push(set.dupe());
        Ok(set)
    }

    fn set_action_data(
        &mut self,
        id: ActionKey,
        action_data: (Option<Value<'v>>, Option<StarlarkCallable<'v>>),
    ) -> buck2_error::Result<()> {
        if &self.self_key != id.holder_key() {
            return Err(internal_error!(
                "Wrong action owner: expecting `{}`, got `{}`",
                self.self_key,
                id
            ));
        }
        self.action_data.insert(id.action_index(), action_data);
        Ok(())
    }

    pub fn set_result_value(
        &self,
        providers: ValueTypedComplex<'v, ProviderCollection<'v>>,
    ) -> buck2_error::Result<()> {
        if self.result_value.set(providers).is_err() {
            return Err(internal_error!("result_value is already set"));
        }
        Ok(())
    }
}

impl AnalysisValueFetcher {
    fn extra_value(
        &self,
    ) -> buck2_error::Result<Option<(&FrozenAnalysisValueStorage, &FrozenHeapRef)>> {
        match &self.frozen_module {
            None => Ok(None),
            Some(module) => {
                let analysis_extra_value = FrozenAnalysisExtraValue::get(module)?
                    .value
                    .analysis_value_storage
                    .internal_error("analysis_value_storage not set")?
                    .as_ref();
                Ok(Some((&analysis_extra_value.value, module.frozen_heap())))
            }
        }
    }

    /// Get the `OwnedFrozenValue` that corresponds to a `DeferredId`, if present
    pub fn get_action_data(
        &self,
        id: &ActionKey,
    ) -> buck2_error::Result<(Option<OwnedFrozenValue>, Option<OwnedFrozenValue>)> {
        let Some((storage, heap_ref)) = self.extra_value()? else {
            return Ok((None, None));
        };

        if id.holder_key() != &storage.self_key {
            return Err(internal_error!(
                "Wrong action owner: expecting `{}`, got `{}`",
                storage.self_key,
                id
            ));
        }

        let Some(value) = storage.action_data.get(&id.action_index()) else {
            return Ok((None, None));
        };

        unsafe {
            Ok((
                value.0.map(|v| OwnedFrozenValue::new(heap_ref.dupe(), v)),
                value.1.map(|v| OwnedFrozenValue::new(heap_ref.dupe(), v.0)),
            ))
        }
    }

    pub(crate) fn get_recorded_values(
        &self,
        actions: RecordedActions,
    ) -> buck2_error::Result<RecordedAnalysisValues> {
        let analysis_storage = match &self.frozen_module {
            None => None,
            Some(module) => Some(FrozenAnalysisExtraValue::get(module)?.try_map(|v| {
                v.value
                    .analysis_value_storage
                    .internal_error("analysis_value_storage not set")
            })?),
        };

        Ok(RecordedAnalysisValues {
            self_key: self.self_key.dupe(),
            analysis_storage,
            actions,
        })
    }
}

/// The analysis values stored in DeferredHolder.
#[derive(Debug, Allocative)]
pub struct RecordedAnalysisValues {
    self_key: DeferredHolderKey,
    analysis_storage: Option<OwnedFrozenValueTyped<StarlarkAnyComplex<FrozenAnalysisValueStorage>>>,
    actions: RecordedActions,
}

impl RecordedAnalysisValues {
    pub fn testing_new(
        self_key: DeferredHolderKey,
        transitive_sets: Vec<(TransitiveSetKey, OwnedFrozenValueTyped<FrozenTransitiveSet>)>,
        actions: RecordedActions,
    ) -> Self {
        let heap = FrozenHeap::new();
        let mut alloced_tsets = Vec::new();
        for (_key, tset) in transitive_sets
            .iter()
            .sorted_by_key(|(key, _)| key.index().0)
        {
            heap.add_reference(tset.owner());
            let tset = tset.owned_frozen_value_typed(&heap);
            alloced_tsets.push(tset);
        }

        let providers = FrozenProviderCollection::testing_new_default(&heap);

        let value = heap.alloc_simple(StarlarkAnyComplex {
            value: FrozenAnalysisValueStorage {
                self_key: self_key.dupe(),
                action_data: SmallMap::new(),
                transitive_sets: alloced_tsets,
                lambda_params: DYNAMIC_LAMBDA_PARAMS_STORAGES
                    .get()
                    .unwrap()
                    .new_frozen_dynamic_lambda_params_storage(),
                result_value: Some(
                    FrozenValueTyped::<FrozenProviderCollection>::new(heap.alloc(providers))
                        .unwrap(),
                ),
            },
        });
        Self {
            self_key,
            analysis_storage: Some(
                unsafe { OwnedFrozenValue::new(heap.into_ref(), value) }
                    .downcast()
                    .unwrap(),
            ),
            actions,
        }
    }

    pub(crate) fn lookup_transitive_set(
        &self,
        key: &TransitiveSetKey,
    ) -> buck2_error::Result<OwnedFrozenValueTyped<FrozenTransitiveSet>> {
        if key.holder_key() != &self.self_key {
            return Err(internal_error!(
                "Wrong owner for transitive set: expecting `{}`, got `{}`",
                self.self_key,
                key
            ));
        }
        self.analysis_storage
            .as_ref()
            .with_internal_error(|| format!("Missing analysis storage for `{key}`"))?
            .maybe_map(|v| v.value.transitive_sets.get(key.index().0 as usize).copied())
            .with_internal_error(|| format!("Missing transitive set `{key}`"))
    }

    pub fn lookup_action(&self, key: &ActionKey) -> buck2_error::Result<ActionLookup> {
        if key.holder_key() != &self.self_key {
            return Err(internal_error!(
                "Wrong owner for action: expecting `{}`, got `{}`",
                self.self_key,
                key
            ));
        }
        self.actions.lookup(key)
    }

    /// Iterates over the actions created in this analysis.
    pub fn iter_actions(&self) -> impl Iterator<Item = &Arc<RegisteredAction>> + '_ {
        self.actions.iter_actions()
    }

    pub fn analysis_storage(
        &self,
    ) -> buck2_error::Result<OwnedRefFrozenRef<'_, FrozenAnalysisValueStorage>> {
        Ok(self
            .analysis_storage
            .as_ref()
            .internal_error("missing analysis storage")?
            .as_owned_ref_frozen_ref()
            .map(|v| &v.value))
    }

    /// Iterates over the declared dynamic_output/actions.
    pub fn iter_dynamic_lambda_outputs(&self) -> impl Iterator<Item = BuildArtifact> + '_ {
        self.analysis_storage
            .iter()
            .flat_map(|v| v.value.lambda_params.iter_dynamic_lambda_outputs())
    }

    pub fn provider_collection(&self) -> buck2_error::Result<FrozenProviderCollectionValueRef<'_>> {
        let analysis_storage = self
            .analysis_storage
            .as_ref()
            .internal_error("missing analysis storage")?;
        let value = analysis_storage
            .as_ref()
            .value
            .result_value
            .internal_error("missing provider collection")?;
        unsafe {
            Ok(FrozenProviderCollectionValueRef::new(
                analysis_storage.owner(),
                value,
            ))
        }
    }

    pub(crate) fn retained_memory(&self) -> buck2_error::Result<usize> {
        Ok(self
            .analysis_storage
            .as_ref()
            .internal_error("missing analysis storage")?
            .owner()
            .allocated_bytes())
    }
}
