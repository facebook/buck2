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

use anyhow::Context as _;
use buck2_common::result::SharedResult;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_interpreter::starlark_profiler::StarlarkProfileDataAndStats;
use buck2_interpreter::starlark_profiler::StarlarkProfileModeOrInstrumentation;
use buck2_interpreter::starlark_profiler::StarlarkProfiler;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use gazebo::prelude::*;
use starlark::collections::SmallMap;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::structs::Struct;
use starlark::values::FrozenRef;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;
use thiserror::Error;

use crate::analysis::registry::AnalysisRegistry;
use crate::attrs::resolve::attr_type::dep::ResolutionError;
use crate::attrs::resolve::ctx::AnalysisQueryResult;
use crate::attrs::resolve::ctx::AttrResolutionContext;
use crate::deferred::types::DeferredAny;
use crate::deferred::types::DeferredId;
use crate::deferred::types::DeferredTable;
use crate::interpreter::rule_defs::cmd_args::FrozenCommandLineArgLike;
use crate::interpreter::rule_defs::context::AnalysisContext;
use crate::interpreter::rule_defs::provider::builtin::template_placeholder_info::FrozenTemplatePlaceholderInfo;
use crate::interpreter::rule_defs::provider::collection::ProviderCollection;
use crate::interpreter::rule_defs::rule::FrozenRuleCallable;

pub mod calculation;
pub(crate) mod configured_graph;
pub mod registry;
use allocative::Allocative;
use buck2_execute::base_deferred_key::BaseDeferredKey;
use buck2_interpreter::types::label::LabelGen;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::rule_type::StarlarkRuleType;

use crate::attrs::resolve::configured_attr::ConfiguredAttrExt;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;

#[derive(Error, Debug)]
pub enum AnalysisError {
    #[error(
        "Analysis context was missing a query result, this shouldn't be possible. Query was `{0}`"
    )]
    MissingQuery(String),
}

#[derive(Debug, Clone, Dupe, Allocative)]
pub struct AnalysisResult {
    /// The actual provider collection, validated to be the correct type (`FrozenProviderCollection`)
    provider_collection: FrozenProviderCollectionValue,
    deferred: DeferredTable,
    profile_data: Option<Arc<StarlarkProfileDataAndStats>>,
}

impl AnalysisResult {
    /// Create a new AnalysisResult
    pub fn new(
        provider_collection: FrozenProviderCollectionValue,
        deferred: DeferredTable,
        profile_data: Option<Arc<StarlarkProfileDataAndStats>>,
    ) -> Self {
        Self {
            provider_collection,
            deferred,
            profile_data,
        }
    }

    pub fn providers(&self) -> &FrozenProviderCollectionValue {
        &self.provider_collection
    }

    /// Used to lookup an inner named provider result.
    pub fn lookup_inner(
        &self,
        label: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<FrozenProviderCollectionValue> {
        self.provider_collection.lookup_inner(label)
    }

    pub fn lookup_deferred(&self, id: DeferredId) -> anyhow::Result<&(dyn DeferredAny + 'static)> {
        self.deferred.lookup_deferred(id)
    }
}

// Contains a `module` that things must live on, and various `FrozenProviderCollectionValue`s
// that are NOT tied to that module. Must claim ownership of them via `add_reference` before returning them.
pub struct RuleAnalysisAttrResolutionContext<'v> {
    pub module: &'v Module,
    pub dep_analysis_results: HashMap<&'v ConfiguredTargetLabel, FrozenProviderCollectionValue>,
    pub query_results: HashMap<String, Arc<AnalysisQueryResult>>,
}

impl<'v> AttrResolutionContext<'v> for RuleAnalysisAttrResolutionContext<'v> {
    fn starlark_module(&self) -> &'v Module {
        self.module
    }

    fn get_dep(
        &self,
        target: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<FrozenProviderCollectionValue> {
        match self.dep_analysis_results.get(target.target()) {
            None => Err(ResolutionError::MissingDep(target.clone()).into()),
            Some(x) => {
                let x = x.lookup_inner(target)?;
                // IMPORTANT: Anything given back to the user must be kept alive
                self.module.frozen_heap().add_reference(x.value().owner());
                Ok(x.dupe())
            }
        }
    }

    fn resolve_unkeyed_placeholder(
        &self,
        name: &str,
    ) -> Option<FrozenRef<'static, dyn FrozenCommandLineArgLike + 'static>> {
        // TODO(cjhopman): Make it an error if two deps provide a value for the placeholder.
        for providers in self.dep_analysis_results.values() {
            if let Some(placeholder_info) =
                FrozenTemplatePlaceholderInfo::from_providers(providers.provider_collection())
            {
                if let Some(value) = placeholder_info.unkeyed_variables().get(name) {
                    // IMPORTANT: Anything given back to the user must be kept alive
                    self.module
                        .frozen_heap()
                        .add_reference(providers.value().owner());
                    return Some(*value);
                }
            }
        }
        None
    }

    fn resolve_query(&self, query: &str) -> SharedResult<Arc<AnalysisQueryResult>> {
        match self.query_results.get(query) {
            None => Err(anyhow::anyhow!(AnalysisError::MissingQuery(query.to_owned())).into()),
            Some(x) => {
                for (_, y) in x.iter() {
                    // IMPORTANT: Anything given back to the user must be kept alive
                    self.module.frozen_heap().add_reference(y.value().owner());
                }
                Ok(x.dupe())
            }
        }
    }
}

pub trait RuleImplFunction: Sync {
    fn invoke<'v>(
        &self,
        eval: &mut Evaluator<'v, '_>,
        ctx: ValueTyped<'v, AnalysisContext<'v>>,
    ) -> anyhow::Result<Value<'v>>;
}

/// Container for the environment that analysis implementation functions should run in
struct AnalysisEnv<'a> {
    impl_function: &'a dyn RuleImplFunction,
    deps: HashMap<&'a ConfiguredTargetLabel, FrozenProviderCollectionValue>,
    query_results: HashMap<String, Arc<AnalysisQueryResult>>,
    execution_platform: &'a ExecutionPlatformResolution,
    label: ConfiguredTargetLabel,
}

async fn run_analysis<'a>(
    label: &ConfiguredTargetLabel,
    results: Vec<(&'a ConfiguredTargetLabel, AnalysisResult)>,
    query_results: HashMap<String, Arc<AnalysisQueryResult>>,
    execution_platform: &'a ExecutionPlatformResolution,
    impl_function: &'a dyn RuleImplFunction,
    node: &ConfiguredTargetNode,
    profile_mode: &StarlarkProfileModeOrInstrumentation,
) -> anyhow::Result<AnalysisResult> {
    let analysis_env = AnalysisEnv::new(
        label,
        results,
        query_results,
        execution_platform,
        impl_function,
    )?;
    run_analysis_with_env(analysis_env, node, profile_mode).await
}

impl<'a> AnalysisEnv<'a> {
    /// Create a new `AnalysisEnv`, ensuring that all heaps are kept alive that need to be
    fn new(
        label: &ConfiguredTargetLabel,
        results: Vec<(&'a ConfiguredTargetLabel, AnalysisResult)>,
        query_results: HashMap<String, Arc<AnalysisQueryResult>>,
        execution_platform: &'a ExecutionPlatformResolution,
        impl_function: &'a dyn RuleImplFunction,
    ) -> anyhow::Result<Self> {
        Ok(AnalysisEnv {
            impl_function,
            deps: get_deps_from_analysis_results(results)?,
            query_results,
            execution_platform,
            label: label.dupe(),
        })
    }
}

pub fn get_deps_from_analysis_results<'a>(
    results: Vec<(&'a ConfiguredTargetLabel, AnalysisResult)>,
) -> anyhow::Result<HashMap<&'a ConfiguredTargetLabel, FrozenProviderCollectionValue>> {
    results
        .into_iter()
        .map(|(label, result)| Ok((label, result.providers().dupe())))
        .collect::<anyhow::Result<
            HashMap<&'a ConfiguredTargetLabel, FrozenProviderCollectionValue>,
        >>()
}

async fn run_analysis_with_env(
    analysis_env: AnalysisEnv<'_>,
    node: &ConfiguredTargetNode,
    profile_mode: &StarlarkProfileModeOrInstrumentation,
) -> anyhow::Result<AnalysisResult> {
    let env = Module::new();
    let mut eval = Evaluator::new(&env);

    let resolution_ctx = RuleAnalysisAttrResolutionContext {
        module: &env,
        dep_analysis_results: analysis_env.deps,
        query_results: analysis_env.query_results,
    };

    let attrs_iter = node.attrs(AttrInspectOptions::All);
    let mut resolved_attrs = SmallMap::with_capacity(attrs_iter.size_hint().0);
    for (name, attr) in attrs_iter {
        resolved_attrs.insert(
            env.heap().alloc_str(name),
            attr.resolve_single(&resolution_ctx)?,
        );
    }

    let registry = AnalysisRegistry::new_from_owner(
        BaseDeferredKey::TargetLabel(node.name().dupe()),
        analysis_env.execution_platform.dupe(),
    );
    let attributes = env.heap().alloc(Struct::new(resolved_attrs));
    let ctx = env.heap().alloc_typed(AnalysisContext::new(
        eval.heap(),
        attributes,
        Some(eval.heap().alloc_typed(LabelGen::new(
            env.heap(),
            ConfiguredProvidersLabel::new(analysis_env.label, ProvidersName::Default),
        ))),
        registry,
    ));

    let mut profiler_opt = profile_mode
        .profile_mode()
        .map(|profile_mode| StarlarkProfiler::new(profile_mode.dupe(), true));

    let mut profiler = match &mut profiler_opt {
        None => {
            StarlarkProfilerOrInstrumentation::maybe_instrumentation(profile_mode.instrumentation())
        }
        Some(profiler) => StarlarkProfilerOrInstrumentation::for_profiler(profiler),
    };

    profiler.initialize(&mut eval)?;

    let list_res = analysis_env.impl_function.invoke(&mut eval, ctx)?;

    profiler
        .evaluation_complete(&mut eval)
        .context("Profiler finalization failed")?;

    // TODO: Convert the ValueError from `try_from_value` better than just printing its Debug
    let res_typed = ProviderCollection::try_from_value(list_res)?;
    let res = env.heap().alloc(res_typed);
    env.set("", res);

    // Pull the ctx object back out, and steal ctx.action's state back
    let analysis_registry = ctx.take_state();
    let (frozen_env, deferreds) = analysis_registry.finalize(&env)(env)?;

    profiler
        .visit_frozen_module(Some(&frozen_env))
        .context("Profiler heap visitation failed")?;

    let profile_data = profiler_opt.map(|p| p.finish()).transpose()?.map(Arc::new);

    let res = frozen_env.get("").unwrap();
    let provider_collection = FrozenProviderCollectionValue::try_from_value(res)
        .expect("just created this, this shouldn't happen");

    // this could look nicer if we had the entire analysis be a deferred
    let deferred = DeferredTable::new(deferreds.take_result()?);
    Ok(AnalysisResult::new(
        provider_collection,
        deferred,
        profile_data,
    ))
}

pub fn get_user_defined_rule_impl(
    module: FrozenModule,
    rule_type: &StarlarkRuleType,
) -> impl RuleImplFunction {
    struct Impl {
        module: FrozenModule,
        name: String,
    }

    impl RuleImplFunction for Impl {
        fn invoke<'v>(
            &self,
            eval: &mut Evaluator<'v, '_>,
            ctx: ValueTyped<'v, AnalysisContext<'v>>,
        ) -> anyhow::Result<Value<'v>> {
            let rule_callable = self
                .module
                .get_any_visibility(&self.name)
                .with_context(|| format!("Couldn't find rule `{}`", self.name))?
                .0;
            let rule_impl = {
                // Need to free up the starlark_ctx borrow before we return
                let rule_callable = rule_callable.owned_value(eval.frozen_heap());

                let frozen_callable = rule_callable
                    .downcast_ref::<FrozenRuleCallable>()
                    .unwrap_or_else(|| {
                        panic!(
                            "A rule implementation should be a FrozenRuleCallable. It was a {}",
                            rule_callable.get_type(),
                        )
                    });
                frozen_callable.implementation()
            };
            eval.eval_function(rule_impl.to_value(), &[ctx.to_value()], &[])
        }
    }

    Impl {
        module,
        name: rule_type.name.clone(),
    }
}
