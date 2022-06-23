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
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProviderName;
use buck2_core::provider::label::ProvidersName;
use buck2_core::result::SharedResult;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use gazebo::prelude::*;
use starlark::collections::SmallMap;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::structs::Struct;
use starlark::values::FrozenRef;
use starlark::values::Value;
use starlark::values::ValueLike;
use thiserror::Error;

use crate::analysis::registry::AnalysisRegistry;
use crate::attrs::analysis::AnalysisQueryResult;
use crate::attrs::analysis::AttrResolutionContext;
use crate::configuration::execution::ExecutionPlatformResolution;
use crate::deferred::DeferredAny;
use crate::deferred::DeferredId;
use crate::deferred::DeferredTable;
use crate::interpreter::rule_defs::cmd_args::FrozenCommandLineArgLike;
use crate::interpreter::rule_defs::context::AnalysisContext;
use crate::interpreter::rule_defs::provider::builtin::template_placeholder_info::FrozenTemplatePlaceholderInfo;
use crate::interpreter::rule_defs::provider::collection::ProviderCollection;
use crate::interpreter::rule_defs::rule::FrozenRuleCallable;

pub mod calculation;
pub(crate) mod configured_graph;
pub mod registry;
use buck2_interpreter::types::label::LabelGen;
pub use calculation::profile_analysis;
pub use calculation::resolve_queries;
use starlark::values::ValueTyped;

use crate::deferred::BaseDeferredKey;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use crate::nodes::configured::ConfiguredTargetNode;
use crate::nodes::StarlarkRuleType;

#[derive(Error, Debug)]
enum AnalysisError {
    #[error(
        "Cannot handle flavor `{flavor}` on target `{target}`. Most flavors are unsupported in Buck2."
    )]
    UnknownFlavors { target: String, flavor: String },
    #[error(
        "Analysis context was missing a query result, this shouldn't be possible. Query was `{0}`"
    )]
    MissingQuery(String),
    #[error(
        "requested sub target named `{0}` of target `{1}` is not available. Available subtargets are: `{2:?}`"
    )]
    RequestedInvalidSubTarget(ProviderName, ConfiguredProvidersLabel, Vec<String>),
}

#[derive(Debug, Clone, Dupe)]
pub struct AnalysisResult {
    /// The actual provider collection, validated to be the correct type (`FrozenProviderCollection`)
    provider_collection: FrozenProviderCollectionValue,
    deferred: DeferredTable,
}

impl AnalysisResult {
    /// Create a new AnalysisResult
    pub fn new(
        provider_collection: FrozenProviderCollectionValue,
        deferred: DeferredTable,
    ) -> Self {
        Self {
            provider_collection,
            deferred,
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
        match label.name() {
            ProvidersName::Default => anyhow::Ok(self.provider_collection.dupe()),
            ProvidersName::Named(provider_names) => {
                Ok(FrozenProviderCollectionValue::from_value(
                    self.provider_collection.value().try_map(|v| {
                        let mut collection_value = v;

                        for provider_name in provider_names {
                            let maybe_di = collection_value
                                .default_info()
                                .get_sub_target_providers(provider_name.as_str());

                            match maybe_di {
                                // The inner values should all be frozen if in a frozen provider collection
                                Some(inner) => {
                                    collection_value = inner;
                                }
                                None => {
                                    return Err(anyhow::anyhow!(
                                        AnalysisError::RequestedInvalidSubTarget(
                                            provider_name.clone(),
                                            label.clone(),
                                            v.default_info()
                                                .sub_targets()
                                                .keys()
                                                .map(|s| (*s).to_owned())
                                                .collect()
                                        )
                                    ));
                                }
                            }
                        }

                        Ok(collection_value)
                    })?,
                ))
            }
            ProvidersName::UnrecognizedFlavor(flavor) => Err(AnalysisError::UnknownFlavors {
                target: label.unconfigured().to_string(),
                flavor: flavor.clone(),
            }
            .into()),
        }
    }

    pub fn lookup_deferred(&self, id: DeferredId) -> anyhow::Result<&(dyn DeferredAny + 'static)> {
        self.deferred.lookup_deferred(id)
    }
}

// Contains a `module` that things must live on, and various `FrozenProviderCollectionValue`s
// that are NOT tied to that module. Must claim ownership of them via `add_reference` before returning them.
struct RuleAnalysisAttrResolutionContext<'v> {
    module: &'v Module,
    dep_analysis_results: HashMap<&'v ConfiguredProvidersLabel, FrozenProviderCollectionValue>,
    query_results: HashMap<String, Arc<AnalysisQueryResult>>,
}

impl<'v> AttrResolutionContext for RuleAnalysisAttrResolutionContext<'v> {
    fn starlark_module(&self) -> &Module {
        self.module
    }

    fn get_dep(&self, target: &ConfiguredProvidersLabel) -> Option<FrozenProviderCollectionValue> {
        match self.dep_analysis_results.get(target) {
            None => None,
            Some(x) => {
                // IMPORTANT: Anything given back to the user must be kept alive
                self.module.frozen_heap().add_reference(x.value().owner());
                Some(x.dupe())
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

pub trait RuleImplFunction {
    fn invoke<'v>(&self, eval: &mut Evaluator<'v, '_>, ctx: Value<'v>)
    -> anyhow::Result<Value<'v>>;
}

/// Container for the environment that analysis implementation functions should run in
struct AnalysisEnv<'a> {
    impl_function: &'a dyn RuleImplFunction,
    deps: HashMap<&'a ConfiguredProvidersLabel, FrozenProviderCollectionValue>,
    query_results: HashMap<String, Arc<AnalysisQueryResult>>,
    execution_platform: &'a ExecutionPlatformResolution,
    label: ConfiguredTargetLabel,
}

fn run_analysis<'a>(
    label: &ConfiguredTargetLabel,
    results: Vec<(&'a ConfiguredProvidersLabel, AnalysisResult)>,
    query_results: HashMap<String, Arc<AnalysisQueryResult>>,
    execution_platform: &'a ExecutionPlatformResolution,
    impl_function: &'a dyn RuleImplFunction,
    node: &ConfiguredTargetNode,
    profiler: &mut StarlarkProfilerOrInstrumentation,
) -> anyhow::Result<AnalysisResult> {
    let analysis_env = AnalysisEnv::new(
        label,
        results,
        query_results,
        execution_platform,
        impl_function,
    )?;
    run_analysis_with_env(analysis_env, node, profiler)
}

impl<'a> AnalysisEnv<'a> {
    /// Create a new `AnalysisEnv`, ensuring that all heaps are kept alive that need to be
    fn new(
        label: &ConfiguredTargetLabel,
        results: Vec<(&'a ConfiguredProvidersLabel, AnalysisResult)>,
        query_results: HashMap<String, Arc<AnalysisQueryResult>>,
        execution_platform: &'a ExecutionPlatformResolution,
        impl_function: &'a dyn RuleImplFunction,
    ) -> anyhow::Result<Self> {
        let deps =
            results
                .into_iter()
                .map(|(label, result)| Ok((label, result.lookup_inner(label)?)))
                .collect::<anyhow::Result<
                    HashMap<&'a ConfiguredProvidersLabel, FrozenProviderCollectionValue>,
                >>()?;

        Ok(AnalysisEnv {
            impl_function,
            deps,
            query_results,
            execution_platform,
            label: label.dupe(),
        })
    }
}

fn run_analysis_with_env(
    analysis_env: AnalysisEnv,
    node: &ConfiguredTargetNode,
    profiler: &mut StarlarkProfilerOrInstrumentation,
) -> anyhow::Result<AnalysisResult> {
    let env = Module::new();
    let mut eval = Evaluator::new(&env);

    let resolution_ctx = RuleAnalysisAttrResolutionContext {
        module: &env,
        dep_analysis_results: analysis_env.deps,
        query_results: analysis_env.query_results,
    };

    let mut resolved_attrs = SmallMap::with_capacity(node.attrs().size_hint().0);
    for (name, attr) in node.attrs() {
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
    let ctx = env.heap().alloc(AnalysisContext::new(
        eval.heap(),
        attributes,
        Some(
            ValueTyped::new(eval.heap().alloc(LabelGen::new(
                env.heap(),
                ConfiguredProvidersLabel::new(analysis_env.label, ProvidersName::Default),
            )))
            .unwrap(),
        ),
        registry,
    ));

    profiler.initialize(&mut eval);

    let list_res = analysis_env.impl_function.invoke(&mut eval, ctx)?;

    profiler
        .finalize(&mut eval)
        .context("Profiler finalization failed")?;

    // TODO: Convert the ValueError from `try_from_value` better than just printing its Debug
    let res_typed = ProviderCollection::try_from_value(list_res)?;
    let res = env.heap().alloc(res_typed);
    env.set("", res);

    // Pull the ctx object back out, and steal ctx.action's state back
    let analysis_registry = AnalysisContext::take_state(ctx);
    let (frozen_env, deferreds) = analysis_registry.finalize(&env)(env)?;

    profiler
        .visit_heap(Some(frozen_env.frozen_heap()))
        .context("Profiler heap visitation failed")?;

    let res = frozen_env.get("").unwrap();
    let provider_collection = FrozenProviderCollectionValue::try_from_value(res)
        .expect("just created this, this shouldn't happen");

    // this could look nicer if we had the entire analysis be a deferred
    let deferred = DeferredTable::new(deferreds.take_result()?);
    Ok(AnalysisResult::new(provider_collection, deferred))
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
            ctx: Value<'v>,
        ) -> anyhow::Result<Value<'v>> {
            let rule_callable = self
                .module
                .get_any_visibility(&self.name)
                .unwrap_or_else(|| {
                    unreachable!(
                        "Expected a rule {}, only {:?}, but there was no value with that name.",
                        self.name,
                        self.module.names().collect::<Vec<_>>()
                    )
                })
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
                frozen_callable.get_impl()
            };
            eval.eval_function(rule_impl.to_value(), &[ctx], &[])
        }
    }

    Impl {
        module,
        name: rule_type.name.clone(),
    }
}
