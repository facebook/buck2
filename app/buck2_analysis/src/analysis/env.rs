/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Context;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::interpreter::rule_defs::cmd_args::value::FrozenCommandLineArg;
use buck2_build_api::interpreter::rule_defs::context::AnalysisContext;
use buck2_build_api::interpreter::rule_defs::provider::builtin::template_placeholder_info::FrozenTemplatePlaceholderInfo;
use buck2_build_api::interpreter::rule_defs::provider::builtin::validation_info::FrozenValidationInfo;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollection;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValueRef;
use buck2_build_api::interpreter::rule_defs::provider::collection::ProviderCollection;
use buck2_build_api::validation::transitive_validations::TransitiveValidations;
use buck2_build_api::validation::transitive_validations::TransitiveValidationsData;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::unsafe_send_future::UnsafeSendFuture;
use buck2_error::BuckErrorContext;
use buck2_events::dispatch::get_dispatcher;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::error::BuckStarlarkError;
use buck2_interpreter::error::OtherErrorHandling;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::soft_error::Buck2StarlarkSoftErrorHandler;
use buck2_interpreter::starlark_profiler::data::ProfileTarget;
use buck2_interpreter::starlark_profiler::mode::StarlarkProfileMode;
use buck2_interpreter::starlark_profiler::profiler::StarlarkProfiler;
use buck2_interpreter::starlark_profiler::profiler::StarlarkProfilerOpt;
use buck2_interpreter::types::rule::FROZEN_PROMISE_ARTIFACT_MAPPINGS_GET_IMPL;
use buck2_interpreter::types::rule::FROZEN_RULE_GET_IMPL;
use buck2_node::nodes::configured::ConfiguredTargetNodeRef;
use buck2_node::rule_type::StarlarkRuleType;
use dice::DiceComputations;
use dupe::Dupe;
use futures::Future;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::FrozenValueTyped;
use starlark::values::OwnedFrozenRef;
use starlark::values::Value;
use starlark::values::ValueTyped;
use starlark::values::ValueTypedComplex;
use starlark_map::small_map::SmallMap;

use crate::analysis::plugins::plugins_to_starlark_value;
use crate::attrs::resolve::ctx::AnalysisQueryResult;
use crate::attrs::resolve::ctx::AttrResolutionContext;
use crate::attrs::resolve::node_to_attrs_struct::node_to_attrs_struct;

#[derive(buck2_error::Error, Debug)]
enum AnalysisError {
    #[error(
        "Analysis context was missing a query result, this shouldn't be possible. Query was `{0}`"
    )]
    MissingQuery(String),
    #[error("required dependency `{0}` was not found")]
    MissingDep(ConfiguredProvidersLabel),
}

// Contains a `module` that things must live on, and various `FrozenProviderCollectionValue`s
// that are NOT tied to that module. Must claim ownership of them via `add_reference` before returning them.
pub struct RuleAnalysisAttrResolutionContext<'v> {
    pub module: &'v Module,
    pub dep_analysis_results: HashMap<&'v ConfiguredTargetLabel, FrozenProviderCollectionValue>,
    pub query_results: HashMap<String, Arc<AnalysisQueryResult>>,
    pub execution_platform_resolution: ExecutionPlatformResolution,
}

impl<'v> AttrResolutionContext<'v> for RuleAnalysisAttrResolutionContext<'v> {
    fn starlark_module(&self) -> &'v Module {
        self.module
    }

    fn get_dep(
        &self,
        target: &ConfiguredProvidersLabel,
    ) -> anyhow::Result<FrozenValueTyped<'v, FrozenProviderCollection>> {
        get_dep(&self.dep_analysis_results, target, self.module)
    }

    fn resolve_unkeyed_placeholder(
        &self,
        name: &str,
    ) -> anyhow::Result<Option<FrozenCommandLineArg>> {
        Ok(resolve_unkeyed_placeholder(
            &self.dep_analysis_results,
            name,
            self.module,
        ))
    }

    fn resolve_query(&self, query: &str) -> buck2_error::Result<Arc<AnalysisQueryResult>> {
        resolve_query(&self.query_results, query, self.module)
    }

    fn execution_platform_resolution(&self) -> &ExecutionPlatformResolution {
        &self.execution_platform_resolution
    }
}

pub fn get_dep<'v>(
    dep_analysis_results: &HashMap<&'_ ConfiguredTargetLabel, FrozenProviderCollectionValue>,
    target: &ConfiguredProvidersLabel,
    module: &'v Module,
) -> anyhow::Result<FrozenValueTyped<'v, FrozenProviderCollection>> {
    match dep_analysis_results.get(target.target()) {
        None => Err(AnalysisError::MissingDep(target.dupe()).into()),
        Some(x) => {
            let x = x.lookup_inner(target)?;
            // IMPORTANT: Anything given back to the user must be kept alive
            Ok(x.add_heap_ref(module.frozen_heap()))
        }
    }
}

pub fn resolve_unkeyed_placeholder<'v>(
    dep_analysis_results: &HashMap<&'v ConfiguredTargetLabel, FrozenProviderCollectionValue>,
    name: &str,
    module: &'v Module,
) -> Option<FrozenCommandLineArg> {
    // TODO(cjhopman): Make it an error if two deps provide a value for the placeholder.
    for providers in dep_analysis_results.values() {
        if let Some(placeholder_info) = providers
            .provider_collection()
            .builtin_provider::<FrozenTemplatePlaceholderInfo>()
        {
            if let Some(value) = placeholder_info.unkeyed_variables().get(name) {
                // IMPORTANT: Anything given back to the user must be kept alive
                module
                    .frozen_heap()
                    .add_reference(providers.value().owner());
                return Some(*value);
            }
        }
    }
    None
}

pub fn resolve_query<'v>(
    query_results: &HashMap<String, Arc<AnalysisQueryResult>>,
    query: &str,
    module: &'v Module,
) -> buck2_error::Result<Arc<AnalysisQueryResult>> {
    match query_results.get(query) {
        None => Err(anyhow::anyhow!(AnalysisError::MissingQuery(query.to_owned())).into()),
        Some(x) => {
            for (_, y) in x.result.iter() {
                // IMPORTANT: Anything given back to the user must be kept alive
                module.frozen_heap().add_reference(y.value().owner());
            }
            Ok(x.dupe())
        }
    }
}

pub trait RuleSpec: Sync {
    fn invoke<'v>(
        &self,
        eval: &mut Evaluator<'v, '_, '_>,
        ctx: ValueTyped<'v, AnalysisContext<'v>>,
    ) -> anyhow::Result<Value<'v>>;

    fn promise_artifact_mappings<'v>(
        &self,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> anyhow::Result<SmallMap<String, Value<'v>>>;
}

/// Container for the environment that analysis implementation functions should run in
struct AnalysisEnv<'a> {
    rule_spec: &'a dyn RuleSpec,
    deps: Vec<(&'a ConfiguredTargetLabel, AnalysisResult)>,
    query_results: HashMap<String, Arc<AnalysisQueryResult>>,
    execution_platform: &'a ExecutionPlatformResolution,
    label: ConfiguredTargetLabel,
}

pub(crate) async fn run_analysis<'a>(
    dice: &'a mut DiceComputations<'_>,
    label: &ConfiguredTargetLabel,
    results: Vec<(&'a ConfiguredTargetLabel, AnalysisResult)>,
    query_results: HashMap<String, Arc<AnalysisQueryResult>>,
    execution_platform: &'a ExecutionPlatformResolution,
    rule_spec: &'a dyn RuleSpec,
    node: ConfiguredTargetNodeRef<'a>,
    profile_mode: &'a StarlarkProfileMode,
) -> anyhow::Result<AnalysisResult> {
    let analysis_env = AnalysisEnv {
        rule_spec,
        deps: results,
        query_results,
        execution_platform,
        label: label.dupe(),
    };
    run_analysis_with_env(dice, analysis_env, node, profile_mode).await
}

pub fn get_deps_from_analysis_results(
    results: Vec<(&ConfiguredTargetLabel, AnalysisResult)>,
) -> anyhow::Result<HashMap<&ConfiguredTargetLabel, FrozenProviderCollectionValue>> {
    results
        .into_iter()
        .map(|(label, result)| Ok((label, result.providers()?.to_owned())))
        .collect::<anyhow::Result<HashMap<&ConfiguredTargetLabel, FrozenProviderCollectionValue>>>()
}

// Used to express that the impl Future below captures multiple named lifetimes.
// See https://github.com/rust-lang/rust/issues/34511#issuecomment-373423999 for more details.
trait Captures<'x> {}
impl<'x, T: ?Sized> Captures<'x> for T {}

fn run_analysis_with_env<'a, 'd: 'a>(
    dice: &'a mut DiceComputations<'d>,
    analysis_env: AnalysisEnv<'a>,
    node: ConfiguredTargetNodeRef<'a>,
    profile_mode: &'a StarlarkProfileMode,
) -> impl Future<Output = anyhow::Result<AnalysisResult>> + 'a + Captures<'d> {
    let fut = async move {
        run_analysis_with_env_underlying(dice, analysis_env, node, profile_mode).await
    };
    unsafe { UnsafeSendFuture::new_encapsulates_starlark(fut) }
}

async fn run_analysis_with_env_underlying(
    dice: &mut DiceComputations<'_>,
    analysis_env: AnalysisEnv<'_>,
    node: ConfiguredTargetNodeRef<'_>,
    profile_mode: &StarlarkProfileMode,
) -> anyhow::Result<AnalysisResult> {
    let env = Module::new();
    let print = EventDispatcherPrintHandler(get_dispatcher());

    let validations_from_deps = analysis_env
        .deps
        .iter()
        .filter_map(|(label, analysis_result)| {
            analysis_result
                .validations
                .dupe()
                .map(|v| ((*label).dupe(), v))
        })
        .collect::<SmallMap<_, _>>();

    let (attributes, plugins) = {
        let dep_analysis_results = get_deps_from_analysis_results(analysis_env.deps)?;
        let resolution_ctx = RuleAnalysisAttrResolutionContext {
            module: &env,
            dep_analysis_results,
            query_results: analysis_env.query_results,
            execution_platform_resolution: node.execution_platform_resolution().clone(),
        };

        (
            node_to_attrs_struct(node, &resolution_ctx)?,
            plugins_to_starlark_value(node, &resolution_ctx)?,
        )
    };

    let registry = AnalysisRegistry::new_from_owner(
        BaseDeferredKey::TargetLabel(node.label().dupe()),
        analysis_env.execution_platform.dupe(),
    )?;

    let mut profiler_opt = profile_mode.profile_mode().map(|profile_mode| {
        StarlarkProfiler::new(
            profile_mode.dupe(),
            true,
            ProfileTarget::Analysis(node.label().dupe()),
        )
    });

    let mut profiler = match &mut profiler_opt {
        None => StarlarkProfilerOpt::disabled(),
        Some(profiler) => StarlarkProfilerOpt::for_profiler(profiler),
    };

    let (dice, mut eval, ctx, list_res) = with_starlark_eval_provider(
        dice,
        &mut profiler,
        format!("analysis:{}", node.label()),
        |provider, dice| {
            let (mut eval, _) = provider.make(&env)?;
            eval.set_print_handler(&print);
            eval.set_soft_error_handler(&Buck2StarlarkSoftErrorHandler);

            let ctx = AnalysisContext::prepare(
                eval.heap(),
                Some(attributes),
                Some(analysis_env.label),
                Some(plugins.into()),
                registry,
                dice.global_data().get_digest_config(),
            );

            let list_res = analysis_env.rule_spec.invoke(&mut eval, ctx)?;

            // TODO(cjhopman): This seems quite wrong. This should be happening after run_promises.
            provider
                .evaluation_complete(&mut eval)
                .context("Profiler finalization failed")?;
            // TODO(cjhopman): This is gross, but we can't await on running the promises within
            // the with_starlark_eval_provider scoped thing (as we may be holding a debugger
            // permit, running the promises may require doing more starlark evaluation which in
            // turn requires those permits). We will actually re-enter a provider scope in the
            // run_promises call when we get back to resolving the promises (and running the starlark
            // Promise::map() lambdas).
            Ok((dice, eval, ctx, list_res))
        },
    )
    .await?;

    ctx.actions
        .run_promises(
            dice,
            &mut eval,
            format!("anon_analysis$promises:{}", node.label()),
        )
        .await?;

    // Pull the ctx object back out, and steal ctx.action's state back
    let analysis_registry = ctx.take_state();

    // TODO: Convert the ValueError from `try_from_value` better than just printing its Debug
    let res_typed = ProviderCollection::try_from_value(list_res)?;
    {
        let provider_collection = ValueTypedComplex::new_err(env.heap().alloc(res_typed))
            .internal_error_anyhow("Just allocated provider collection")?;
        analysis_registry
            .analysis_value_storage
            .set_result_value(provider_collection)?;
    }

    drop(eval);

    let declared_actions = analysis_registry.num_declared_actions();
    let declared_artifacts = analysis_registry.num_declared_artifacts();
    let (frozen_env, recorded_values) = analysis_registry.finalize(&env)?(env)?;

    profiler
        .visit_frozen_module(Some(&frozen_env))
        .context("Profiler heap visitation failed")?;

    let profile_data = profiler_opt.map(|p| p.finish()).transpose()?.map(Arc::new);

    let validations = transitive_validations(
        validations_from_deps,
        recorded_values.provider_collection()?,
    );

    Ok(AnalysisResult::new(
        recorded_values,
        profile_data,
        HashMap::new(),
        declared_actions,
        declared_artifacts,
        validations,
    ))
}

pub fn transitive_validations(
    deps: SmallMap<ConfiguredTargetLabel, TransitiveValidations>,
    provider_collection: FrozenProviderCollectionValueRef,
) -> Option<TransitiveValidations> {
    let info = provider_collection
        .value()
        .builtin_provider::<FrozenValidationInfo>();
    if info.is_some() || deps.len() > 1 {
        let owned_info = info.map(|x| unsafe {
            OwnedFrozenRef::new_unchecked(x.as_ref(), provider_collection.owner().dupe())
        });
        Some(TransitiveValidations(Arc::new(TransitiveValidationsData {
            info: owned_info,
            children: deps.into_keys().collect(),
        })))
    } else {
        assert!(
            deps.len() <= 1,
            "Reuse the single element if any from one of the deps for current node."
        );
        deps.into_values().next()
    }
}

pub fn get_user_defined_rule_spec(
    module: FrozenModule,
    rule_type: &StarlarkRuleType,
) -> impl RuleSpec {
    struct Impl {
        module: FrozenModule,
        name: String,
    }

    impl RuleSpec for Impl {
        fn invoke<'v>(
            &self,
            eval: &mut Evaluator<'v, '_, '_>,
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
                let rule_callable = rule_callable
                    .unpack_frozen()
                    .internal_error_anyhow("Must be frozen")?;

                (FROZEN_RULE_GET_IMPL.get()?)(rule_callable)?
            };
            eval.eval_function(rule_impl.0.to_value(), &[ctx.to_value()], &[])
                .map_err(|e| BuckStarlarkError::new(e, OtherErrorHandling::InputError).into())
        }

        fn promise_artifact_mappings<'v>(
            &self,
            eval: &mut Evaluator<'v, '_, '_>,
        ) -> anyhow::Result<SmallMap<String, Value<'v>>> {
            let rule_callable = self
                .module
                .get_any_visibility(&self.name)
                .with_context(|| format!("Couldn't find rule `{}`", self.name))?
                .0;
            let frozen_promise_artifact_mappings = {
                // Need to free up the starlark_ctx borrow before we return
                let rule_callable = rule_callable.owned_value(eval.frozen_heap());
                let rule_callable = rule_callable
                    .unpack_frozen()
                    .internal_error_anyhow("Must be frozen")?;

                (FROZEN_PROMISE_ARTIFACT_MAPPINGS_GET_IMPL.get()?)(rule_callable)?
            };

            Ok(frozen_promise_artifact_mappings
                .iter()
                .map(|(frozen_string, frozen_func)| {
                    (frozen_string.to_string(), frozen_func.to_value())
                })
                .collect::<SmallMap<_, _>>())
        }
    }

    Impl {
        module,
        name: rule_type.name.clone(),
    }
}
