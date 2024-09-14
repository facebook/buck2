/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Starlark Actions API for bxl functions
use std::sync::Arc;

use allocative::Allocative;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::interpreter::rule_defs::provider::dependency::Dependency;
use buck2_configured::configuration::calculation::ConfigurationCalculation;
use buck2_configured::nodes::calculation::ExecutionPlatformConstraints;
use buck2_configured::target::TargetConfiguredTargetLabel;
use buck2_core::cells::name::CellName;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::pair::ConfigurationNoExec;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_interpreter::types::configured_providers_label::StarlarkProvidersLabel;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::attrs::configuration_context::AttrConfigurationContextImpl;
use buck2_node::configuration::resolved::ConfigurationSettingKey;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use futures::FutureExt;
use gazebo::prelude::SliceExt;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::dict::AllocDict;
use starlark::values::dict::DictType;
use starlark::values::starlark_value;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark::StarlarkDocs;
use starlark_map::ordered_map::OrderedMap;

use crate::bxl::starlark_defs::context::BxlContextNoDice;

#[derive(Debug, buck2_error::Error)]
enum BxlActionsError {
    #[error(
        "An action registry was already requested via `ctx.bxl_actions().actions`. Only one action registry is allowed"
    )]
    RegistryAlreadyCreated,
}

pub(crate) async fn resolve_bxl_execution_platform(
    ctx: &mut DiceComputations<'_>,
    cell: CellName,
    exec_deps: Vec<ProvidersLabel>,
    toolchain_deps: Vec<ProvidersLabel>,
    target_platform: Option<TargetLabel>,
    exec_compatible_with: Arc<[ConfigurationSettingKey]>,
) -> anyhow::Result<BxlExecutionResolution> {
    // bxl has on transitions
    let resolved_transitions = OrderedMap::new();

    let platform_configuration = match target_platform.as_ref() {
        Some(global_target_platform) => {
            ctx.get_platform_configuration(global_target_platform)
                .await?
        }
        None => ConfigurationData::unspecified(),
    };
    let resolved_configuration = {
        ctx.get_resolved_configuration(&platform_configuration, cell, &*exec_compatible_with)
            .await?
    };

    // there is not explicit configured deps, so platforms is empty
    let platform_cfgs = OrderedMap::new();

    let configuration_ctx = AttrConfigurationContextImpl::new(
        &resolved_configuration,
        ConfigurationNoExec::unbound_exec(),
        // We don't really need `resolved_transitions` here:
        // `Traversal` declared above ignores transitioned dependencies.
        // But we pass `resolved_transitions` here to prevent breakages in the future
        // if something here changes.
        &resolved_transitions,
        &platform_cfgs,
    );

    let toolchain_deps_configured: Vec<_> = toolchain_deps
        .iter()
        .map(|t| configuration_ctx.configure_toolchain_target(t))
        .collect();

    let execution_constraints = ExecutionPlatformConstraints::new_constraints(
        exec_deps
            .iter()
            .map(|label| label.target().dupe())
            .collect(),
        toolchain_deps_configured
            .iter()
            .map(|dep| TargetConfiguredTargetLabel::new_without_exec_cfg(dep.target().dupe()))
            .collect(),
        exec_compatible_with,
    );

    let resolved_execution = execution_constraints.one_for_cell(ctx, cell).await?;

    let exec_deps_configured = exec_deps.try_map(|e| {
        let label =
            e.configure_pair_no_exec(resolved_execution.platform()?.cfg_pair_no_exec().dupe());
        anyhow::Ok(label)
    })?;

    Ok(BxlExecutionResolution {
        resolved_execution,
        exec_deps_configured,
        toolchain_deps_configured,
    })
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Allocative)]
pub(crate) struct BxlExecutionResolution {
    pub(crate) resolved_execution: ExecutionPlatformResolution,
    pub(crate) exec_deps_configured: Vec<ConfiguredProvidersLabel>,
    pub(crate) toolchain_deps_configured: Vec<ConfiguredProvidersLabel>,
}

impl BxlExecutionResolution {
    pub(crate) fn unspecified() -> BxlExecutionResolution {
        BxlExecutionResolution {
            resolved_execution: ExecutionPlatformResolution::unspecified(),
            exec_deps_configured: Vec::new(),
            toolchain_deps_configured: Vec::new(),
        }
    }
}

pub(crate) fn validate_action_instantiation(
    this: &BxlContextNoDice<'_>,
    bxl_execution_resolution: &BxlExecutionResolution,
) -> anyhow::Result<()> {
    let mut registry = this.state.state.borrow_mut();

    if (*registry).is_some() {
        return Err(anyhow::anyhow!(BxlActionsError::RegistryAlreadyCreated));
    } else {
        let execution_platform = bxl_execution_resolution.resolved_execution.clone();
        let analysis_registry = AnalysisRegistry::new_from_owner(
            this.current_bxl()
                .dupe()
                .into_base_deferred_key(bxl_execution_resolution.clone()),
            execution_platform,
        )?;

        *registry = Some(analysis_registry);
    }

    Ok(())
}

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    StarlarkDocs,
    Allocative
)]
#[starlark_docs(directory = "bxl")]
#[derivative(Debug)]
#[display("{:?}", self)]
pub(crate) struct BxlActions<'v> {
    actions: ValueTyped<'v, AnalysisActions<'v>>,
    exec_deps: ValueOfUnchecked<'v, DictType<StarlarkProvidersLabel, Dependency<'v>>>,
    toolchains: ValueOfUnchecked<'v, DictType<StarlarkProvidersLabel, Dependency<'v>>>,
}

impl<'v> BxlActions<'v> {
    pub(crate) async fn new<'c>(
        actions: ValueTyped<'v, AnalysisActions<'v>>,
        exec_deps: Vec<ConfiguredProvidersLabel>,
        toolchains: Vec<ConfiguredProvidersLabel>,
        eval: &mut Evaluator<'v, '_, '_>,
        ctx: &'c mut DiceComputations<'_>,
    ) -> anyhow::Result<BxlActions<'v>> {
        let exec_deps = alloc_deps(exec_deps, eval, ctx).await?;
        let toolchains = alloc_deps(toolchains, eval, ctx).await?;
        Ok(Self {
            actions,
            exec_deps,
            toolchains,
        })
    }
}

async fn alloc_deps<'v, 'c>(
    deps: Vec<ConfiguredProvidersLabel>,
    eval: &mut Evaluator<'v, '_, '_>,
    ctx: &'c mut DiceComputations<'_>,
) -> anyhow::Result<ValueOfUnchecked<'v, DictType<StarlarkProvidersLabel, Dependency<'v>>>> {
    let analysis_results: Vec<_> = ctx
        .try_compute_join(deps, |ctx, target| {
            async move {
                let res = ctx
                    .get_analysis_result(target.target())
                    .await?
                    .require_compatible()?;
                anyhow::Ok((target, res))
            }
            .boxed()
        })
        .await?;

    let deps: Vec<(StarlarkProvidersLabel, Dependency)> = analysis_results
        .into_iter()
        .map(|(configured, analysis_result)| {
            let v = analysis_result.lookup_inner(&configured)?;

            let starlark_label = StarlarkProvidersLabel::new(configured.unconfigured());
            let dependency = Dependency::new(
                eval.heap(),
                configured,
                v.value().owned_frozen_value_typed(eval.frozen_heap()),
                None,
            );

            anyhow::Ok((starlark_label, dependency))
        })
        .collect::<Result<_, _>>()?;

    Ok(eval.heap().alloc_typed_unchecked(AllocDict(deps)).cast())
}

#[starlark_value(type = "bxl.Actions", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for BxlActions<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(bxl_actions_methods)
    }
}

impl<'v> AllocValue<'v> for BxlActions<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

/// The bxl action context is the context for creating actions. This context is obtained after
/// performing execution platform resolution based on a set of given dependencies and toolchains.
///
/// You can access the analysis actions to create actions, and the resolved dependencies and
/// toolchains from this context
#[starlark_module]
fn bxl_actions_methods(builder: &mut MethodsBuilder) {
    /// Gets the analysis action context to create and register actions on the execution platform
    /// corresponding to this bxl action's execution platform resolution.
    #[starlark(attribute)]
    fn actions<'v>(this: &'v BxlActions) -> anyhow::Result<ValueTyped<'v, AnalysisActions<'v>>> {
        Ok(this.actions)
    }

    /// Gets the execution deps requested correctly configured for the current execution platform
    #[starlark(attribute)]
    fn exec_deps<'v>(
        this: &'v BxlActions,
    ) -> anyhow::Result<ValueOfUnchecked<'v, DictType<StarlarkProvidersLabel, Dependency<'v>>>>
    {
        Ok(this.exec_deps)
    }

    /// Gets the toolchains requested configured for the current execution platform
    #[starlark(attribute)]
    fn toolchains<'v>(
        this: &'v BxlActions,
    ) -> anyhow::Result<ValueOfUnchecked<'v, DictType<StarlarkProvidersLabel, Dependency<'v>>>>
    {
        Ok(this.toolchains)
    }
}
