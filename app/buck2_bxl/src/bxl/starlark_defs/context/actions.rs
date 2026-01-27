/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Starlark Actions API for bxl functions
use std::sync::Arc;

use allocative::Allocative;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::interpreter::rule_defs::provider::dependency::Dependency;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::pair::Configuration;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::deferred::key::DeferredHolderKey;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::soft_error;
use buck2_core::target::label::label::TargetLabel;
use buck2_core::target::target_configured_target_label::TargetConfiguredTargetLabel;
use buck2_error::buck2_error;
use buck2_interpreter::types::configured_providers_label::StarlarkProvidersLabel;
use buck2_node::configuration::calculation::CONFIGURATION_CALCULATION;
use buck2_node::configuration::calculation::CellNameForConfigurationResolution;
use buck2_node::configuration::resolved::ConfigurationSettingKey;
use buck2_node::execution::GET_EXECUTION_PLATFORMS;
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
use starlark::starlark_module;
use starlark::values::AllocValue;
use starlark::values::FrozenHeap;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueOfUnchecked;
use starlark::values::ValueTyped;
use starlark::values::dict::AllocDict;
use starlark::values::dict::DictType;
use starlark::values::starlark_value;
use strong_hash::StrongHash;

use crate::bxl::starlark_defs::context::BxlContext;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum BxlActionsError {
    #[error(
        "An action registry was already requested via `ctx.bxl_actions().actions`. Only one action registry is allowed"
    )]
    RegistryAlreadyCreated,
}

pub(crate) async fn resolve_bxl_execution_platform(
    ctx: &mut DiceComputations<'_>,
    cell: CellNameForConfigurationResolution,
    exec_deps: Vec<ProvidersLabel>,
    toolchain_deps: Vec<ProvidersLabel>,
    target_platform: Option<TargetLabel>,
    exec_compatible_with: Arc<[ConfigurationSettingKey]>,
) -> buck2_error::Result<BxlExecutionResolution> {
    let target_cfg = match target_platform.as_ref() {
        Some(global_target_platform) => {
            CONFIGURATION_CALCULATION
                .get()?
                .get_platform_configuration(ctx, global_target_platform)
                .await?
        }
        None => ConfigurationData::unspecified(),
    };

    let resolved_execution = GET_EXECUTION_PLATFORMS
        .get()?
        .execution_platform_resolution_one_for_cell(
            ctx,
            exec_deps
                .iter()
                .map(|label| label.target().dupe())
                .collect(),
            toolchain_deps
                .iter()
                .map(|dep| {
                    TargetConfiguredTargetLabel::new_configure(dep.target(), target_cfg.dupe())
                })
                .collect(),
            exec_compatible_with,
            cell,
        )
        .await?;

    let exec_cfg = resolved_execution.platform()?.cfg_pair_no_exec().dupe();
    let toolchain_cfg = Configuration::new(target_cfg.dupe(), Some(exec_cfg.cfg().dupe()));

    let toolchain_deps_configured: Vec<_> = toolchain_deps
        .iter()
        .map(|t| t.configure_pair(toolchain_cfg.dupe()))
        .collect();

    let exec_deps_configured = exec_deps.try_map(|e| {
        let label =
            e.configure_pair_no_exec(resolved_execution.platform()?.cfg_pair_no_exec().dupe());
        buck2_error::Ok(label)
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

impl StrongHash for BxlExecutionResolution {
    fn strong_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        use std::hash::Hash;

        // FIXME(JakobDegen): It seems wrong that we're structurally hashing this entire value
        self.resolved_execution.hash(state);
        self.exec_deps_configured.strong_hash(state);
        self.toolchain_deps_configured.strong_hash(state);
    }
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
    this: &BxlContext<'_>,
    bxl_execution_resolution: &BxlExecutionResolution,
) -> buck2_error::Result<()> {
    let mut registry = this.state.state.borrow_mut();

    if (*registry).is_some() {
        return Err(BxlActionsError::RegistryAlreadyCreated.into());
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
    Allocative
)]
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
        heap: Heap<'v>,
        frozen_heap: &FrozenHeap,
        ctx: &'c mut DiceComputations<'_>,
    ) -> buck2_error::Result<BxlActions<'v>> {
        let exec_deps = alloc_deps(exec_deps, heap, frozen_heap, ctx).await?;
        let toolchains = alloc_deps(toolchains, heap, frozen_heap, ctx).await?;
        Ok(Self {
            actions,
            exec_deps,
            toolchains,
        })
    }

    fn is_anon_target_or_dyn_action(&self) -> buck2_error::Result<bool> {
        let key = &self.actions.state()?.analysis_value_storage.self_key;
        Ok(match key {
            DeferredHolderKey::Base(base_deferred_key) => match base_deferred_key {
                BaseDeferredKey::AnonTarget(_) => true,
                BaseDeferredKey::TargetLabel(_) => false,
                BaseDeferredKey::BxlLabel(_) => false,
            },
            DeferredHolderKey::DynamicLambda(_) => true,
        })
    }
}

async fn alloc_deps<'v, 'c>(
    deps: Vec<ConfiguredProvidersLabel>,
    heap: Heap<'v>,
    frozen_heap: &FrozenHeap,
    ctx: &'c mut DiceComputations<'_>,
) -> buck2_error::Result<ValueOfUnchecked<'v, DictType<StarlarkProvidersLabel, Dependency<'v>>>> {
    let analysis_results: Vec<_> = ctx
        .try_compute_join(deps, |ctx, target| {
            async move {
                let res = ctx
                    .get_analysis_result(target.target())
                    .await?
                    .require_compatible()?;
                buck2_error::Ok((target, res))
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
                heap,
                configured,
                v.value().owned_frozen_value_typed(frozen_heap),
                None,
            );

            buck2_error::Ok((starlark_label, dependency))
        })
        .collect::<Result<_, _>>()?;

    Ok(heap.alloc_typed_unchecked(AllocDict(deps)).cast())
}

#[starlark_value(type = "bxl.Actions", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for BxlActions<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(bxl_actions_methods)
    }
}

impl<'v> AllocValue<'v> for BxlActions<'v> {
    fn alloc_value(self, heap: Heap<'v>) -> Value<'v> {
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
    fn actions<'v>(this: &BxlActions<'v>) -> starlark::Result<ValueTyped<'v, AnalysisActions<'v>>> {
        Ok(this.actions)
    }

    /// Gets the execution deps requested correctly configured for the current execution platform
    #[starlark(attribute)]
    fn exec_deps<'v>(
        this: &BxlActions<'v>,
    ) -> starlark::Result<ValueOfUnchecked<'v, DictType<StarlarkProvidersLabel, Dependency<'v>>>>
    {
        if this.is_anon_target_or_dyn_action()? {
            soft_error!(
                "bxl_acessing_exec_platform",
                buck2_error!(buck2_error::ErrorTag::Input, "Anon target or dynamic action accesses bxl.Actions.exec_deps."),
                quiet: true
            )?;
        }

        Ok(this.exec_deps)
    }

    /// Gets the toolchains requested configured for the current execution platform
    #[starlark(attribute)]
    fn toolchains<'v>(
        this: &BxlActions<'v>,
    ) -> starlark::Result<ValueOfUnchecked<'v, DictType<StarlarkProvidersLabel, Dependency<'v>>>>
    {
        if this.is_anon_target_or_dyn_action()? {
            soft_error!(
                "bxl_acessing_exec_platform",
                buck2_error!(buck2_error::ErrorTag::Input, "Anon target or dynamic action accesses bxl.Actions.toolchains."),
                quiet: true
            )?;
        }
        Ok(this.toolchains)
    }
}
