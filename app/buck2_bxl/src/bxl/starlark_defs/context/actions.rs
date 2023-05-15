/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Starlark Actions API for bxl functions
//!

use std::collections::HashMap;

use allocative::Allocative;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::configuration::calculation::ConfigurationCalculation;
use buck2_build_api::deferred::base_deferred_key::BaseDeferredKey;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use buck2_build_api::interpreter::rule_defs::provider::dependency::Dependency;
use buck2_build_api::nodes::calculation::ExecutionPlatformConstraints;
use buck2_core::cells::name::CellName;
use buck2_core::collections::ordered_map::OrderedMap;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::pair::ConfigurationNoExec;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::TargetLabel;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::attrs::configuration_context::AttrConfigurationContextImpl;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::environment::Module;
use starlark::starlark_module;
use starlark::starlark_type;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;
use starlark::StarlarkDocs;
use thiserror::Error;

use crate::bxl::starlark_defs::context::BxlContext;

#[derive(Debug, Error)]
enum BxlActionsError {
    #[error(
        "An action registry was already requested via `actions_factory`. Only one action registry is allowed"
    )]
    RegistryAlreadyCreated,
}

#[allow(unused)]
pub(crate) async fn resolve_bxl_execution_platform<'v>(
    ctx: &'v DiceComputations,
    cell: CellName,
    exec_deps: Vec<ProvidersLabel>,
    toolchain_deps: Vec<ProvidersLabel>,
    target_platform: Option<TargetLabel>,
    exec_compatible_with: Vec<TargetLabel>,
    module: &'v Module,
) -> anyhow::Result<BxlExecutionResolution<'v>> {
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
        ctx.get_resolved_configuration(&platform_configuration, cell, &exec_compatible_with)
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
    let mut toolchain_deps_configured = HashMap::new();

    for dep in toolchain_deps.into_iter() {
        let configured = configuration_ctx.configure_toolchain_target(&dep);

        let dependency = get_dependency_for_label(configured, ctx, module).await?;

        toolchain_deps_configured.insert(dep, dependency);
    }

    let execution_constraints = ExecutionPlatformConstraints::new_constraints(
        exec_deps
            .iter()
            .map(|label| label.target().dupe())
            .collect(),
        toolchain_deps_configured
            .values()
            .map(|dep| dep.label().inner().target().clone())
            .collect(),
        exec_compatible_with,
    );

    let resolved_execution = execution_constraints.one_for_cell(ctx, cell).await?;

    let mut exec_deps_configured = HashMap::new();

    for exec_dep in exec_deps.into_iter() {
        let configured = exec_dep
            .configure_pair_no_exec(resolved_execution.platform()?.cfg_pair_no_exec().dupe());

        let dependency = get_dependency_for_label(configured, ctx, module).await?;

        exec_deps_configured.insert(exec_dep, dependency);
    }

    Ok(BxlExecutionResolution {
        resolved_execution,
        exec_deps_configured,
        toolchain_deps_configured,
    })
}

async fn get_dependency_for_label<'v>(
    configured: ConfiguredProvidersLabel,
    ctx: &'v DiceComputations,
    module: &'v Module,
) -> anyhow::Result<Dependency<'v>> {
    let analysis_result = ctx
        .get_analysis_result(configured.target())
        .await?
        .require_compatible()?;

    let v = analysis_result.lookup_inner(&configured)?;

    let dependency = Dependency::new(
        module.heap(),
        configured,
        v.value().owned_value(module.frozen_heap()),
    );

    Ok(dependency)
}

pub(crate) struct BxlExecutionResolution<'v> {
    pub(crate) resolved_execution: ExecutionPlatformResolution,
    pub(crate) exec_deps_configured: HashMap<ProvidersLabel, Dependency<'v>>,
    pub(crate) toolchain_deps_configured: HashMap<ProvidersLabel, Dependency<'v>>,
}

pub(crate) fn validate_action_instantiation<'v>(
    this: &BxlContext<'v>,
    execution_platform: ExecutionPlatformResolution,
) -> anyhow::Result<()> {
    let mut registry = this.state.state.borrow_mut();

    if (*registry).is_some() {
        return Err(anyhow::anyhow!(BxlActionsError::RegistryAlreadyCreated));
    } else {
        let analysis_registry = AnalysisRegistry::new_from_owner(
            BaseDeferredKey::BxlLabel(this.current_bxl.dupe()),
            execution_platform,
        );

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
#[display(fmt = "{:?}", self)]
pub(crate) struct BxlActions<'v> {
    actions: ValueTyped<'v, AnalysisActions<'v>>,
    /// A Dict
    exec_deps: Value<'v>,
    /// A dict
    toolchains: Value<'v>,
}

impl<'v> BxlActions<'v> {
    pub(crate) fn new(
        actions: ValueTyped<'v, AnalysisActions<'v>>,
        exec_deps: Value<'v>,
        toolchains: Value<'v>,
    ) -> Self {
        Self {
            actions,
            exec_deps,
            toolchains,
        }
    }
}

impl<'v> StarlarkValue<'v> for BxlActions<'v> {
    starlark_type!("bxl_actions");

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

impl<'v> StarlarkTypeRepr for &'v BxlActions<'v> {
    fn starlark_type_repr() -> String {
        BxlActions::get_type_value_static().as_str().to_owned()
    }
}

impl<'v> UnpackValue<'v> for &'v BxlActions<'v> {
    fn unpack_value(x: Value<'v>) -> Option<&'v BxlActions<'v>> {
        x.downcast_ref()
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
    #[starlark(attribute, return_type = "\"dict\"")]
    fn exec_deps<'v>(this: &'v BxlActions) -> anyhow::Result<Value<'v>> {
        Ok(this.exec_deps)
    }

    /// Gets the toolchains requested configured for the current execution platform
    #[starlark(attribute, return_type = "\"dict\"")]
    fn toolchains<'v>(this: &'v BxlActions) -> anyhow::Result<Value<'v>> {
        Ok(this.toolchains)
    }
}
