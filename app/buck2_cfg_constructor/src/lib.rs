/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![feature(async_closure)]
#![feature(async_fn_in_trait)]

pub(crate) mod calculation;
pub(crate) mod registration;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_build_api::interpreter::rule_defs::provider::builtin::platform_info::PlatformInfo;
use buck2_core::configuration::data::ConfigurationData;
use buck2_events::dispatch::get_dispatcher;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_node::cfg_constructor::CfgConstructorImpl;
use buck2_node::cfg_constructor::CFG_CONSTRUCTOR_CALCULATION_IMPL;
use calculation::CfgConstructorCalculationInstance;
use dice::DiceComputations;
use starlark::environment::Module;
use starlark::values::OwnedFrozenValue;
use starlark::values::UnpackValue;
use starlark::values::Value;

use crate::registration::init_registration;

#[derive(Allocative, Debug)]
pub(crate) struct CfgConstructor {
    pub(crate) cfg_constructor_pre_constraint_analysis: OwnedFrozenValue,
    pub(crate) cfg_constructor_post_constraint_analysis: OwnedFrozenValue,
}

#[async_trait]
impl CfgConstructorImpl for CfgConstructor {
    async fn eval(
        &self,
        ctx: &DiceComputations,
        cfg: &ConfigurationData,
    ) -> anyhow::Result<ConfigurationData> {
        let module = Module::new();
        let print = EventDispatcherPrintHandler(get_dispatcher());
        with_starlark_eval_provider(
            ctx,
            // TODO: pass proper profiler (T163570348)
            &mut StarlarkProfilerOrInstrumentation::disabled(),
            // TODO: better description
            format!("cfg constructor invocation for cfg: {}", &cfg),
            move |provider, _| -> anyhow::Result<ConfigurationData> {
                let mut eval = provider.make(&module)?;
                eval.set_print_handler(&print);

                // Pre-constraint analysis
                let args = vec![(
                    "platform",
                    // TODO: should eventually accept cli modifiers, target modifiers, and PACKAGE modifiers (T163570597)
                    // and unbound platform case will be handled properly
                    if cfg.is_bound() {
                        eval.heap()
                            .alloc_complex(PlatformInfo::from_configuration(cfg, eval.heap())?)
                    } else {
                        Value::new_none()
                    },
                )];
                let pre_constraint_analysis_result = eval.eval_function(
                    self.cfg_constructor_pre_constraint_analysis.value(),
                    &[],
                    &args,
                )?;
                // Check return type
                drop(<(Vec<String>, Value)>::unpack_value_err(
                    pre_constraint_analysis_result,
                )?);

                // TODO: analysis of constraints (T163226707)

                // Post-constraint analysis
                let post_constraint_analysis_result = eval.eval_function(
                    self.cfg_constructor_post_constraint_analysis.value(),
                    &[],
                    &[("refs", pre_constraint_analysis_result)],
                )?;

                <&PlatformInfo>::unpack_value_err(post_constraint_analysis_result)?
                    .to_configuration()
            },
        )
        .await
    }
}

pub fn init_late_bindings() {
    CFG_CONSTRUCTOR_CALCULATION_IMPL.init(&CfgConstructorCalculationInstance);
    init_registration();
}
