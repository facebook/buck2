/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use async_trait::async_trait;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::interpreter::rule_defs::provider::builtin::platform_info::PlatformInfo;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_build_api::transition::TransitionCalculation;
use buck2_build_api::transition::TRANSITION_CALCULATION;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_common::result::ToUnsharedResultExt;
use buck2_core::configuration::cfg_diff::cfg_diff;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::transition::applied::TransitionApplied;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::target::label::TargetLabel;
use buck2_events::dispatch::get_dispatcher;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::display::AttrDisplayWithContextExt;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_util::collections::ordered_map::OrderedMap;
use buck2_util::collections::sorted_map::SortedMap;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use gazebo::prelude::*;
use itertools::Itertools;
use more_futures::cancellation::CancellationContext;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::dict::DictOf;
use starlark::values::structs::AllocStruct;
use starlark::values::UnpackValue;
use starlark::values::Value;
use thiserror::Error;

use crate::coerced_attr::CoercedAttrResolveExt;
use crate::transition::calculation_fetch_transition::FetchTransition;
use crate::transition::starlark::FrozenTransition;

#[derive(Error, Debug)]
enum ApplyTransitionError {
    #[error("transition function not marked as `split` must return a `PlatformInfo`")]
    NonSplitTransitionMustReturnPlatformInfo,
    #[error("transition function marked `split` must return a dict of `str` to `PlatformInfo`")]
    SplitTransitionMustReturnDict,
    #[error(
        "transition applied again to transition output \
        did not produce identical `PlatformInfo`, the diff:\n{0}"
    )]
    SplitTransitionAgainDifferentPlatformInfo(String),
    #[error(
        "Transition object is not consistent with transition computation params, \
        this may happen because of how DICE recomputation works, \
        a user should never see this message"
    )]
    InconsistentTransitionAndComputation,
}

fn call_transition_function<'v>(
    transition: &FrozenTransition,
    conf: &ConfigurationData,
    refs: Value<'v>,
    attrs: Option<Value<'v>>,
    eval: &mut Evaluator<'v, '_>,
) -> anyhow::Result<TransitionApplied> {
    let mut args = vec![
        (
            "platform",
            eval.heap()
                .alloc_complex(PlatformInfo::from_configuration(conf, eval.heap())?),
        ),
        ("refs", refs),
    ];
    if let Some(attrs) = attrs {
        args.push(("attrs", attrs));
    }
    let new_platforms = eval.eval_function(transition.implementation.to_value(), &[], &args)?;
    if transition.split {
        match DictOf::<&str, &PlatformInfo>::unpack_value(new_platforms) {
            Some(dict) => {
                let mut split = OrderedMap::new();
                for (k, v) in dict.to_dict() {
                    let prev = split.insert(k.to_owned(), v.to_configuration()?);
                    assert!(prev.is_none());
                }
                Ok(TransitionApplied::Split(SortedMap::from(split)))
            }
            None => Err(ApplyTransitionError::SplitTransitionMustReturnDict.into()),
        }
    } else {
        match <&PlatformInfo>::unpack_value(new_platforms) {
            Some(platform) => Ok(TransitionApplied::Single(platform.to_configuration()?)),
            None => Err(ApplyTransitionError::NonSplitTransitionMustReturnPlatformInfo.into()),
        }
    }
}

async fn do_apply_transition(
    ctx: &DiceComputations,
    attrs: Option<&[Option<CoercedAttr>]>,
    conf: &ConfigurationData,
    transition_id: &TransitionId,
) -> SharedResult<TransitionApplied> {
    let transition = ctx.fetch_transition(transition_id).await?;
    let module = Module::new();
    let mut refs = Vec::with_capacity(transition.refs.len());
    let mut refs_refs = Vec::new();
    for (s, t) in &transition.refs {
        let provider_collection_value = ctx.fetch_transition_function_reference(t).await?;
        refs.push((
            *s,
            // This is safe because we store a reference to provider collection in `refs_refs`.
            unsafe { provider_collection_value.value().to_frozen_value() },
        ));
        refs_refs.push(provider_collection_value);
    }
    let print = EventDispatcherPrintHandler(get_dispatcher());
    with_starlark_eval_provider(
        ctx,
        &mut StarlarkProfilerOrInstrumentation::disabled(),
        format!("transition:{}", transition_id),
        move |provider| {
            let mut eval = provider.make(&module)?;
            eval.set_print_handler(&print);
            let refs = module.heap().alloc(AllocStruct(refs));
            let attrs = match (&transition.attrs, attrs) {
                (Some(names), Some(values)) => {
                    if names.len() != values.len() {
                        return Err(
                            ApplyTransitionError::InconsistentTransitionAndComputation.into()
                        );
                    }
                    let mut attrs = Vec::with_capacity(names.len());
                    for (name, value) in names.iter().zip(values.iter()) {
                        let value = match value {
                            Some(value) => value.to_value(module.heap()).with_context(|| {
                                format!(
                                    "Error converting attribute `{}={}` to Starlark value",
                                    name.as_str(),
                                    value.as_display_no_ctx(),
                                )
                            })?,
                            None => Value::new_none(),
                        };
                        attrs.push((*name, value));
                    }
                    Some(module.heap().alloc(AllocStruct(attrs)))
                }
                (None, None) => None,
                (Some(_), None) | (None, Some(_)) => {
                    return Err(ApplyTransitionError::InconsistentTransitionAndComputation.into());
                }
            };
            match call_transition_function(&transition, conf, refs, attrs, &mut eval)? {
                TransitionApplied::Single(new) => {
                    let new_2 =
                        match call_transition_function(&transition, &new, refs, attrs, &mut eval)
                            .context("applying transition again on transition output")
                            .shared_error()?
                        {
                            TransitionApplied::Single(new_2) => new_2,
                            TransitionApplied::Split(_) => {
                                unreachable!(
                                    "split transition filtered out in call_transition_function"
                                )
                            }
                        };
                    if let Err(diff) = cfg_diff(&new, &new_2) {
                        return Err(
                            ApplyTransitionError::SplitTransitionAgainDifferentPlatformInfo(diff)
                                .into(),
                        );
                    }
                    Ok(TransitionApplied::Single(new))
                }
                TransitionApplied::Split(split) => {
                    // Not validating split transitions yet, because it's not 100% clear what to validate,
                    // and because it is not that important, because split transitions
                    // are not used in per-rule transitions.
                    Ok(TransitionApplied::Split(split))
                }
            }
        },
    )
    .await
    .shared_error()
}

#[async_trait]
pub(crate) trait ApplyTransition {
    /// Resolve `refs` param of transition function.
    async fn fetch_transition_function_reference(
        &self,
        target: &TargetLabel,
    ) -> SharedResult<FrozenProviderCollectionValue>;
}

#[async_trait]
impl ApplyTransition for DiceComputations {
    async fn fetch_transition_function_reference(
        &self,
        target: &TargetLabel,
    ) -> SharedResult<FrozenProviderCollectionValue> {
        Ok(self
            .get_configuration_analysis_result(target)
            .await?
            .providers()
            .dupe())
    }
}

struct TransitionCalculationImpl;

pub(crate) fn init_transition_calculation() {
    TRANSITION_CALCULATION.init(&TransitionCalculationImpl);
}

#[async_trait]
impl TransitionCalculation for TransitionCalculationImpl {
    async fn apply_transition(
        &self,
        ctx: &DiceComputations,
        target_node: &TargetNode,
        cfg: &ConfigurationData,
        transition_id: &TransitionId,
    ) -> anyhow::Result<Arc<TransitionApplied>> {
        #[derive(Debug, Eq, PartialEq, Hash, Clone, Display, Allocative)]
        #[display(fmt = "{} ({}){}", transition_id, cfg, "self.fmt_attrs()")]
        struct TransitionKey {
            cfg: ConfigurationData,
            transition_id: TransitionId,
            /// Attributes which requested by transition function, not all attributes.
            /// The attr value index is the index of attribute in transition object.
            /// Attributes are added here so multiple targets with the equal attributes
            /// (e.g. the same `java_version = 14`) share the transition computation.
            attrs: Option<Vec<Option<CoercedAttr>>>,
        }

        impl TransitionKey {
            fn fmt_attrs(&self) -> String {
                if let Some(attrs) = &self.attrs {
                    format!(
                        " [{}]",
                        attrs
                            .iter()
                            .map(|a| {
                                if let Some(attr) = a {
                                    attr.as_display_no_ctx().to_string()
                                } else {
                                    "None".to_owned()
                                }
                            })
                            .join(", ")
                    )
                } else {
                    String::new()
                }
            }
        }

        #[async_trait]
        impl Key for TransitionKey {
            type Value = SharedResult<Arc<TransitionApplied>>;

            async fn compute(
                &self,
                ctx: &DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
                let v: SharedResult<_> = try {
                    do_apply_transition(ctx, self.attrs.as_deref(), &self.cfg, &self.transition_id)
                        .await?
                };

                Ok(Arc::new(v.with_context(|| {
                    format!("Error computing transition `{}`", self)
                })?))
            }

            fn equality(x: &Self::Value, y: &Self::Value) -> bool {
                if let (Ok(x), Ok(y)) = (x, y) {
                    x == y
                } else {
                    false
                }
            }
        }

        let transition = ctx.fetch_transition(transition_id).await?;

        #[allow(clippy::manual_map)]
        let attrs = if let Some(attrs) = &transition.attrs {
            Some(attrs.try_map(|attr| {
                target_node
                    .attr(attr, AttrInspectOptions::All)
                    .map(|o| o.cloned())
            })?)
        } else {
            None
        };

        let key = TransitionKey {
            cfg: cfg.dupe(),
            transition_id: transition_id.clone(),
            attrs,
        };

        ctx.compute(&key).await?.unshared_error()
    }
}
