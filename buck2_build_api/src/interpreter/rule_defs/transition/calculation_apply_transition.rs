/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{collections::BTreeMap, sync::Arc};

use anyhow::Context;
use async_trait::async_trait;
use buck2_core::{
    configuration::Configuration,
    result::{SharedError, SharedResult, ToSharedResultExt},
    target::TargetLabel,
};
use derive_more::Display;
use dice::{DiceComputations, Key};
use gazebo::{dupe::Dupe, prelude::*};
use itertools::Itertools;
use starlark::{
    collections::SmallMap,
    environment::Module,
    eval::Evaluator,
    values::{dict::DictOf, structs::Struct, UnpackValue, Value},
};
use thiserror::Error;

use crate::{
    analysis::calculation::RuleAnalysisCalculation,
    attrs::coerced_attr::CoercedAttr,
    interpreter::rule_defs::{
        provider::{platform_info::PlatformInfo, FrozenProviderCollectionValue},
        transition::{
            applied::TransitionApplied, calculation_fetch_transition::FetchTransition,
            cfg_diff::cfg_diff, id::TransitionId, starlark::FrozenTransition,
        },
    },
    nodes::unconfigured::TargetNode,
};

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
}

fn call_transition_function<'v>(
    transition: &FrozenTransition,
    conf: &Configuration,
    refs: Value<'v>,
    eval: &mut Evaluator<'v, '_>,
) -> anyhow::Result<TransitionApplied> {
    let new_platforms = eval.eval_function(
        transition.implementation.to_value(),
        &[],
        &[
            (
                "platform",
                eval.heap()
                    .alloc_complex(PlatformInfo::from_configuration(conf, eval.heap())?),
            ),
            ("refs", refs),
        ],
    )?;
    if transition.split {
        match DictOf::<&str, &PlatformInfo>::unpack_value(new_platforms) {
            Some(dict) => {
                let mut split = BTreeMap::new();
                for (k, v) in dict.to_dict() {
                    let prev = split.insert(k.to_owned(), v.to_configuration()?);
                    assert!(prev.is_none());
                }
                Ok(TransitionApplied::Split(split))
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
    conf: &Configuration,
    transition_id: &TransitionId,
) -> SharedResult<TransitionApplied> {
    let transition = ctx.fetch_transition(transition_id).await?;
    let module = Module::new();
    let mut refs = SmallMap::new();
    let mut refs_refs = Vec::new();
    for (s, t) in &transition.refs {
        let provider_collection_value = ctx.fetch_transition_function_reference(t).await?;
        refs.insert(
            module.heap().alloc_str(s),
            // This is safe because we store a reference to provider collection in `refs_refs`.
            unsafe { provider_collection_value.value().to_frozen_value() }.to_value(),
        );
        refs_refs.push(provider_collection_value);
    }
    let mut eval = Evaluator::new(&module);
    let refs = module.heap().alloc_complex(Struct::new(refs));
    let _ = attrs; // TODO(nga): used in the following diffs.
    match call_transition_function(&transition, conf, refs, &mut eval).shared_error()? {
        TransitionApplied::Single(new) => {
            let new_2 = match call_transition_function(&transition, &new, refs, &mut eval)
                .context("applying transition again on transition output")
                .shared_error()?
            {
                TransitionApplied::Single(new_2) => new_2,
                TransitionApplied::Split(_) => {
                    unreachable!("split transition filtered out in call_transition_function")
                }
            };
            if let Err(diff) = cfg_diff(&new, &new_2) {
                return Err(SharedError::new(
                    ApplyTransitionError::SplitTransitionAgainDifferentPlatformInfo(diff),
                ));
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
}

#[async_trait]
pub(crate) trait ApplyTransition {
    /// Resolve `refs` param of transition function.
    async fn fetch_transition_function_reference(
        &self,
        target: &TargetLabel,
    ) -> SharedResult<FrozenProviderCollectionValue>;

    /// Apply transition function to configuration and cache the result.
    async fn apply_transition(
        &self,
        target_node: &TargetNode,
        conf: &Configuration,
        transition_id: &TransitionId,
    ) -> SharedResult<Arc<TransitionApplied>>;
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

    async fn apply_transition(
        &self,
        target_node: &TargetNode,
        cfg: &Configuration,
        transition_id: &TransitionId,
    ) -> SharedResult<Arc<TransitionApplied>> {
        #[derive(Debug, Eq, PartialEq, Hash, Clone, Display)]
        #[display(fmt = "{} ({}){}", transition_id, cfg, "self.fmt_attrs()")]
        struct TransitionKey {
            cfg: Configuration,
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
                                    attr.to_string()
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

            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
                let v: SharedResult<_> = try {
                    do_apply_transition(ctx, self.attrs.as_deref(), &self.cfg, &self.transition_id)
                        .await?
                };

                Ok(Arc::new(v.with_context(|| {
                    format!("when computing transition `{}`", self)
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

        let transition = self.fetch_transition(transition_id).await?;

        #[allow(clippy::manual_map)]
        let attrs = if let Some(attrs) = &transition.attrs {
            Some(attrs.try_map(|attr| target_node.attr(attr).map(|o| o.cloned()))?)
        } else {
            None
        };

        let key = TransitionKey {
            cfg: cfg.dupe(),
            transition_id: transition_id.clone(),
            attrs,
        };

        self.compute(&key).await
    }
}
