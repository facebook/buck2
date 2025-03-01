/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use async_trait::async_trait;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::transition::TransitionAttrProvider;
use buck2_build_api::transition::TRANSITION_ATTRS_PROVIDER;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_interpreter::load_module::InterpreterCalculation;
use dice::DiceComputations;
use dice::Key;
use either::Either;
use ref_cast::RefCast;
use starlark::values::FrozenStringValue;
use starlark::values::OwnedFrozenValueTyped;

use crate::transition::provider::FrozenTransitionInfo;
use crate::transition::starlark::FrozenTransition;

pub(crate) enum TransitionData {
    MagicObject(OwnedFrozenValueTyped<FrozenTransition>),
    Target(OwnedFrozenValueTyped<FrozenTransitionInfo>),
}

impl TransitionData {
    pub(crate) fn refs(
        &self,
    ) -> impl Iterator<Item = (&FrozenStringValue, &TargetLabel)> + Send + Sync {
        match self {
            TransitionData::MagicObject(v) => Either::Left(v.refs.iter()),
            TransitionData::Target(_) => Either::Right([].into_iter()),
        }
        .into_iter()
    }

    pub(crate) fn attr_names(&self) -> Option<impl IntoIterator<Item = &str>> {
        match self {
            TransitionData::MagicObject(v) => Some(Either::Left(
                v.attrs_names.as_ref()?.iter().map(|s| s.as_str()),
            )),
            TransitionData::Target(v) => {
                Some(Either::Right(v.as_ref().get_attrs_names()?.into_iter()))
            }
        }
    }

    pub(crate) fn is_split(&self) -> bool {
        match self {
            TransitionData::MagicObject(v) => v.split,
            TransitionData::Target(_) => false,
        }
    }
}

/// Fetch transition object (function plus context) by id.
#[async_trait]
pub(crate) trait FetchTransition {
    /// Fetch transition object by id.
    async fn fetch_transition(&mut self, id: &TransitionId) -> buck2_error::Result<TransitionData>;
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
enum FetchTransitionError {
    #[error("Transition object not found by id {:?}", _0)]
    NotFound(TransitionId),
    #[error("Expected `{0}` to be a transition target, but it had no `TransitionInfo` provider.")]
    MissingTransitionInfo(ProvidersLabel),
}

#[async_trait]
impl FetchTransition for DiceComputations<'_> {
    async fn fetch_transition(&mut self, id: &TransitionId) -> buck2_error::Result<TransitionData> {
        match id {
            TransitionId::MagicObject { path, name } => {
                let module = self.get_loaded_module_from_import_path(path).await?;
                let transition = module
                    .env()
                    // This is a hashmap lookup, so we are not caching the result in DICE.
                    .get_any_visibility(name)
                    .map_err(|_| {
                        buck2_error::Error::from(FetchTransitionError::NotFound(id.clone()))
                    })?
                    .0;

                Ok(TransitionData::MagicObject(transition.downcast_starlark()?))
            }
            TransitionId::Target(label) => {
                let transition_info = self
                    .get_configuration_analysis_result(label)
                    .await?
                    .value
                    .try_map(|c| {
                        c.as_ref()
                            .builtin_provider_value::<FrozenTransitionInfo>()
                            .ok_or_else(|| {
                                FetchTransitionError::MissingTransitionInfo(label.clone())
                            })
                    })?;
                Ok(TransitionData::Target(transition_info))
            }
        }
    }
}

/// Computes the attributes required by a transition.
///
/// This basically only exists so that we have a lifetime to attach to the `Arc<[String]>`, as we
/// cannot directly return the `FrozenStarlarkStr`s that are actually stored to crates that avoid
/// depending on starlark.
#[derive(
    Debug,
    Eq,
    PartialEq,
    Hash,
    Clone,
    derive_more::Display,
    allocative::Allocative,
    ref_cast::RefCast
)]
#[display("{}", _0)]
#[repr(transparent)]
struct TransitionAttrsKey(TransitionId);

#[async_trait]
impl Key for TransitionAttrsKey {
    type Value = buck2_error::Result<Option<Arc<[String]>>>;

    async fn compute(
        &self,
        ctx: &mut DiceComputations,
        _cancellation: &dice::CancellationContext,
    ) -> Self::Value {
        Ok(ctx
            .fetch_transition(&self.0)
            .await?
            .attr_names()
            .map(|n| n.into_iter().map(|s| s.to_owned()).collect()))
    }

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        if let (Ok(x), Ok(y)) = (x, y) {
            x == y
        } else {
            false
        }
    }
}

struct TransitionGetAttrs;

#[async_trait]
impl TransitionAttrProvider for TransitionGetAttrs {
    async fn transition_attrs(
        &self,
        ctx: &mut DiceComputations<'_>,
        transition_id: &TransitionId,
    ) -> buck2_error::Result<Option<Arc<[String]>>> {
        let k = TransitionAttrsKey::ref_cast(transition_id);
        ctx.compute(k).await?
    }
}

pub(crate) fn init_transition_attr_provider() {
    TRANSITION_ATTRS_PROVIDER.init(&TransitionGetAttrs);
}
