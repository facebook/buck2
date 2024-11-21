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
use buck2_build_api::transition::TransitionAttrProvider;
use buck2_build_api::transition::TRANSITION_ATTRS_PROVIDER;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_error::starlark_error::from_starlark;
use buck2_interpreter::load_module::InterpreterCalculation;
use dice::DiceComputations;
use dupe::OptionDupedExt;
use starlark::values::OwnedFrozenValueTyped;

use crate::transition::starlark::FrozenTransition;

/// Fetch transition object (function plus context) by id.
#[async_trait]
pub(crate) trait FetchTransition {
    /// Fetch transition object by id.
    async fn fetch_transition(
        &mut self,
        id: &TransitionId,
    ) -> buck2_error::Result<OwnedFrozenValueTyped<FrozenTransition>>;
}

#[derive(Debug, buck2_error::Error)]
enum FetchTransitionError {
    #[error("Transition object not found by id {:?}", _0)]
    NotFound(TransitionId),
}

#[async_trait]
impl FetchTransition for DiceComputations<'_> {
    async fn fetch_transition(
        &mut self,
        id: &TransitionId,
    ) -> buck2_error::Result<OwnedFrozenValueTyped<FrozenTransition>> {
        let module = self.get_loaded_module_from_import_path(&id.path).await?;
        let transition = module
            .env()
            // This is a hashmap lookup, so we are not caching the result in DICE.
            .get_any_visibility(&id.name)
            .map_err(|_| buck2_error::Error::new(FetchTransitionError::NotFound(id.clone())))?
            .0;

        transition.downcast_starlark().map_err(from_starlark)
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
        let transition = ctx.fetch_transition(transition_id).await?;
        Ok(transition.attrs_names.as_ref().duped())
    }
}

pub(crate) fn init_transition_attr_provider() {
    TRANSITION_ATTRS_PROVIDER.init(&TransitionGetAttrs);
}
