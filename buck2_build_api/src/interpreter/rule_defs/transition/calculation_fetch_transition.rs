/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
use buck2_core::result::{SharedError, SharedResult};
use buck2_interpreter::{common::StarlarkModulePath, dice::HasCalculationDelegate};
use dice::DiceComputations;
use starlark::values::OwnedFrozenValueTyped;
use thiserror::Error;

use crate::interpreter::rule_defs::transition::{id::TransitionId, starlark::FrozenTransition};

/// Fetch transition object (function plus context) by id.
#[async_trait]
pub trait FetchTransition {
    /// Fetch transition object by id.
    async fn fetch_transition(
        &self,
        id: &TransitionId,
    ) -> SharedResult<OwnedFrozenValueTyped<FrozenTransition>>;
}

#[derive(Debug, Error)]
enum FetchTransitionError {
    #[error("Transition object not found by id {:?}", _0)]
    NotFound(TransitionId),
    #[error("Transition id {:?} is resolved to not transition object", _0)]
    NotTransition(TransitionId),
}

#[async_trait]
impl FetchTransition for DiceComputations {
    async fn fetch_transition(
        &self,
        id: &TransitionId,
    ) -> SharedResult<OwnedFrozenValueTyped<FrozenTransition>> {
        let calculation = self
            .get_interpreter_calculator(id.path.cell(), id.path.build_file_cell())
            .await?;
        let module = calculation
            .eval_module(StarlarkModulePath::LoadFile(&id.path))
            .await?;
        let transition = module
            .env()
            // This is a hashmap lookup, so we are not caching the result in DICE.
            .get_any_visibility(&id.name)
            .ok_or_else(|| SharedError::new(FetchTransitionError::NotFound(id.clone())))?
            .0;

        Ok(transition
            .downcast()
            .map_err(|_| SharedError::new(FetchTransitionError::NotTransition(id.clone())))?)
    }
}
