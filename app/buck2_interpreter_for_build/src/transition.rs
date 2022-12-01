/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::configuration::transition::id::TransitionId;
use gazebo::any::ProvidesStaticType;
use starlark::values::Value;

#[derive(Debug, thiserror::Error)]
enum TransitionError {
    #[error("cfg parameter is not a transition object: {}", _0)]
    WrongType(String),
}

/// Implemented by starlark transition objects.
pub trait TransitionValue {
    fn transition_id(&self) -> anyhow::Result<Arc<TransitionId>>;
}

unsafe impl<'v> ProvidesStaticType for &'v dyn TransitionValue {
    type StaticType = &'static dyn TransitionValue;
}

pub fn transition_id_from_value(value: Value) -> anyhow::Result<Arc<TransitionId>> {
    match value.request_value::<&dyn TransitionValue>() {
        Some(has) => has.transition_id(),
        None => Err(TransitionError::WrongType(value.to_repr()).into()),
    }
}
