/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;

use crate::configuration::Configuration;

#[derive(thiserror::Error, Debug)]
enum TransitionAppliedError {
    #[error(
        "Transition object is declared split, but transition to one is needed in this context"
    )]
    SplitWhereSingleExpected,
    #[error(
        "Transition object is declared non-split, but split transition is needed in this context"
    )]
    SingleWhereSplitExpected,
}

/// Result of `transition` function application to a configuration.
#[derive(PartialEq, Eq, Hash, Debug)]
pub enum TransitionApplied {
    /// Transition to single configuration.
    Single(Configuration),
    /// Split transition.
    Split(BTreeMap<String, Configuration>),
}

impl TransitionApplied {
    pub fn single(&self) -> anyhow::Result<&Configuration> {
        match self {
            TransitionApplied::Single(configuration) => Ok(configuration),
            _ => Err(TransitionAppliedError::SplitWhereSingleExpected.into()),
        }
    }

    pub fn split(&self) -> anyhow::Result<&BTreeMap<String, Configuration>> {
        match self {
            TransitionApplied::Split(configurations) => Ok(configurations),
            _ => Err(TransitionAppliedError::SingleWhereSplitExpected.into()),
        }
    }
}
