/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    fmt::{Debug, Display, Formatter},
    hash::Hash,
    sync::Arc,
};

use buck2_core::target::{ConfiguredTargetLabel, TargetLabel};
use gazebo::prelude::*;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum CompatibilityErrors {
    #[error("{}", .0)]
    TargetIncompatible(IncompatiblePlatformReason),
}

pub trait MaybeCompatibleTypeConstraints: Clone + Dupe + Debug {}
impl<T: Clone + Dupe + Debug> MaybeCompatibleTypeConstraints for T {}

/// MaybeCompatible is used to gracefully deal with things that are incompatible
/// with the target platform. The main place this comes up is that targets provided on the
/// cli may be incompatible with the default or requested platform and we want to skip
/// building those rather than have it be an error.
#[derive(Clone, Dupe, Debug)]
pub enum MaybeCompatible<T: MaybeCompatibleTypeConstraints> {
    Incompatible(Arc<IncompatiblePlatformReason>),
    Compatible(T),
}

impl<T: MaybeCompatibleTypeConstraints> From<T> for MaybeCompatible<T> {
    fn from(res: T) -> Self {
        Self::Compatible(res)
    }
}

impl<T: MaybeCompatibleTypeConstraints + Eq> Eq for MaybeCompatible<T> {}

impl<T: MaybeCompatibleTypeConstraints + PartialEq> PartialEq for MaybeCompatible<T> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (MaybeCompatible::Incompatible(l), MaybeCompatible::Incompatible(r)) => l.eq(r),
            (MaybeCompatible::Compatible(l), MaybeCompatible::Compatible(r)) => l.eq(r),
            _ => false,
        }
    }
}
impl<T: MaybeCompatibleTypeConstraints + Hash> Hash for MaybeCompatible<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            MaybeCompatible::Incompatible(v) => v.hash(state),
            MaybeCompatible::Compatible(v) => v.hash(state),
        }
    }
}

impl<T: MaybeCompatibleTypeConstraints> MaybeCompatible<T> {
    /// Converts to a result. Incompatible values get converted to an error.
    ///
    /// This is just a convencience for treating incompatibility as an error.
    pub fn require_compatible(self) -> anyhow::Result<T> {
        match self {
            MaybeCompatible::Incompatible(reason) => Err(reason.to_err().into()),
            MaybeCompatible::Compatible(result) => Ok(result),
        }
    }

    pub fn is_compatible(&self) -> bool {
        matches!(self, Self::Compatible(..))
    }

    pub fn map<U: MaybeCompatibleTypeConstraints>(
        self,
        func: impl FnOnce(T) -> U,
    ) -> MaybeCompatible<U> {
        match self {
            MaybeCompatible::Incompatible(e) => MaybeCompatible::Incompatible(e),
            MaybeCompatible::Compatible(t) => MaybeCompatible::Compatible(func(t)),
        }
    }

    pub fn try_map<E, U: MaybeCompatibleTypeConstraints>(
        self,
        func: impl FnOnce(T) -> Result<U, E>,
    ) -> Result<MaybeCompatible<U>, E> {
        match self {
            MaybeCompatible::Incompatible(e) => Ok(MaybeCompatible::Incompatible(e)),
            MaybeCompatible::Compatible(t) => Ok(MaybeCompatible::Compatible(func(t)?)),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct IncompatiblePlatformReason {
    pub root_incompatible_target: ConfiguredTargetLabel,
    pub unsatisfied_config: TargetLabel,
}

impl IncompatiblePlatformReason {
    pub fn to_err(&self) -> CompatibilityErrors {
        CompatibilityErrors::TargetIncompatible(self.clone())
    }

    pub fn skipping_message(&self, target: &ConfiguredTargetLabel) -> String {
        format!("Skipping target incompatible node `{}`", target)
    }
}

impl Display for IncompatiblePlatformReason {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} incompatible in configuration {} ({} unsatisfied)",
            self.root_incompatible_target.unconfigured(),
            self.root_incompatible_target.cfg(),
            &self.unsatisfied_config
        )
    }
}
