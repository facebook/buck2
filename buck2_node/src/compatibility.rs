/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Write;
use std::hash::Hash;
use std::sync::Arc;

use buck2_core::target::ConfiguredTargetLabel;
use buck2_core::target::TargetLabel;
use gazebo::prelude::*;
use thiserror::Error;

#[derive(Debug, Error)]
enum CompatibilityErrors {
    #[error("{0}")]
    TargetIncompatible(IncompatiblePlatformReason),
}

/// MaybeCompatible is used to gracefully deal with things that are incompatible
/// with the target platform. The main place this comes up is that targets provided on the
/// cli may be incompatible with the default or requested platform and we want to skip
/// building those rather than have it be an error.
#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash)]
pub enum MaybeCompatible<T> {
    Incompatible(Arc<IncompatiblePlatformReason>),
    Compatible(T),
}

impl<T> MaybeCompatible<T> {
    /// Converts to a result. Incompatible values get converted to an error.
    ///
    /// This is just a convencience for treating incompatibility as an error.
    pub fn require_compatible(self) -> anyhow::Result<T> {
        match self {
            MaybeCompatible::Incompatible(reason) => Err(reason.to_err()),
            MaybeCompatible::Compatible(result) => Ok(result),
        }
    }

    pub fn is_compatible(&self) -> bool {
        matches!(self, Self::Compatible(..))
    }

    pub fn map<U>(self, func: impl FnOnce(T) -> U) -> MaybeCompatible<U> {
        match self {
            MaybeCompatible::Incompatible(e) => MaybeCompatible::Incompatible(e),
            MaybeCompatible::Compatible(t) => MaybeCompatible::Compatible(func(t)),
        }
    }

    pub fn try_map<E, U>(
        self,
        func: impl FnOnce(T) -> Result<U, E>,
    ) -> Result<MaybeCompatible<U>, E> {
        match self {
            MaybeCompatible::Incompatible(e) => Ok(MaybeCompatible::Incompatible(e)),
            MaybeCompatible::Compatible(t) => Ok(MaybeCompatible::Compatible(func(t)?)),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Dupe)]
pub enum IncompatiblePlatformReasonCause {
    /// Target is incompatible because of unsatisfied config setting.
    UnsatisfiedConfig(TargetLabel),
    /// Target is incompatible because dependency is incompatible.
    Dependency(Arc<IncompatiblePlatformReason>),
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct IncompatiblePlatformReason {
    pub target: ConfiguredTargetLabel,
    pub cause: IncompatiblePlatformReasonCause,
}

impl IncompatiblePlatformReason {
    pub fn to_err(&self) -> anyhow::Error {
        CompatibilityErrors::TargetIncompatible(self.clone()).into()
    }

    pub fn skipping_message(&self, target: &ConfiguredTargetLabel) -> String {
        format!("Skipping target incompatible node `{}`", target)
    }

    pub fn skipping_message_for_multiple<'t>(
        incompatible_targets: impl IntoIterator<Item = &'t ConfiguredTargetLabel>,
    ) -> String {
        let mut incompatible_targets = incompatible_targets.into_iter().collect::<Vec<_>>();
        incompatible_targets.sort();
        let mut message = String::new();

        writeln!(
            message,
            "Skipped {} incompatible targets:",
            incompatible_targets.len()
        )
        .unwrap();
        if incompatible_targets.len() < 10 {
            for target in incompatible_targets.iter() {
                writeln!(message, "  {}", target).unwrap();
            }
        } else {
            for target in incompatible_targets.iter().take(3) {
                writeln!(message, "  {}", target).unwrap();
            }
            writeln!(message, "  ...").unwrap();
            for target in incompatible_targets.iter().rev().take(3).rev() {
                writeln!(message, "  {}", target).unwrap();
            }
        }
        message
    }
}

impl Display for IncompatiblePlatformReason {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.cause {
            IncompatiblePlatformReasonCause::UnsatisfiedConfig(unsatisfied_config) => write!(
                f,
                "{} incompatible ({} unsatisfied)",
                self.target, unsatisfied_config
            ),
            IncompatiblePlatformReasonCause::Dependency(previous) => {
                if f.alternate() {
                    write!(f, "{}\n-> {:#}", self.target, previous)
                } else {
                    write!(f, "{} -> {}", self.target, previous)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::configuration::Configuration;
    use buck2_core::target::testing::TargetLabelExt;
    use buck2_core::target::TargetLabel;

    use crate::compatibility::IncompatiblePlatformReason;

    #[test]
    fn test_skipping_message_for_multiple() {
        let set =
            vec![TargetLabel::testing_parse("//foo:bar").configure(Configuration::testing_new())];
        assert_eq!(
            "Skipped 1 incompatible targets:\n  //foo:bar (<testing>)\n",
            &IncompatiblePlatformReason::skipping_message_for_multiple(&set)
        );

        let mut set = Vec::new();
        for i in 0..20 {
            // Test it doesn't crash
            IncompatiblePlatformReason::skipping_message_for_multiple(&set);

            set.push(
                TargetLabel::testing_parse(&format!("//foo:bar{}", i))
                    .configure(Configuration::testing_new()),
            );
        }
        assert_eq!(
            r"Skipped 20 incompatible targets:
  //foo:bar0 (<testing>)
  //foo:bar1 (<testing>)
  //foo:bar10 (<testing>)
  ...
  //foo:bar7 (<testing>)
  //foo:bar8 (<testing>)
  //foo:bar9 (<testing>)
",
            &IncompatiblePlatformReason::skipping_message_for_multiple(&set)
        );
    }
}
