/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt::Write;
use std::hash::Hash;
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use pagable::Pagable;
use strong_hash::StrongHash;

use crate::provider::label::ProvidersLabel;
use crate::target::configured_target_label::ConfiguredTargetLabel;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = CompatibilityError)]
enum CompatibilityErrors {
    /// Target is immediately incompatible with the configuration.
    #[error("{0:#}")]
    #[buck2(tag = TargetIncompatible)]
    TargetIncompatible(IncompatiblePlatformReason),
    /// Target is compatible but a transitive dependency is not.
    #[error("{0:#}")]
    #[buck2(tag = DepOnlyIncompatible)]
    DepOnlyIncompatible(IncompatiblePlatformReason),
    // We just need this so that the soft error doesn't print a wall of text to stderr
    // TODO(scottcao): Delete this once we are done with migration and made this a hard error
    // everywhere
    #[error(
        "{0:#} does not pass compatibility check (will be error in future) because its transitive dep {1:#}"
    )]
    #[buck2(tag = DepOnlyIncompatible)]
    DepOnlyIncompatibleSoftError(ConfiguredTargetLabel, IncompatiblePlatformReason),
}

/// MaybeCompatible is used to gracefully deal with things that are incompatible
/// with the target platform. The main place this comes up is that targets provided on the
/// cli may be incompatible with the default or requested platform, and we want to skip
/// building those rather than have it be an error.
#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash, Allocative)]
pub enum MaybeCompatible<T> {
    Incompatible(Arc<IncompatiblePlatformReason>),
    Compatible(T),
}

impl<T> MaybeCompatible<T> {
    /// Converts to a result. Incompatible values get converted to an error.
    ///
    /// This is just a convenience for treating incompatibility as an error.
    pub fn require_compatible(self) -> buck2_error::Result<T> {
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

#[derive(
    Debug, Eq, PartialEq, Hash, StrongHash, Clone, Dupe, Allocative, Pagable
)]
pub enum IncompatiblePlatformReasonCause {
    /// Target is incompatible because of unsatisfied config setting.
    UnsatisfiedConfig(ProvidersLabel),
    /// Target is incompatible because dependency is incompatible.
    Dependency(Arc<IncompatiblePlatformReason>),
}

#[derive(
    Debug, Eq, PartialEq, Hash, StrongHash, Clone, Dupe, Allocative, Pagable
)]
pub struct IncompatiblePlatformReason {
    pub target: ConfiguredTargetLabel,
    pub cause: IncompatiblePlatformReasonCause,
}

impl IncompatiblePlatformReason {
    pub fn to_err(&self) -> buck2_error::Error {
        match self.cause {
            IncompatiblePlatformReasonCause::UnsatisfiedConfig(_) => {
                CompatibilityErrors::TargetIncompatible(self.dupe()).into()
            }
            IncompatiblePlatformReasonCause::Dependency(_) => {
                CompatibilityErrors::DepOnlyIncompatible(self.dupe()).into()
            }
        }
    }

    pub fn to_soft_err(&self) -> buck2_error::Error {
        match &self.cause {
            IncompatiblePlatformReasonCause::UnsatisfiedConfig(_) => self.to_err(),
            IncompatiblePlatformReasonCause::Dependency(reason) => {
                let root_cause = reason.get_root_cause();
                CompatibilityErrors::DepOnlyIncompatibleSoftError(
                    self.target.dupe(),
                    root_cause.dupe(),
                )
                .into()
            }
        }
    }

    fn get_root_cause(&self) -> &IncompatiblePlatformReason {
        // Recurse until we find the root UnsatisfiedConfig error that caused incompatibility errors
        match &self.cause {
            IncompatiblePlatformReasonCause::UnsatisfiedConfig(_) => self,
            IncompatiblePlatformReasonCause::Dependency(reason) => reason.get_root_cause(),
        }
    }

    pub fn skipping_message(&self, target: &ConfiguredTargetLabel) -> String {
        format!("Skipping target incompatible node `{target}`")
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
                writeln!(message, "  {target}").unwrap();
            }
        } else {
            for target in incompatible_targets.iter().take(3) {
                writeln!(message, "  {target}").unwrap();
            }
            writeln!(message, "  ...").unwrap();
            for target in incompatible_targets.iter().rev().take(3).rev() {
                writeln!(message, "  {target}").unwrap();
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
                // WARN: CI uses this message to filter targets
                // If you change this message, please also update https://fburl.com/code/nvdg28nv
                "{}\n    is incompatible with {} ({} unsatisfied), check the target's compatibility attributes",
                self.target.unconfigured(),
                self.target.cfg(),
                unsatisfied_config,
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
    use crate::configuration::compatibility::IncompatiblePlatformReason;
    use crate::configuration::data::ConfigurationData;
    use crate::target::label::label::TargetLabel;

    #[test]
    fn test_skipping_message_for_multiple() {
        let set = vec![
            TargetLabel::testing_parse("glass//foo:bar")
                .configure(ConfigurationData::testing_new()),
        ];
        assert_eq!(
            format!(
                "Skipped 1 incompatible targets:\n  glass//foo:bar ({})\n",
                ConfigurationData::testing_new()
            ),
            IncompatiblePlatformReason::skipping_message_for_multiple(&set),
        );

        let mut set = Vec::new();
        for i in 0..20 {
            // Test it doesn't crash
            IncompatiblePlatformReason::skipping_message_for_multiple(&set);

            set.push(
                TargetLabel::testing_parse(&format!("plate//foo:bar{i}"))
                    .configure(ConfigurationData::testing_new()),
            );
        }
        assert_eq!(
            format!(
                r"Skipped 20 incompatible targets:
  plate//foo:bar0 ({c})
  plate//foo:bar1 ({c})
  plate//foo:bar10 ({c})
  ...
  plate//foo:bar7 ({c})
  plate//foo:bar8 ({c})
  plate//foo:bar9 ({c})
",
                c = ConfigurationData::testing_new()
            ),
            IncompatiblePlatformReason::skipping_message_for_multiple(&set)
        );
    }
}
