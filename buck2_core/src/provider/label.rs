/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::fmt::Formatter;

use derive_more::Display;
use gazebo::dupe::Dupe;
use serde::Serialize;
use serde::Serializer;
use thiserror::Error;

use crate::ascii_char_set::AsciiCharSet;
use crate::configuration::Configuration;
use crate::target::ConfiguredTargetLabel;
use crate::target::TargetLabel;

#[derive(Display, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
#[display(fmt = "{}", _0)]
pub struct ProviderName(String);

#[derive(Error, Debug)]
#[error(
    "Invalid provider name `{}`. Inner providers names can only contain non-empty alpha numeric characters, and symbols `,`, '=', `-`, `/`, `+` and `_`. No other characters are allowed.",
    _0
)]
struct InvalidProviderName(String);

impl ProviderName {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn new_unchecked(name: String) -> ProviderName {
        ProviderName(name)
    }

    pub fn new(name: String) -> anyhow::Result<ProviderName> {
        Self::verify(&name)?;
        Ok(ProviderName(name))
    }

    fn verify(name: &str) -> anyhow::Result<()> {
        const VALID_CHARS: &str =
            r"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_\\/.=,+-";
        const SET: AsciiCharSet = AsciiCharSet::new(VALID_CHARS);

        if name.is_empty() || name.as_bytes().iter().any(|&b| !SET.contains(b)) {
            return Err(InvalidProviderName(name.to_owned()).into());
        }

        Ok(())
    }
}

///
/// A 'ProvidersName' is an optional String label that refers to the specific
/// set of inner providers of a rule.
/// It should be non-empty alphanumeric characteres, '/', '.', ',', '-','=',
/// and'_' character. All other special characters including spaces are
/// prohibited.
#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum ProvidersName {
    Default,
    Named(Vec<ProviderName>),
    // For some flavors from buck1, we can translate them to ProvidersName::Named
    // as we know that we can implement them as a subtarget. For many flavored targets,
    // we can't do that. For those cases, we parse them to this "UnrecognizedFlavor" so
    // that we can defer any errors related to us not supporting it.
    UnrecognizedFlavor(String),
    // TODO(cjhopman): We should add an InferredNamed for flavors where we infer a name
    // so that we can display them in their original form.
}

impl Default for ProvidersName {
    fn default() -> Self {
        Self::Default
    }
}

impl Display for ProvidersName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ProvidersName::Default => {
                write!(f, "")
            }
            ProvidersName::Named(names) => {
                for name in names {
                    write!(f, "[{}]", name)?;
                }
                Ok(())
            }
            ProvidersName::UnrecognizedFlavor(s) => {
                write!(f, "#{}", s)
            }
        }
    }
}

/// A unconfigured 'ProvidersLabel'. This contains a 'TargetLabel' referring to
/// the node on the static graph that produces this provider, and a 'name' is
/// the 'ProvidersName' referring to the specific set of inner providers of a
/// rule.
#[derive(Clone, Debug, Display, Hash, Eq, PartialEq, Ord, PartialOrd)]
#[display(fmt = "{}{}", target, name)]
pub struct ProvidersLabel {
    target: TargetLabel,
    name: ProvidersName,
}

impl ProvidersLabel {
    pub fn new(target: TargetLabel, name: ProvidersName) -> Self {
        ProvidersLabel { target, name }
    }

    pub fn default_for(target: TargetLabel) -> Self {
        Self::new(target, ProvidersName::Default)
    }

    pub fn into_parts(self) -> (TargetLabel, ProvidersName) {
        (self.target, self.name)
    }

    pub fn target(&self) -> &TargetLabel {
        &self.target
    }

    pub fn name(&self) -> &ProvidersName {
        &self.name
    }

    /// Creates a 'ConfiguredProvidersLabel' from ['Self'] based on the provided
    /// configuration.
    pub fn configure(&self, cfg: Configuration) -> ConfiguredProvidersLabel {
        ConfiguredProvidersLabel {
            target: self.target.configure(cfg),
            name: self.name.clone(),
        }
    }

    /// Like `configure`, but forces the execution configuration too.
    pub fn configure_with_exec(
        &self,
        cfg: Configuration,
        exec_cfg: Configuration,
    ) -> ConfiguredProvidersLabel {
        ConfiguredProvidersLabel {
            target: self.target.configure_with_exec(cfg, exec_cfg),
            name: self.name.clone(),
        }
    }

    /// Determines whether a string, **IF IT IS LATER COERCED** would be a relative label.
    ///
    /// This function **DOES NOT** validate the entire label string.
    pub fn maybe_relative_label(raw_label: &str) -> bool {
        raw_label.starts_with(':')
    }
}

impl Serialize for ProvidersLabel {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

///
/// A configured 'ProvidersLabel'.
#[derive(Clone, Debug, Display, Hash, Eq, PartialEq, Ord, PartialOrd)]
#[display(fmt = "{}{} ({})", "target.unconfigured()", "name", "target.cfg()")]
pub struct ConfiguredProvidersLabel {
    target: ConfiguredTargetLabel,
    name: ProvidersName,
}

impl ConfiguredProvidersLabel {
    pub fn new(target: ConfiguredTargetLabel, name: ProvidersName) -> Self {
        Self { target, name }
    }

    pub fn target(&self) -> &ConfiguredTargetLabel {
        &self.target
    }

    pub fn cfg(&self) -> &Configuration {
        self.target.cfg()
    }

    pub fn unconfigured(&self) -> ProvidersLabel {
        ProvidersLabel::new(self.target.unconfigured().dupe(), self.name.clone())
    }

    pub fn name(&self) -> &ProvidersName {
        &self.name
    }
}

impl Serialize for ConfiguredProvidersLabel {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

/// [`ProvidersLabel`] or [`ConfiguredProvidersLabel`].
pub trait ProvidersLabelMaybeConfigured {}

impl ProvidersLabelMaybeConfigured for ProvidersLabel {}
impl ProvidersLabelMaybeConfigured for ConfiguredProvidersLabel {}

pub mod testing {
    use gazebo::prelude::*;

    use super::*;
    use crate::package::testing::*;
    use crate::package::Package;
    use crate::target::TargetLabel;
    use crate::target::TargetName;

    pub trait ProvidersLabelTestExt {
        fn testing_new(
            cell: &str,
            package: &str,
            target: &str,
            name: Option<&[&str]>,
        ) -> ProvidersLabel;
    }

    impl ProvidersLabelTestExt for ProvidersLabel {
        fn testing_new(
            cell: &str,
            package: &str,
            target: &str,
            name: Option<&[&str]>,
        ) -> ProvidersLabel {
            ProvidersLabel::new(
                TargetLabel::new(
                    Package::testing_new(cell, package),
                    TargetName::new(target).unwrap(),
                ),
                match name {
                    Some(n) => {
                        ProvidersName::Named(n.map(|s| ProviderName::new((*s).to_owned()).unwrap()))
                    }
                    _ => ProvidersName::Default,
                },
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::provider::label::ProviderName;
    use crate::provider::label::ProvidersLabel;

    #[test]
    fn providers_name_validation() {
        ProviderName::new("foo".to_owned()).unwrap();
        ProviderName::new("foo_-,.=+/1".to_owned()).unwrap();
        assert!(ProviderName::new("foo bar".to_owned()).is_err());
        assert!(ProviderName::new("foo@bar".to_owned()).is_err());
    }

    #[test]
    fn providers_label_maybe_relative() {
        assert!(ProvidersLabel::maybe_relative_label(":foo"));
        assert!(ProvidersLabel::maybe_relative_label(":foo[bar]"));
        assert!(ProvidersLabel::maybe_relative_label(":invalid@label"));
        assert_eq!(ProvidersLabel::maybe_relative_label("root//:bar"), false);
        assert_eq!(ProvidersLabel::maybe_relative_label("root//foo:foo"), false);
        assert_eq!(
            ProvidersLabel::maybe_relative_label("root//foo:invalid@label"),
            false
        );
    }
}
