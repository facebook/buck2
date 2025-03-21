/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Formatter;
use std::iter;

use allocative::Allocative;
use buck2_util::arc_str::ArcSlice;
use buck2_util::arc_str::ArcStr;
use derive_more::Display;
use dupe::Dupe;
use serde::Serialize;
use serde::Serializer;
use static_assertions::assert_eq_size;
use triomphe::Arc;

use crate::ascii_char_set::AsciiCharSet;
use crate::cells::name::CellName;
use crate::cells::CellAliasResolver;
use crate::cells::CellResolver;
use crate::configuration::data::ConfigurationData;
use crate::configuration::pair::Configuration;
use crate::configuration::pair::ConfigurationNoExec;
use crate::pattern::pattern::ParsedPattern;
use crate::pattern::pattern_type::ProvidersPatternExtra;
use crate::target::configured_target_label::ConfiguredTargetLabel;
use crate::target::label::label::TargetLabel;

#[derive(
    Display, Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative
)]
pub struct ProviderName(String);

#[derive(buck2_error::Error, Debug)]
#[error(
    "Invalid provider name `{}`. Inner providers names can only contain non-empty alpha numeric characters, and symbols `,`, `=`, `-`, `/`, `+` and `_`. No other characters are allowed.",
    _0
)]
#[buck2(tag = Input)]
struct InvalidProviderName(String);

impl ProviderName {
    pub fn as_str(&self) -> &str {
        &self.0
    }

    pub fn new_unchecked(name: String) -> ProviderName {
        ProviderName(name)
    }

    pub fn new(name: String) -> buck2_error::Result<ProviderName> {
        Self::verify(&name)?;
        Ok(ProviderName(name))
    }

    fn verify(name: &str) -> buck2_error::Result<()> {
        const VALID_CHARS: &str =
            r"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_\\/.=,+-";
        const SET: AsciiCharSet = AsciiCharSet::new(VALID_CHARS);

        if name.is_empty() || name.as_bytes().iter().any(|&b| !SET.contains(b)) {
            return Err(InvalidProviderName(name.to_owned()).into());
        }

        Ok(())
    }
}

#[derive(Clone, Dupe, Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative)]
pub enum NonDefaultProvidersName {
    Named(ArcSlice<ProviderName>),
    // For some flavors from buck1, we can translate them to ProvidersName::Named
    // as we know that we can implement them as a subtarget. For many flavored targets,
    // we can't do that. For those cases, we parse them to this "UnrecognizedFlavor" so
    // that we can defer any errors related to us not supporting it.
    UnrecognizedFlavor(ArcStr),
    // TODO(cjhopman): We should add an InferredNamed for flavors where we infer a name
    // so that we can display them in their original form.
}

///
/// A 'ProvidersName' is an optional String label that refers to the specific
/// set of inner providers of a rule.
/// It should be non-empty alphanumeric characters, '/', '.', ',', '-','=',
/// and'_' character. All other special characters including spaces are
/// prohibited.
#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative)]
pub enum ProvidersName {
    Default,
    NonDefault(Arc<NonDefaultProvidersName>),
}

assert_eq_size!(ProvidersName, [usize; 1]);

impl Dupe for ProvidersName {}

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
            ProvidersName::NonDefault(flavor) => match flavor.as_ref() {
                NonDefaultProvidersName::Named(names) => {
                    for name in &**names {
                        write!(f, "[{}]", name)?;
                    }
                    Ok(())
                }
                NonDefaultProvidersName::UnrecognizedFlavor(s) => {
                    write!(f, "#{}", s)
                }
            },
        }
    }
}

impl ProvidersName {
    pub fn push(&self, name: ProviderName) -> Self {
        let items = match self {
            ProvidersName::Default => vec![name],
            ProvidersName::NonDefault(x) => match &**x {
                NonDefaultProvidersName::Named(xs) => {
                    xs.iter().cloned().chain(iter::once(name)).collect()
                }
                NonDefaultProvidersName::UnrecognizedFlavor(_) => return self.dupe(),
            },
        };
        ProvidersName::NonDefault(Arc::new(NonDefaultProvidersName::Named(
            ArcSlice::from_iter(items),
        )))
    }
}

/// A unconfigured 'ProvidersLabel'. This contains a 'TargetLabel' referring to
/// the node on the static graph that produces this provider, and a 'name' is
/// the 'ProvidersName' referring to the specific set of inner providers of a
/// rule.
#[derive(
    Clone, Dupe, Debug, Display, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative
)]
#[display("{}{}", target, name)]
pub struct ProvidersLabel {
    target: TargetLabel,
    name: ProvidersName,
}

assert_eq_size!(ProvidersLabel, [usize; 2]);

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

    pub fn parse(
        label: &str,
        cell_name: CellName,
        cell_resolver: &CellResolver,
        cell_alias_resolver: &CellAliasResolver,
    ) -> buck2_error::Result<ProvidersLabel> {
        let providers_label = ParsedPattern::<ProvidersPatternExtra>::parse_precise(
            label,
            cell_name,
            cell_resolver,
            cell_alias_resolver,
        )?
        .as_providers_label(label)?;
        Ok(providers_label)
    }

    /// Creates a 'ConfiguredProvidersLabel' from ['Self'] based on the provided
    /// configuration.
    pub fn configure(&self, cfg: ConfigurationData) -> ConfiguredProvidersLabel {
        ConfiguredProvidersLabel {
            target: self.target.configure(cfg),
            name: self.name.dupe(),
        }
    }

    /// Like `configure`, but forces the execution configuration too.
    pub fn configure_with_exec(
        &self,
        cfg: ConfigurationData,
        exec_cfg: ConfigurationData,
    ) -> ConfiguredProvidersLabel {
        ConfiguredProvidersLabel {
            target: self.target.configure_with_exec(cfg, exec_cfg),
            name: self.name.dupe(),
        }
    }

    #[inline]
    pub fn configure_pair(&self, cfg_pair: Configuration) -> ConfiguredProvidersLabel {
        ConfiguredProvidersLabel {
            target: self.target.configure_pair(cfg_pair),
            name: self.name.dupe(),
        }
    }

    #[inline]
    pub fn configure_pair_no_exec(&self, cfg: ConfigurationNoExec) -> ConfiguredProvidersLabel {
        self.configure_pair(cfg.cfg_pair().dupe())
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
#[derive(
    Clone, Dupe, Debug, Display, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative
)]
#[display("{}{} ({})", target.unconfigured(), name, target.cfg())]
pub struct ConfiguredProvidersLabel {
    target: ConfiguredTargetLabel,
    name: ProvidersName,
}

impl ConfiguredProvidersLabel {
    pub fn new(target: ConfiguredTargetLabel, name: ProvidersName) -> Self {
        Self { target, name }
    }

    pub fn default_for(target: ConfiguredTargetLabel) -> Self {
        Self {
            target,
            name: ProvidersName::Default,
        }
    }

    pub fn target(&self) -> &ConfiguredTargetLabel {
        &self.target
    }

    #[inline]
    pub fn cfg(&self) -> &ConfigurationData {
        self.target.cfg()
    }

    pub fn unconfigured(&self) -> ProvidersLabel {
        ProvidersLabel::new(self.target.unconfigured().dupe(), self.name.dupe())
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
pub trait ProvidersLabelMaybeConfigured: Display + Allocative {}

impl ProvidersLabelMaybeConfigured for ProvidersLabel {}
impl ProvidersLabelMaybeConfigured for ConfiguredProvidersLabel {}

pub mod testing {
    use gazebo::prelude::*;

    use super::*;
    use crate::package::PackageLabel;
    use crate::target::name::TargetNameRef;

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
                    PackageLabel::testing_new(cell, package),
                    TargetNameRef::new(target).unwrap(),
                ),
                match name {
                    Some(n) => ProvidersName::NonDefault(Arc::new(NonDefaultProvidersName::Named(
                        ArcSlice::from_iter(n.map(|s| ProviderName::new((*s).to_owned()).unwrap())),
                    ))),
                    _ => ProvidersName::Default,
                },
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::provider::label::ProviderName;

    #[test]
    fn providers_name_validation() {
        ProviderName::new("foo".to_owned()).unwrap();
        ProviderName::new("foo_-,.=+/1".to_owned()).unwrap();
        assert!(ProviderName::new("foo bar".to_owned()).is_err());
        assert!(ProviderName::new("foo@bar".to_owned()).is_err());
    }
}
