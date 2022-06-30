/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A 'Configuration' is a set of attributes that are attached to each node in
//! the 'static graph' that affects the behaviour of the build. Examples of
//! these attributes are the target platform, and compiler settings.
//!
//! 'Configuration's are propagated from the top level request node to each of
//! the transitive child nodes. During propagation, the configuration may change
//! under a "transition". Multiple distinct configurations may be applied to the
//! transitive graph, effectively duplicating the graph to create two distinct
//! graphs with different build behaviours (split-transitions).
//!

use std::borrow::Borrow;
use std::collections::hash_map::DefaultHasher;
use std::collections::BTreeMap;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;

use gazebo::prelude::*;
use internment_tweaks::Intern;
use internment_tweaks::StaticInterner;
use once_cell::sync::Lazy;
use serde::Serialize;
use serde::Serializer;
use thiserror::Error;

use crate::configuration::constraints::ConstraintKey;
use crate::configuration::constraints::ConstraintValue;

pub mod constraints;
pub mod transition;

#[derive(Debug, Error)]
enum ConfigurationError {
    #[error(
        "Attempted to access the configuration data for the \"unbound\" platform. This platform should only be used when processing configuration rules (`platform()`, `config_setting()`, etc) and these do not support configuration features (like `select()`)."
    )]
    Unbound,
    #[error(
        "Attempted to access the configuration data for the \"unspecified\" platform. This platform is used when the global default platform is unspecified and in that case configuration features (like `select()`) are unsupported."
    )]
    Unspecified,
    #[error("Platform is not bound: {0}")]
    NotBound(String),
    #[error(
        "Attempted to access the configuration data for the \"unspecified_exec\" platform. This platform is used when no execution platform was resolved for a target."
    )]
    UnspecifiedExec,
    #[error("Configuration label is empty")]
    LabelIsEmpty,
    #[error("Configuration label is too long: {0:?}")]
    LabelIsTooLong(String),
}

#[derive(Debug, Error)]
enum ConfigurationLookupError {
    #[error(
        "Got invalid configuration string `{0}`, expected something like `cell//package/path:target-<hash>`"
    )]
    InvalidFormat(String),
    #[error("
    Could not find configuration `{0}` (with hash `{1}`). Configuration lookup by string requires
    that buck has already loaded the configuration through some other mechanism. You can run `buck2 cquery <some_target>`
    with a target that uses the configuration (somewhere in its graph) to make buck aware of the configuration first.
    ")]
    ConfigNotFound(String, String),
}

/// The inner PlatformConfigurationData is interned as the same configuration could be formed through
/// paths (as many transitions are associative).
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Configuration(Intern<HashedPlatformConfigurationData>);

/// Intern doesn't implement Hash.
#[allow(clippy::derive_hash_xor_eq)] // The derived PartialEq (that uses pointer equality) is still correct.
impl Hash for Configuration {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl Dupe for Configuration {}

impl Borrow<str> for HashedPlatformConfigurationData {
    fn borrow(&self) -> &str {
        &self.output_hash
    }
}

static INTERNER: StaticInterner<HashedPlatformConfigurationData> = StaticInterner::new();

impl Configuration {
    /// Produces a "bound" configuration for a platform. The label should be a unique identifier for the data.
    pub fn from_platform(label: String, data: ConfigurationData) -> anyhow::Result<Self> {
        if label.is_empty() {
            return Err(ConfigurationError::LabelIsEmpty.into());
        }
        if label.len() > 1000 {
            // Sanity check.
            return Err(ConfigurationError::LabelIsTooLong(label).into());
        }
        Ok(Self::from_data(HashedPlatformConfigurationData::new(
            PlatformConfigurationData {
                platform: ConfigurationPlatform::Bound(label),
                data,
            },
        )))
    }

    pub fn unspecified() -> Self {
        static CONFIG: Lazy<Configuration> = Lazy::new(|| {
            Configuration::from_data(HashedPlatformConfigurationData::new(
                PlatformConfigurationData {
                    platform: ConfigurationPlatform::Unspecified,
                    data: ConfigurationData::empty(),
                },
            ))
        });
        CONFIG.dupe()
    }

    pub fn unspecified_exec() -> Self {
        static CONFIG: Lazy<Configuration> = Lazy::new(|| {
            Configuration::from_data(HashedPlatformConfigurationData::new(
                PlatformConfigurationData {
                    platform: ConfigurationPlatform::UnspecifiedExec,
                    data: ConfigurationData::empty(),
                },
            ))
        });
        CONFIG.dupe()
    }

    /// Produces the "unbound" configuration. This is used only when performing analysis of platform() targets and
    /// their dependencies (which is done to form the initial "bound" configurations).
    pub fn unbound() -> Self {
        static CONFIG: Lazy<Configuration> = Lazy::new(|| {
            Configuration::from_data(HashedPlatformConfigurationData::new(
                PlatformConfigurationData {
                    platform: ConfigurationPlatform::Unbound,
                    data: ConfigurationData::empty(),
                },
            ))
        });
        CONFIG.dupe()
    }

    /// Produces the "unbound_exec" configuration. This is used only when getting the exec_deps for a configured node
    /// before we've determined the execution configuration for the node.
    pub fn unbound_exec() -> Self {
        static CONFIG: Lazy<Configuration> = Lazy::new(|| {
            Configuration::from_data(HashedPlatformConfigurationData::new(
                PlatformConfigurationData {
                    platform: ConfigurationPlatform::Unbound,
                    data: ConfigurationData::empty(),
                },
            ))
        });
        CONFIG.dupe()
    }

    /// Produces an "invalid" configuration for testing.
    pub fn testing_new() -> Self {
        static CONFIG: Lazy<Configuration> = Lazy::new(|| {
            Configuration::from_data(HashedPlatformConfigurationData::new(
                PlatformConfigurationData {
                    platform: ConfigurationPlatform::Testing,
                    data: ConfigurationData::empty(),
                },
            ))
        });
        CONFIG.dupe()
    }

    fn from_data(data: HashedPlatformConfigurationData) -> Self {
        Self(INTERNER.intern(data))
    }

    /// Iterates over the existing interned configurations. As these configurations
    /// are never evicted, this may return configurations that aren't present in the
    /// actual current state (for example, if you do a build and then delete everything
    /// this will still iterate over previously existing configurations).
    pub fn iter_existing() -> anyhow::Result<impl Iterator<Item = Self>> {
        Ok(INTERNER.iter().map(Self))
    }

    /// Looks up a known configuration from a `Configuration::full_name()` string. Generally
    /// this is a debugging utility that most buck code shouldn't use, it's primarily useful
    /// for resolving configuration strings provided on the command line.
    ///
    /// This can only find configurations that have otherwise already been encountered by
    /// the current daemon process.
    pub fn lookup_from_string(cfg: &str) -> anyhow::Result<Self> {
        match cfg.rsplit_once('-') {
            Some((_, hash)) => match INTERNER.get(hash) {
                Some(cfg) => Ok(Self(cfg)),
                None => Err(ConfigurationLookupError::ConfigNotFound(
                    cfg.to_owned(),
                    hash.to_owned(),
                )
                .into()),
            },
            None => Err(ConfigurationLookupError::InvalidFormat(cfg.to_owned()).into()),
        }
    }

    pub fn get_constraint_value(
        &self,
        key: &ConstraintKey,
    ) -> anyhow::Result<Option<&ConstraintValue>> {
        Ok(self.data()?.constraints.get(key))
    }

    pub fn label(&self) -> anyhow::Result<&str> {
        match &self.0.platform_configuration_data.platform {
            ConfigurationPlatform::Bound(label) => Ok(label),
            _ => Err(ConfigurationError::NotBound(self.to_string()).into()),
        }
    }

    pub fn data(&self) -> anyhow::Result<&ConfigurationData> {
        match &self.0.platform_configuration_data.platform {
            ConfigurationPlatform::Unbound => Err(ConfigurationError::Unbound.into()),
            ConfigurationPlatform::Unspecified => Err(ConfigurationError::Unspecified.into()),
            ConfigurationPlatform::UnspecifiedExec => {
                Err(ConfigurationError::UnspecifiedExec.into())
            }
            _ => Ok(&self.0.platform_configuration_data.data),
        }
    }

    pub fn is_unbound(&self) -> bool {
        match &self.0.platform_configuration_data.platform {
            ConfigurationPlatform::Unbound => true,
            ConfigurationPlatform::UnboundExec => true,
            _ => false,
        }
    }

    pub fn is_bound(&self) -> bool {
        match &self.0.platform_configuration_data.platform {
            ConfigurationPlatform::Bound(..) => true,
            _ => false,
        }
    }

    pub fn output_hash(&self) -> &str {
        &self.0.output_hash
    }

    pub fn full_name(&self) -> &str {
        &self.0.full_name
    }
}

impl Display for Configuration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.full_name())
    }
}

impl Serialize for Configuration {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.collect_str(self)
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum ConfigurationPlatform {
    /// This represents the normal case where a platform has been defined by a `platform()` (or similar) target.
    Bound(String),
    /// The unbound platform is used when we don't yet have a platform bound. This is to support initialization
    /// and is used when analyzing a platform target itself (since we clearly can't have a platform yet bound
    /// at that point).
    /// That leads to a slight oddity that the platform's deps are processed with an empty platform. And so,
    /// the ConfigurationInfo it receives from a constraint that it itself sets, will indicate that the constraint
    /// doesn't match.
    Unbound,

    /// In some cases, a build does not have a default platform. In this case, we will use the "unspecified"
    /// platform. Any attempt to access the ConfigurationData for an "unspecified" platform will fail. This means
    /// that any use of constraints or selects or any other "configuration" feature will fail.
    ///
    /// This is generally only the case for users that haven't yet adopted configurations.
    Unspecified,
    UnspecifiedExec,
    /// The testing platform is just used as a convenience in tests where we don't want or need to construct a
    /// fully specified configuration.
    Testing,

    // Note: If new variants are added somewhere other than the end, it will change the hash of the other variants. This
    // will change the computed buck-out hash path for outputs using that configuration. The `Testing` configuration
    // is widely used in tests.
    // TODO(cjhopman): Change this so that our paths are less sensitive to changes.
    /// The unbound_exec platform is used when we don't yet have an execution platform bound. This is used so that
    /// we can get the exec deps of a "configured" attr, which we need to resolve the execution platform.
    UnboundExec,
}

impl Display for ConfigurationPlatform {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigurationPlatform::Bound(label) => write!(f, "{}", label),
            ConfigurationPlatform::Testing => write!(f, "<testing>"),
            ConfigurationPlatform::Unbound => write!(f, "<unbound>"),
            ConfigurationPlatform::UnboundExec => write!(f, "<unbound_exec>"),
            ConfigurationPlatform::Unspecified => write!(f, "<unspecified>"),
            ConfigurationPlatform::UnspecifiedExec => write!(f, "<unspecified_exec>"),
        }
    }
}

/// A set of values used in configuration-related contexts.
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct ConfigurationData {
    // contains the full specification of the platform configuration
    pub constraints: BTreeMap<ConstraintKey, ConstraintValue>,
    // contains mappings of `section.key` to `value` for buckconfigs
    // TODO(scottcao): Make this into a Vec<ConfigArgumentPair> for more structured data
    // This can't be done right now because ConfigArgumentPair lives in buck2_common
    // and buck2_core cannot depend on buck2_common.
    pub buckconfigs: BTreeMap<String, String>,
}

/// We don't use derive(Hash) here because we build Buck 2 on two different versions of Rustc at
/// the moment, and their hashing disagrees <https://github.com/rust-lang/rust/pull/89443>. In any
/// case, we should control what goes into our hash here.
#[allow(clippy::derive_hash_xor_eq)]
impl Hash for ConfigurationData {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for elt in self.constraints.iter() {
            elt.hash(state);
        }
        for elt in self.buckconfigs.iter() {
            elt.hash(state);
        }
    }
}

impl ConfigurationData {
    pub fn empty() -> Self {
        Self {
            constraints: Default::default(),
            buckconfigs: Default::default(),
        }
    }

    pub fn new(
        constraints: BTreeMap<ConstraintKey, ConstraintValue>,
        buckconfigs: BTreeMap<String, String>,
    ) -> Self {
        Self {
            constraints,
            buckconfigs,
        }
    }

    pub fn get_constraint_value(&self, key: &ConstraintKey) -> Option<&ConstraintValue> {
        self.constraints.get(key)
    }

    /// merges this into other, with values in other taking precedence
    pub fn merge(&self, mut other: ConfigurationData) -> Self {
        for (k, v) in &self.constraints {
            other
                .constraints
                .entry(k.dupe())
                .or_insert_with(|| v.dupe());
        }
        other
    }

    fn is_empty(&self) -> bool {
        self.constraints.is_empty()
    }

    fn is_subset<K: Ord, V: Eq>(a: &BTreeMap<K, V>, b: &BTreeMap<K, V>) -> bool {
        // TODO(nga): this can be done in linear time.
        a.len() <= b.len() && a.iter().all(|(k, v)| b.get(k) == Some(v))
    }

    fn len_sum(&self) -> usize {
        self.constraints.len() + self.buckconfigs.len()
    }

    pub fn refines(&self, that: &ConfigurationData) -> bool {
        self.len_sum() > that.len_sum()
            && Self::is_subset(&that.constraints, &self.constraints)
            && Self::is_subset(&that.buckconfigs, &self.buckconfigs)
    }
}

/// Represents the configuration passed down to a target through its
/// dependent. A target may also be affected by global configuration that isn't captured here.
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct PlatformConfigurationData {
    /// platform is just informative and indicates the initial platform that formed this configuration. It
    /// doesn't fully specify the configuration as transitions could add, change, or remove values.
    platform: ConfigurationPlatform,
    /// data contains the fully specified "configuration"
    data: ConfigurationData,
}

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
pub(crate) struct HashedPlatformConfigurationData {
    platform_configuration_data: PlatformConfigurationData,
    // The remaining fields are computed from `platform_configuration_data`.
    /// The "full name" includes both the platform and a hash of the configuration data.
    full_name: String,
    /// A hash of the configuration data that is used for determining output paths.
    output_hash: String,
}

/// This will hash just the "output_hash" which should uniquely identify this data.
#[allow(clippy::derive_hash_xor_eq)] // The derived PartialEq is still correct.
impl std::hash::Hash for HashedPlatformConfigurationData {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.output_hash.hash(state)
    }
}

impl HashedPlatformConfigurationData {
    fn new(platform_configuration_data: PlatformConfigurationData) -> Self {
        // TODO(cjhopman): Should this be a crypto hasher?
        let mut hasher = DefaultHasher::new();
        platform_configuration_data.hash(&mut hasher);
        let output_hash = hasher.finish();
        let output_hash = format!("{:x}", output_hash);

        let full_name = if platform_configuration_data.data.is_empty() {
            platform_configuration_data.platform.to_string()
        } else {
            format!("{:#}-{}", platform_configuration_data.platform, output_hash)
        };
        Self {
            platform_configuration_data,
            full_name,
            output_hash,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::iter::FromIterator;

    use gazebo::dupe::Dupe;

    use crate::configuration::constraints::ConstraintKey;
    use crate::configuration::constraints::ConstraintValue;
    use crate::configuration::ConfigurationData;
    use crate::target::testing::TargetLabelExt;
    use crate::target::TargetLabel;

    #[test]
    fn is_subset() {
        let m_12_34 = BTreeMap::from_iter([(1, 2), (3, 4)]);
        let m_12_35 = BTreeMap::from_iter([(1, 2), (3, 5)]);
        let m_12 = BTreeMap::from_iter([(1, 2)]);
        let empty = BTreeMap::<u32, u32>::new();

        // A set is a subset of itself
        assert!(ConfigurationData::is_subset(&m_12_34, &m_12_34));
        assert!(ConfigurationData::is_subset(&m_12, &m_12));
        assert!(ConfigurationData::is_subset(&empty, &empty));

        assert!(ConfigurationData::is_subset(&m_12, &m_12_34));
        assert!(!ConfigurationData::is_subset(&m_12_34, &m_12));

        assert!(!ConfigurationData::is_subset(&m_12_34, &m_12_35));
    }

    #[test]
    fn refines() {
        fn constraint_key(t: &str) -> ConstraintKey {
            ConstraintKey(TargetLabel::testing_parse(t))
        }

        fn constraint_value(t: &str) -> ConstraintValue {
            ConstraintValue(TargetLabel::testing_parse(t))
        }

        let os = constraint_key("//:os");
        let linux = constraint_value("//:linux");
        let cpu = constraint_key("//:cpu");
        let arm64 = constraint_value("//:arm64");
        let x86_64 = constraint_value("//:x86_64");

        let c_linux = ConfigurationData {
            constraints: BTreeMap::from_iter([(os.dupe(), linux.dupe())]),
            buckconfigs: BTreeMap::new(),
        };
        let c_arm64 = ConfigurationData {
            constraints: BTreeMap::from_iter([(cpu.dupe(), arm64.dupe())]),
            buckconfigs: BTreeMap::new(),
        };
        let c_linux_arm64 = ConfigurationData {
            constraints: BTreeMap::from_iter([
                (os.dupe(), linux.dupe()),
                (cpu.dupe(), arm64.dupe()),
            ]),
            buckconfigs: BTreeMap::new(),
        };
        let c_linux_x86_64 = ConfigurationData {
            constraints: BTreeMap::from_iter([
                (os.dupe(), linux.dupe()),
                (cpu.dupe(), x86_64.dupe()),
            ]),
            buckconfigs: BTreeMap::new(),
        };

        // Config setting does not refines identical config setting.
        assert!(!c_linux.refines(&c_linux));

        assert!(!c_linux.refines(&c_arm64));
        assert!(!c_arm64.refines(&c_linux));

        assert!(c_linux_arm64.refines(&c_linux));
        assert!(c_linux_arm64.refines(&c_arm64));

        assert!(!c_linux_x86_64.refines(&c_linux_arm64));
    }

    #[test]
    fn buckconfig_refines() {
        let c1 = ConfigurationData {
            constraints: BTreeMap::new(),
            buckconfigs: BTreeMap::from_iter([("foo.bar".to_owned(), "baz".to_owned())]),
        };
        let c11 = ConfigurationData {
            constraints: BTreeMap::new(),
            buckconfigs: BTreeMap::from_iter([
                ("foo.bar".to_owned(), "baz".to_owned()),
                ("foo.qux".to_owned(), "quux".to_owned()),
            ]),
        };

        assert!(c11.refines(&c1));
        assert!(!c11.refines(&c11));
        assert!(!c1.refines(&c1));
        assert!(!c1.refines(&c11));
    }
}
