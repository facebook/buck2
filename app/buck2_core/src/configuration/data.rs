/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::collections::BTreeMap;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use buck2_data::ToProtoMessage;
use buck2_util::hash::BuckHasher;
use buck2_util::strong_hasher::Blake3StrongHasher;
use dupe::Dupe;
use equivalent::Equivalent;
use once_cell::sync::Lazy;
use pagable::Pagable;
use serde::Serialize;
use serde::Serializer;
use static_interner::Intern;
use static_interner::InternDisposition;
use static_interner::interner;
use strong_hash::StrongHash;

use crate::configuration::bound_id::BoundConfigurationId;
use crate::configuration::bound_label::BoundConfigurationLabel;
use crate::configuration::builtin::BuiltinPlatform;
use crate::configuration::constraints::ConstraintKey;
use crate::configuration::constraints::ConstraintValue;
use crate::configuration::hash::ConfigurationHash;
use crate::event::EVENT_DISPATCH;

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum ConfigurationError {
    #[error(
        "Attempted to access the configuration data for the {0} platform. \
        This platform is used when the global default platform is unspecified \
        and in that case configuration features (like `select()`) are unsupported."
    )]
    Builtin(BuiltinPlatform),
    #[error("Platform is not bound: {0}")]
    NotBound(String),
    #[error(
        "Attempted to access the configuration data for the \"unspecified_exec\" platform. This platform is used when no execution platform was resolved for a target."
    )]
    UnspecifiedExec,
}

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum ConfigurationLookupError {
    #[error("
    Could not find configuration `{0}`. Configuration lookup by string requires
    that buck has already loaded the configuration through some other mechanism. You can run `buck2 cquery <some_target>`
    with a target that uses the configuration (somewhere in its graph) to make buck aware of the configuration first.
    ")]
    ConfigNotFound(BoundConfigurationId),
    #[error(
        "Found configuration `{0}` by hash, but label mismatched from what is requested: `{1}`"
    )]
    ConfigFoundByHashLabelMismatch(ConfigurationData, BoundConfigurationId),
}

fn emit_configuration_instant_event(cfg: &ConfigurationData) -> buck2_error::Result<()> {
    let constraints: Vec<buck2_data::Constraint> = cfg
        .data()?
        .constraints
        .iter()
        .map(|(k, v)| buck2_data::Constraint {
            setting: k.to_string(),
            value: v.to_string(),
        })
        .collect();

    // Sometimes this isn't going to be init'd in tests (oss or buck2), let's
    // ignore that and rely on e2e test to assert we're still logging data from
    // production code paths.
    if let Ok(event_dispatch) = EVENT_DISPATCH.get() {
        event_dispatch.emit_instant_event_for_data(
            buck2_data::ConfigurationCreated {
                cfg: Some(buck2_data::ConfigurationWithConstraints {
                    full_name: cfg.full_name().to_owned(),
                    constraint: constraints,
                }),
            }
            .into(),
        );
    }

    Ok(())
}

/// The inner PlatformConfigurationData is interned as the same configuration could be formed through
/// paths (as many transitions are associative).
#[derive(
    Clone,
    Debug,
    Eq,
    PartialEq,
    Hash,
    Dupe,
    Ord,
    PartialOrd,
    Allocative,
    derive_more::Display,
    StrongHash,
    Pagable
)]
pub struct ConfigurationData(Intern<HashedConfigurationPlatform>);

#[derive(Hash)]
struct ConfigurationHashRef<'a>(&'a str);

impl Equivalent<HashedConfigurationPlatform> for ConfigurationHashRef<'_> {
    fn equivalent(&self, key: &HashedConfigurationPlatform) -> bool {
        self.0 == key.output_hash.as_str()
    }
}

interner!(INTERNER, BuckHasher, HashedConfigurationPlatform);

impl ConfigurationData {
    /// Produces a "bound" configuration for a platform. The label should be a unique identifier for the data.
    pub fn from_platform(label: String, data: ConfigurationDataData) -> buck2_error::Result<Self> {
        let label = BoundConfigurationLabel::new(label)?;
        let (cfg, disposition) = Self::from_data(HashedConfigurationPlatform::new(
            ConfigurationPlatform::Bound(label, data),
        ));
        if let InternDisposition::Computed = disposition {
            emit_configuration_instant_event(&cfg)?;
        }

        Ok(cfg)
    }

    pub fn unspecified() -> Self {
        static CONFIG: Lazy<ConfigurationData> = Lazy::new(|| {
            ConfigurationData::from_data(HashedConfigurationPlatform::new(
                ConfigurationPlatform::Builtin(BuiltinPlatform::Unspecified),
            ))
            .0
        });
        CONFIG.dupe()
    }

    pub fn unspecified_exec() -> Self {
        static CONFIG: Lazy<ConfigurationData> = Lazy::new(|| {
            ConfigurationData::from_data(HashedConfigurationPlatform::new(
                ConfigurationPlatform::Builtin(BuiltinPlatform::UnspecifiedExec),
            ))
            .0
        });
        CONFIG.dupe()
    }

    /// Produces the "unbound" configuration. This is used only when performing analysis of platform() targets and
    /// their dependencies (which is done to form the initial "bound" configurations).
    pub fn unbound() -> Self {
        static CONFIG: Lazy<ConfigurationData> = Lazy::new(|| {
            ConfigurationData::from_data(HashedConfigurationPlatform::new(
                ConfigurationPlatform::Builtin(BuiltinPlatform::Unbound),
            ))
            .0
        });
        CONFIG.dupe()
    }

    /// Produces the "unbound_exec" configuration. This is used only when getting the exec_deps for a configured node
    /// before we've determined the execution configuration for the node.
    pub fn unbound_exec() -> Self {
        static CONFIG: Lazy<ConfigurationData> = Lazy::new(|| {
            ConfigurationData::from_data(HashedConfigurationPlatform::new(
                ConfigurationPlatform::Builtin(BuiltinPlatform::UnboundExec),
            ))
            .0
        });
        CONFIG.dupe()
    }

    pub fn builtin(builtin: BuiltinPlatform) -> Self {
        match builtin {
            BuiltinPlatform::Unspecified => Self::unspecified(),
            BuiltinPlatform::UnspecifiedExec => Self::unspecified_exec(),
            BuiltinPlatform::Unbound => Self::unbound(),
            BuiltinPlatform::UnboundExec => Self::unbound_exec(),
        }
    }

    /// Produces an "invalid" configuration for testing.
    pub fn testing_new() -> Self {
        Self::from_data(HashedConfigurationPlatform::new(
            ConfigurationPlatform::Bound(
                BoundConfigurationLabel::new("<testing>".to_owned()).unwrap(),
                ConfigurationDataData {
                    constraints: BTreeMap::new(),
                },
            ),
        ))
        .0
    }

    fn from_data(data: HashedConfigurationPlatform) -> (Self, InternDisposition) {
        let (val, disposition) = INTERNER.observed_intern(data);
        (Self(val), disposition)
    }

    /// Iterates over the existing interned configurations. As these configurations
    /// are never evicted, this may return configurations that aren't present in the
    /// actual current state (for example, if you do a build and then delete everything
    /// this will still iterate over previously existing configurations).
    pub fn iter_existing() -> impl Iterator<Item = Self> {
        INTERNER.iter().map(Self)
    }

    /// Looks up a known configuration from a `Configuration::full_name()` string. Generally
    /// this is a debugging utility that most buck code shouldn't use, it's primarily useful
    /// for resolving configuration strings provided on the command line.
    ///
    /// This can only find configurations that have otherwise already been encountered by
    /// the current daemon process.
    pub fn lookup_bound(cfg: BoundConfigurationId) -> buck2_error::Result<Self> {
        match INTERNER.get(ConfigurationHashRef(cfg.hash.as_str())) {
            Some(found_cfg) => {
                let found_cfg = ConfigurationData(found_cfg);
                if found_cfg.bound_id().as_ref() != Some(&cfg) {
                    Err(
                        ConfigurationLookupError::ConfigFoundByHashLabelMismatch(found_cfg, cfg)
                            .into(),
                    )
                } else {
                    Ok(found_cfg)
                }
            }
            None => Err(ConfigurationLookupError::ConfigNotFound(cfg).into()),
        }
    }

    pub fn get_constraint_value(
        &self,
        key: &ConstraintKey,
    ) -> buck2_error::Result<Option<&ConstraintValue>> {
        Ok(self.data()?.constraints.get(key))
    }

    pub fn label(&self) -> buck2_error::Result<&str> {
        match &self.0.configuration_platform {
            ConfigurationPlatform::Bound(label, _) => Ok(label.as_str()),
            _ => Err(ConfigurationError::NotBound(self.to_string()).into()),
        }
    }

    pub fn data(&self) -> buck2_error::Result<&ConfigurationDataData> {
        match &self.0.configuration_platform {
            ConfigurationPlatform::Builtin(BuiltinPlatform::UnspecifiedExec) => {
                Err(ConfigurationError::UnspecifiedExec.into())
            }
            ConfigurationPlatform::Builtin(builtin) => {
                Err(ConfigurationError::Builtin(*builtin).into())
            }
            ConfigurationPlatform::Bound(_, data) => Ok(data),
        }
    }

    pub fn is_unbound(&self) -> bool {
        match &self.0.configuration_platform {
            ConfigurationPlatform::Builtin(BuiltinPlatform::Unbound) => true,
            _ => false,
        }
    }

    pub fn bound(&self) -> Option<&BoundConfigurationLabel> {
        match &self.0.configuration_platform {
            ConfigurationPlatform::Bound(label, _) => Some(label),
            _ => None,
        }
    }

    pub fn bound_id(&self) -> Option<BoundConfigurationId> {
        Some(BoundConfigurationId {
            label: self.bound()?.clone(),
            hash: self.output_hash().clone(),
        })
    }

    pub fn is_bound(&self) -> bool {
        match &self.0.configuration_platform {
            ConfigurationPlatform::Bound(..) => true,
            _ => false,
        }
    }

    pub fn output_hash(&self) -> &ConfigurationHash {
        &self.0.output_hash
    }

    /// Name without hash.
    pub fn short_name(&self) -> &str {
        self.0.configuration_platform.label()
    }

    pub fn full_name(&self) -> &str {
        &self.0.full_name
    }
}

impl Serialize for ConfigurationData {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        s.collect_str(self)
    }
}

impl ToProtoMessage for ConfigurationData {
    type Message = buck2_data::Configuration;

    fn as_proto(&self) -> Self::Message {
        buck2_data::Configuration {
            full_name: self.full_name().to_owned(),
        }
    }
}

#[derive(
    Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative, StrongHash, Pagable
)]
enum ConfigurationPlatform {
    /// This represents the normal case where a platform has been defined by a `platform()` (or similar) target.
    Bound(BoundConfigurationLabel, ConfigurationDataData),
    Builtin(BuiltinPlatform),
}

impl ConfigurationPlatform {
    fn label(&self) -> &str {
        match self {
            ConfigurationPlatform::Bound(label, _) => label.as_str(),
            ConfigurationPlatform::Builtin(builtin) => builtin.label(),
        }
    }
}

/// A set of values used in configuration-related contexts.
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Allocative, StrongHash, Pagable)]
pub struct ConfigurationDataData {
    // contains the full specification of the platform configuration
    pub constraints: BTreeMap<ConstraintKey, ConstraintValue>,
}

/// We don't use derive(Hash) here because we build Buck 2 on two different versions of Rustc at
/// the moment, and their hashing disagrees <https://github.com/rust-lang/rust/pull/89443>. In any
/// case, we should control what goes into our hash here.
#[allow(clippy::derived_hash_with_manual_eq)]
impl Hash for ConfigurationDataData {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for elt in self.constraints.iter() {
            elt.hash(state);
        }
    }
}

impl ConfigurationDataData {
    pub fn empty() -> Self {
        Self {
            constraints: Default::default(),
        }
    }

    pub fn new(constraints: BTreeMap<ConstraintKey, ConstraintValue>) -> Self {
        Self { constraints }
    }

    pub fn get_constraint_value(&self, key: &ConstraintKey) -> Option<&ConstraintValue> {
        self.constraints.get(key)
    }

    /// merges this into other, with values in other taking precedence
    pub fn merge(&self, mut other: ConfigurationDataData) -> Self {
        for (k, v) in &self.constraints {
            other
                .constraints
                .entry(k.dupe())
                .or_insert_with(|| v.dupe());
        }
        other
    }
}

#[derive(
    Debug,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Allocative,
    derive_more::Display,
    Pagable
)]
#[display("{}", full_name)]
pub(crate) struct HashedConfigurationPlatform {
    configuration_platform: ConfigurationPlatform,
    // The remaining fields are computed from `platform_configuration_data`.
    /// The "full name" includes both the platform and a hash of the configuration data.
    full_name: String,
    /// A hash of the configuration data that is used for determining output paths.
    output_hash: ConfigurationHash,
}

/// This will hash just the "output_hash" which should uniquely identify this data.
#[allow(clippy::derived_hash_with_manual_eq)] // The derived PartialEq is still correct.
impl std::hash::Hash for HashedConfigurationPlatform {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.output_hash.hash(state)
    }
}

impl StrongHash for HashedConfigurationPlatform {
    fn strong_hash<H: Hasher>(&self, state: &mut H) {
        // This is already a strong hash (computed a few lines below).
        self.output_hash.hash(state)
    }
}

impl HashedConfigurationPlatform {
    fn new(configuration_platform: ConfigurationPlatform) -> Self {
        let mut hasher = Blake3StrongHasher::new();
        configuration_platform.strong_hash(&mut hasher);
        let output_hash = hasher.finish();
        let output_hash = ConfigurationHash::new(output_hash);

        let full_name = match &configuration_platform {
            ConfigurationPlatform::Bound(label, _cfg) => {
                format!("{label:#}#{output_hash}")
            }
            ConfigurationPlatform::Builtin(builtin) => builtin.label().to_owned(),
        };
        Self {
            configuration_platform,
            full_name,
            output_hash,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::configuration::bound_id::BoundConfigurationId;
    use crate::configuration::constraints::ConstraintKey;
    use crate::configuration::constraints::ConstraintValue;
    use crate::configuration::data::ConfigurationData;
    use crate::configuration::data::ConfigurationDataData;

    /// We don't want the output hash to change by accident. This test is here to assert that it
    /// doesn't. If we have a legit reason to update the config hash, we can update the hash here,
    /// but this will ensure we a) know and b) don't do it by accident.
    #[test]
    fn test_stable_output_hash() -> buck2_error::Result<()> {
        let configuration = ConfigurationData::from_platform(
            "cfg_for//:testing_exec".to_owned(),
            ConfigurationDataData {
                constraints: BTreeMap::from_iter([
                    (
                        ConstraintKey::testing_new("foo//bar:c"),
                        ConstraintValue::testing_new("foo//bar:v", None),
                    ),
                    (
                        ConstraintKey::testing_new("foo//qux:c"),
                        ConstraintValue::testing_new("foo//qux:vx", None),
                    ),
                ]),
            },
        )
        .unwrap();

        assert_eq!(configuration.output_hash().as_str(), "6770d7f2ebfc0845");
        assert_eq!(
            configuration.to_string(),
            "cfg_for//:testing_exec#6770d7f2ebfc0845"
        );

        Ok(())
    }

    #[test]
    fn test_lookup_from_string() {
        let configuration = ConfigurationData::from_platform(
            "cfg_for//:testing_exec".to_owned(),
            ConfigurationDataData {
                constraints: BTreeMap::from_iter([
                    (
                        ConstraintKey::testing_new("foo//bar:c"),
                        ConstraintValue::testing_new("foo//bar:v", None),
                    ),
                    (
                        ConstraintKey::testing_new("foo//qux:c"),
                        ConstraintValue::testing_new("foo//qux:vx", None),
                    ),
                ]),
            },
        )
        .unwrap();

        let expected_cfg_str = "cfg_for//:testing_exec#6770d7f2ebfc0845";
        assert_eq!(expected_cfg_str, configuration.to_string());

        let looked_up =
            ConfigurationData::lookup_bound(BoundConfigurationId::parse(expected_cfg_str).unwrap())
                .unwrap();
        assert_eq!(configuration, looked_up);
    }
}
