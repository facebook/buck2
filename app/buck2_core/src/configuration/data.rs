/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;
use std::hash::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;

use allocative::Allocative;
use buck2_data::ToProtoMessage;
use buck2_util::hash::BuckHasher;
use buck2_util::strong_hasher::Blake3StrongHasher;
use dupe::Dupe;
use equivalent::Equivalent;
use once_cell::sync::Lazy;
use once_cell::sync::OnceCell;
use serde::Serialize;
use serde::Serializer;
use static_interner::Intern;
use static_interner::InternDisposition;
use static_interner::Interner;
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
    #[error("Internal error: NEW_PLATFORM_HASH_ROLLOUT_THRESHOLD is already initialized")]
    NewPlatformHashRolloutThresholdAlreadyInitialized,
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

pub static NEW_PLATFORM_HASH_ROLLOUT_THRESHOLD: OnceCell<u8> = OnceCell::new();

pub fn init_new_platform_hash_rollout_threshold(rollout: Option<f64>) -> buck2_error::Result<()> {
    let rollout_threshold = if let Some(rollout) = rollout {
        (rollout * (u64::MAX as f64)) as u8
    } else {
        // disabled by default
        0u8
    };
    NEW_PLATFORM_HASH_ROLLOUT_THRESHOLD
        .set(rollout_threshold)
        .map_err(|_| ConfigurationError::NewPlatformHashRolloutThresholdAlreadyInitialized)?;
    Ok(())
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
    derive_more::Display
)]
pub struct ConfigurationData(Intern<HashedConfigurationPlatform>);

#[derive(Hash)]
struct ConfigurationHashRef<'a>(&'a str);

impl<'a> Equivalent<HashedConfigurationPlatform> for ConfigurationHashRef<'a> {
    fn equivalent(&self, key: &HashedConfigurationPlatform) -> bool {
        self.0 == key.output_hash.as_str()
    }
}

static INTERNER: Interner<HashedConfigurationPlatform, BuckHasher> = Interner::new();

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

#[derive(Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative, StrongHash)]
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
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Allocative, StrongHash)]
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
    derive_more::Display
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

impl HashedConfigurationPlatform {
    fn new(configuration_platform: ConfigurationPlatform) -> Self {
        let mut hasher = Blake3StrongHasher::new();
        configuration_platform.strong_hash(&mut hasher);
        let output_hash = hasher.digest();
        let output_hash = u64::from_be_bytes(
            output_hash.as_bytes()[0..8]
                .try_into()
                .expect("Internal error from converting a slice of 8 to an array of 8"),
        );

        let rollout_threshold = match NEW_PLATFORM_HASH_ROLLOUT_THRESHOLD.get() {
            Some(v) => *v,
            // We only hit the uninitialized case in unit tests. In this case, we don't want to throw an
            // error because it would fail a bunch of unit tests, and we don't want to
            // unit tests explicitly initialize this value because this is a temporary migration value
            // that we will get rid of later. We have e2e tests checking that this does not enable
            // new hashing in production.
            None => u8::MAX,
        };

        let output_hash = if output_hash as u8 <= rollout_threshold {
            output_hash
        } else {
            let mut hasher = DefaultHasher::new();
            configuration_platform.hash(&mut hasher);
            hasher.finish()
        };
        let output_hash = ConfigurationHash::new(output_hash);

        let full_name = match &configuration_platform {
            ConfigurationPlatform::Bound(label, _cfg) => {
                format!("{:#}#{}", label, output_hash)
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
    use crate::target::label::label::TargetLabel;

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
                        ConstraintKey(TargetLabel::testing_parse("foo//bar:c")),
                        ConstraintValue(TargetLabel::testing_parse("foo//bar:v")),
                    ),
                    (
                        ConstraintKey(TargetLabel::testing_parse("foo//qux:c")),
                        ConstraintValue(TargetLabel::testing_parse("foo//qux:vx")),
                    ),
                ]),
            },
        )
        .unwrap();

        assert_eq!(configuration.output_hash().as_str(), "aa02f1990fb35119");
        assert_eq!(
            configuration.to_string(),
            "cfg_for//:testing_exec#aa02f1990fb35119"
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
                        ConstraintKey(TargetLabel::testing_parse("foo//bar:c")),
                        ConstraintValue(TargetLabel::testing_parse("foo//bar:v")),
                    ),
                    (
                        ConstraintKey(TargetLabel::testing_parse("foo//qux:c")),
                        ConstraintValue(TargetLabel::testing_parse("foo//qux:vx")),
                    ),
                ]),
            },
        )
        .unwrap();

        let expected_cfg_str = "cfg_for//:testing_exec#aa02f1990fb35119";
        assert_eq!(expected_cfg_str, configuration.to_string());

        let looked_up =
            ConfigurationData::lookup_bound(BoundConfigurationId::parse(expected_cfg_str).unwrap())
                .unwrap();
        assert_eq!(configuration, looked_up);
    }
}
