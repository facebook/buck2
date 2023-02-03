/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::str;

use allocative::Allocative;
use dupe::Dupe;
use serde::Serialize;
use serde::Serializer;
use starlark_map::StarlarkHashValue;
use triomphe::ThinArc;

use crate::configuration::pair::ConfigurationPair;
use crate::configuration::pair::ConfigurationPairNoExec;
use crate::configuration::Configuration;
use crate::package::PackageLabel;
use crate::target::name::TargetNameRef;

#[derive(Eq, PartialEq, Allocative)]
struct TargetLabelHeader {
    /// Hash of target label (not package, not name).
    /// Place hash first to make equality check faster.
    hash: StarlarkHashValue,
    pkg: PackageLabel,
    // TODO(nga): this struct has 4 bytes of padding.
}

/// 'TargetLabel' that uniquely maps to a 'target'
/// It contains a 'Package' which is the 'Package' defined by the build fine
/// that contains this 'target', and a 'name' which is a 'TargetName'
/// representing the target name given to the particular target.
#[derive(Clone, derive_more::Display, Eq, PartialEq, Allocative)]
#[display(fmt = "{}:{}", "self.pkg()", "self.name()")]
pub struct TargetLabel(
    ThinArc<
        TargetLabelHeader,
        // `u8` type argument means `ThinArc` stores `[u8]` inline.
        // We store string target name in that `[u8]`.
        u8,
    >,
);

impl Debug for TargetLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("TargetLabel")
            .field("pkg", &self.pkg())
            .field("name", &self.name())
            .finish()
    }
}

impl Dupe for TargetLabel {}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for TargetLabel {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.header.header.hash.hash(state);
    }
}

impl Ord for TargetLabel {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        (self.pkg(), self.name()).cmp(&(other.pkg(), other.name()))
    }
}

impl PartialOrd for TargetLabel {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl TargetLabel {
    pub fn new(pkg: PackageLabel, name: &TargetNameRef) -> Self {
        // TODO(nga): unnecessary to take `TargetName` by value.
        let hash = StarlarkHashValue::new(&(pkg.dupe(), &name));
        TargetLabel(ThinArc::from_header_and_slice(
            TargetLabelHeader { hash, pkg },
            name.as_str().as_bytes(),
        ))
    }

    #[inline]
    pub fn pkg(&self) -> PackageLabel {
        self.0.header.header.pkg.dupe()
    }

    #[inline]
    pub fn name(&self) -> &TargetNameRef {
        let name = unsafe { str::from_utf8_unchecked(&self.0.slice) };
        TargetNameRef::unchecked_new(name)
    }

    /// Creates a 'ConfiguredTargetLabel' from ['Self'] based on the provided
    /// configuration.
    #[inline]
    pub fn configure(&self, cfg: Configuration) -> ConfiguredTargetLabel {
        self.configure_pair(ConfigurationPair::new(cfg, None))
    }

    /// Like `configure`, but forces the execution configuration too.
    #[inline]
    pub fn configure_with_exec(
        &self,
        cfg: Configuration,
        exec_cfg: Configuration,
    ) -> ConfiguredTargetLabel {
        self.configure_pair(ConfigurationPair::new(cfg, Some(exec_cfg)))
    }

    #[inline]
    pub fn configure_pair(&self, cfg_pair: ConfigurationPair) -> ConfiguredTargetLabel {
        ConfiguredTargetLabel {
            target: self.dupe(),
            cfg_pair,
        }
    }

    #[inline]
    pub fn configure_pair_no_exec(&self, cfg: ConfigurationPairNoExec) -> ConfiguredTargetLabel {
        self.configure_pair(cfg.cfg_pair().dupe())
    }
}

impl Serialize for TargetLabel {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

/// 'ConfiguredTargetLabel' are 'TargetLabel's with an 'Configuration' attached.
/// These uniquely map to nodes of the build graph with 'Configuration's
/// applied.
#[derive(Clone, Dupe, Debug, Hash, Eq, PartialEq, Ord, PartialOrd, Allocative)]
pub struct ConfiguredTargetLabel {
    target: TargetLabel,
    cfg_pair: ConfigurationPair,
}

impl Display for ConfiguredTargetLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.target, self.cfg())?;
        if let Some(exec_cfg) = self.exec_cfg() {
            write!(f, " ({})", exec_cfg)?;
        }
        Ok(())
    }
}

impl ConfiguredTargetLabel {
    #[inline]
    pub fn pkg(&self) -> PackageLabel {
        self.target.pkg()
    }

    #[inline]
    pub fn name(&self) -> &TargetNameRef {
        self.target.name()
    }

    #[inline]
    pub fn unconfigured(&self) -> &TargetLabel {
        &self.target
    }

    #[inline]
    pub fn cfg_pair(&self) -> &ConfigurationPair {
        &self.cfg_pair
    }

    #[inline]
    pub fn cfg(&self) -> &Configuration {
        self.cfg_pair.cfg()
    }

    #[inline]
    pub fn exec_cfg(&self) -> Option<&Configuration> {
        self.cfg_pair.exec_cfg()
    }
}

impl Serialize for ConfiguredTargetLabel {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(self)
    }
}

/// [`TargetLabel`] or [`ConfiguredTargetLabel`]
pub trait TargetLabelMaybeConfigured: Display {}

impl TargetLabelMaybeConfigured for TargetLabel {}
impl TargetLabelMaybeConfigured for ConfiguredTargetLabel {}

pub mod testing {
    use crate::configuration::pair::ConfigurationPair;
    use crate::configuration::Configuration;
    use crate::package::testing::PackageExt;
    use crate::package::PackageLabel;
    use crate::target::label::ConfiguredTargetLabel;
    use crate::target::label::TargetLabel;
    use crate::target::name::TargetName;
    use crate::target::name::TargetNameRef;

    pub trait ConfiguredTargetLabelExt {
        /// creates 'ConfiguredTargetLabel'
        fn testing_new(
            pkg: PackageLabel,
            label: TargetName,
            cfg: Configuration,
        ) -> ConfiguredTargetLabel {
            ConfiguredTargetLabel {
                target: TargetLabel::new(pkg, label.as_ref()),
                cfg_pair: ConfigurationPair::new(cfg, None),
            }
        }
    }
    impl ConfiguredTargetLabelExt for ConfiguredTargetLabel {}

    pub trait TargetLabelExt {
        /// Simple and incorrect target label parser which can be used in tests.
        fn testing_parse(target_label: &str) -> TargetLabel {
            let (cell, cell_rel) = target_label.split_once("//").expect("no //");
            let (path, name) = cell_rel.split_once(':').expect("no :");
            let pkg = PackageLabel::testing_new(cell, path);
            let name = TargetNameRef::new(name).unwrap();
            TargetLabel::new(pkg, name)
        }
    }
    impl TargetLabelExt for TargetLabel {}
}
