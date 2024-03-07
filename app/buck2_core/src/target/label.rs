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
use std::hash::Hash;
use std::hash::Hasher;
use std::str;

use allocative::Allocative;
use buck2_data::ToProtoMessage;
use buck2_util::hash::BuckHasher;
use dupe::Dupe;
use serde::Serialize;
use serde::Serializer;
use triomphe::ThinArc;

use crate::cells::name::CellName;
use crate::cells::paths::CellRelativePath;
use crate::cells::CellAliasResolver;
use crate::cells::CellResolver;
use crate::configuration::data::ConfigurationData;
use crate::configuration::pair::Configuration;
use crate::configuration::pair::ConfigurationNoExec;
use crate::package::PackageLabel;
use crate::pattern::lex_target_pattern;
use crate::pattern::pattern_type::TargetPatternExtra;
use crate::pattern::ParsedPattern;
use crate::target::configured_target_label::ConfiguredTargetLabel;
use crate::target::name::TargetNameRef;

#[derive(Eq, PartialEq, Allocative)]
struct TargetLabelHeader {
    /// Hash of target label (not package, not name).
    /// Place hash first to make equality check faster.
    hash: u32,
    pkg: PackageLabel,
    // TODO(nga): this struct has 4 bytes of padding.
}

/// 'TargetLabel' that uniquely maps to a 'target'
/// It contains a 'Package' which is the 'Package' defined by the build fine
/// that contains this 'target', and a 'name' which is a 'TargetName'
/// representing the target name given to the particular target.
#[derive(Clone, derive_more::Display, Eq, PartialEq, Allocative)]
#[display(fmt = "{}", "self.as_ref()")]
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

#[allow(clippy::derived_hash_with_manual_eq)]
impl Hash for TargetLabel {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.header.header.hash.hash(state);
    }
}

impl Ord for TargetLabel {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_ref().cmp(&other.as_ref())
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

        // Hash should be stable because it is used to generate the configuration hash.
        let key = &(pkg.dupe(), &name);
        let mut hasher = BuckHasher::default();
        key.hash(&mut hasher);
        let hash = hasher.finish() as u32;

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
    pub fn configure(&self, cfg: ConfigurationData) -> ConfiguredTargetLabel {
        self.configure_pair(Configuration::new(cfg, None))
    }

    /// Like `configure`, but forces the execution configuration too.
    #[inline]
    pub fn configure_with_exec(
        &self,
        cfg: ConfigurationData,
        exec_cfg: ConfigurationData,
    ) -> ConfiguredTargetLabel {
        self.configure_pair(Configuration::new(cfg, Some(exec_cfg)))
    }

    #[inline]
    pub fn configure_pair(&self, cfg_pair: Configuration) -> ConfiguredTargetLabel {
        ConfiguredTargetLabel {
            target: self.dupe(),
            cfg_pair,
        }
    }

    #[inline]
    pub fn configure_pair_no_exec(&self, cfg: ConfigurationNoExec) -> ConfiguredTargetLabel {
        self.configure_pair(cfg.cfg_pair().dupe())
    }

    #[inline]
    pub fn as_ref(&self) -> TargetLabelRef {
        TargetLabelRef::new(self.pkg(), self.name())
    }

    pub fn parse(
        label: &str,
        cell_name: CellName,
        cell_resolver: &CellResolver,
        cell_alias_resolver: &CellAliasResolver,
    ) -> anyhow::Result<TargetLabel> {
        let (pkg, name, TargetPatternExtra) = ParsedPattern::<TargetPatternExtra>::parse_precise(
            label,
            cell_name,
            cell_resolver,
            cell_alias_resolver,
        )?
        .as_literal(label)?;
        Ok(TargetLabel::new(pkg, name.as_ref()))
    }

    /// Simple and incorrect target label parser which can be used in tests.
    pub fn testing_parse(target_label: &str) -> TargetLabel {
        let parts = lex_target_pattern(target_label, false).expect("failed to parse");
        let cell_name = CellName::testing_new(parts.cell_alias.expect("must have cell name"));

        let pattern_data = parts
            .pattern
            .reject_ambiguity()
            .expect("target label must be unambiguous");
        let (target_name, TargetPatternExtra) =
            pattern_data.target().expect("target label must be precise");

        TargetLabel::new(
            PackageLabel::new(
                cell_name,
                CellRelativePath::new(pattern_data.package_path()),
            ),
            target_name,
        )
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

impl ToProtoMessage for TargetLabel {
    type Message = buck2_data::TargetLabel;

    fn as_proto(&self) -> Self::Message {
        buck2_data::TargetLabel {
            package: self.pkg().to_string(),
            name: self.name().to_string(),
        }
    }
}

#[derive(
    Clone,
    Dupe,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Debug,
    derive_more::Display
)]
#[display(fmt = "{}:{}", pkg, name)]
pub struct TargetLabelRef<'a> {
    pkg: PackageLabel,
    name: &'a TargetNameRef,
}

impl<'a> TargetLabelRef<'a> {
    #[inline]
    pub fn new(pkg: PackageLabel, name: &'a TargetNameRef) -> TargetLabelRef<'a> {
        TargetLabelRef { pkg, name }
    }
}
