/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::hash::Hasher;
use std::ptr;
use std::str;

use allocative::Allocative;
use buck2_data::ToProtoMessage;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_util::hash::BuckHasher;
use dupe::Dupe;
use lock_free_hashtable::atomic_value::AtomicValue;
use pagable::Pagable;
use ref_cast::RefCastCustom;
use ref_cast::ref_cast_custom;
use serde::Serialize;
use serde::Serializer;
use strong_hash::StrongHash;
use triomphe::ThinArc;

use crate::cells::CellAliasResolver;
use crate::cells::CellResolver;
use crate::cells::name::CellName;
use crate::cells::paths::CellRelativePath;
use crate::configuration::data::ConfigurationData;
use crate::configuration::pair::Configuration;
use crate::configuration::pair::ConfigurationNoExec;
use crate::package::PackageLabel;
use crate::pattern::pattern::ParsedPattern;
use crate::pattern::pattern::lex_target_pattern;
use crate::pattern::pattern_type::TargetPatternExtra;
use crate::target::configured_target_label::ConfiguredTargetLabel;
use crate::target::label::triomphe_thin_arc_borrow::ThinArcBorrow;
use crate::target::name::TargetNameRef;

#[derive(Debug, Eq, PartialEq, Allocative, Pagable)]
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
#[derive(
    Clone,
    derive_more::Display,
    Eq,
    PartialEq,
    Allocative,
    RefCastCustom,
    Pagable
)]
#[display("{}", self.as_ref())]
#[repr(transparent)]
pub struct TargetLabel(
    ThinArc<
        TargetLabelHeader,
        // `u8` type argument means `ThinArc` stores `[u8]` inline.
        // We store string target name in that `[u8]`.
        u8,
    >,
);

impl StrongHash for TargetLabel {
    fn strong_hash<H: Hasher>(&self, state: &mut H) {
        self.pkg().strong_hash(state);
        self.name().strong_hash(state);
    }
}

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

    #[ref_cast_custom]
    fn ref_cast(arc: &ThinArc<TargetLabelHeader, u8>) -> &Self;

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
    pub fn as_ref(&self) -> TargetLabelRef<'_> {
        TargetLabelRef::new(self.pkg(), self.name())
    }

    pub fn parse(
        label: &str,
        cell_name: CellName,
        cell_resolver: &CellResolver,
        cell_alias_resolver: &CellAliasResolver,
    ) -> buck2_error::Result<TargetLabel> {
        let (target_label, TargetPatternExtra) =
            ParsedPattern::<TargetPatternExtra>::parse_precise(
                label,
                cell_name,
                cell_resolver,
                cell_alias_resolver,
            )?
            .as_literal(label)?;
        Ok(target_label)
    }

    fn into_raw(self) -> *const () {
        ThinArc::into_raw(self.0) as *const ()
    }

    #[cfg(test)]
    pub(crate) fn as_raw(&self) -> *const () {
        ThinArc::as_ptr(&self.0) as *const ()
    }

    unsafe fn from_raw(raw: *const ()) -> Self {
        TargetLabel(unsafe { ThinArc::from_raw(raw as *const _) })
    }

    pub(crate) fn arc_borrow(&self) -> TargetLabelBorrow<'_> {
        TargetLabelBorrow {
            borrow: ThinArcBorrow::borrow(&self.0),
        }
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
                CellRelativePath::new(
                    <&ForwardRelativePath>::try_from(pattern_data.package_path())
                        .expect("must be valid path"),
                ),
            )
            .unwrap(),
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
#[display("{}:{}", pkg, name)]
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

/// `TargetLabel` but without refcounter increment.
#[derive(Copy, Clone, Dupe)]
#[doc(hidden)] // `impl AtomicValue` is wants this to be public.
pub struct TargetLabelBorrow<'a> {
    borrow: ThinArcBorrow<'a, TargetLabelHeader, u8>,
}

impl TargetLabelBorrow<'_> {
    /// Obtain a temporary reference to the `TargetLabel`.
    fn with_target_label<R>(self, mut f: impl FnMut(&TargetLabel) -> R) -> R {
        self.borrow.with_arc(|arc| f(TargetLabel::ref_cast(arc)))
    }

    /// Upgrade to `TargetLabel`.
    pub(crate) fn to_owned(self) -> TargetLabel {
        TargetLabel(self.borrow.to_owned())
    }

    pub(crate) unsafe fn from_raw(raw: *const ()) -> Self {
        TargetLabelBorrow {
            borrow: unsafe { ThinArcBorrow::from_raw(raw) },
        }
    }
}

impl PartialEq for TargetLabelBorrow<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.with_target_label(|a| other.with_target_label(|b| a == b))
    }
}

impl Hash for TargetLabelBorrow<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.with_target_label(|a| a.hash(state))
    }
}

impl AtomicValue for TargetLabel {
    type Raw = *const ();
    type Ref<'a>
        = TargetLabelBorrow<'a>
    where
        Self: 'a;

    fn null() -> Self::Raw {
        ptr::null()
    }

    fn is_null(this: Self::Raw) -> bool {
        this.is_null()
    }

    fn into_raw(this: Self) -> Self::Raw {
        TargetLabel::into_raw(this)
    }

    unsafe fn from_raw(raw: Self::Raw) -> Self {
        unsafe { TargetLabel::from_raw(raw) }
    }

    unsafe fn deref<'a>(raw: Self::Raw) -> Self::Ref<'a> {
        unsafe { TargetLabelBorrow::from_raw(raw) }
    }
}
