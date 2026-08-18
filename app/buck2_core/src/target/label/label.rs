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
use allocative::Visitor;
use buck2_data::ToProtoMessage;
use buck2_fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_hash::BuckHasher;
use dupe::Dupe;
use lock_free_hashtable::atomic_value::AtomicValue;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use pagable::arc_erase::ArcErase;
use pagable::arc_erase::ArcEraseType;
use pagable::arc_erase::StdArcEraseType;
use pagable::arc_erase::deserialize_arc;
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
use crate::target::label::interner::global_intern;
use crate::target::label::triomphe_thin_arc_borrow::ThinArcBorrow;
use crate::target::name::TargetNameRef;

#[derive(Debug, Eq, PartialEq, Allocative)]
pub(crate) struct TargetLabelHeader {
    /// Hash of target label (not package, not name). Stored because it must
    /// stay bit-stable — it feeds target hashes (`TargetNode::target_hash`,
    /// surfaced by `targets --show-target-hash`) — and it doubles as the
    /// precomputed `Hash` for label-keyed maps. Equality does not read it:
    /// labels are interned, so pointer comparison suffices.
    hash: u32,
    pkg: PackageLabel,
    // TODO(nga): this struct has 4 bytes of padding.
}

/// The owning form of a target label allocation.
///
/// Exactly one of these exists per unique label, and it lives forever inside
/// the global interner's table. All public `TargetLabel` values are
/// non-refcounted borrows of these entries.
///
/// Visibility is deliberately restricted to this module tree: storing this
/// type in any table other than the immortal global interner would let its
/// allocation be freed behind live `'static` `TargetLabel` handles.
#[derive(Allocative)]
pub(in crate::target::label) struct OwnedTargetLabel(ThinArc<TargetLabelHeader, u8>);

impl OwnedTargetLabel {
    /// This computation must not change: the truncated `u32` feeds target
    /// hashes (`TargetNode::target_hash`), which are compared across daemons.
    pub(crate) fn label_hash(pkg: PackageLabel, name: &TargetNameRef) -> u64 {
        let key = &(pkg.dupe(), &name);
        let mut hasher = BuckHasher::default();
        key.hash(&mut hasher);
        hasher.finish()
    }

    pub(crate) fn alloc(pkg: PackageLabel, name: &TargetNameRef, hash: u64) -> OwnedTargetLabel {
        OwnedTargetLabel(ThinArc::from_header_and_slice(
            TargetLabelHeader {
                hash: hash as u32,
                pkg,
            },
            name.as_str().as_bytes(),
        ))
    }
}

/// This impl hands out `'static` `TargetLabel`s untied to the table's
/// lifetime, which is sound only for the immortal global interner. Inside
/// `insert`, the eq callback can also receive a handle to a not-yet-inserted
/// candidate that is freed if it loses the insert race; such handles must
/// never escape the callback.
impl AtomicValue for OwnedTargetLabel {
    type Raw = usize; // *const ()
    type Ref<'a> = TargetLabel;

    fn null() -> Self::Raw {
        0
    }

    fn is_null(this: Self::Raw) -> bool {
        this == 0
    }

    fn into_raw(this: Self) -> Self::Raw {
        (ThinArc::into_raw(this.0) as *const ()).expose_provenance()
    }

    unsafe fn from_raw(raw: Self::Raw) -> Self {
        // SAFETY: `raw` came from `into_raw` on an `OwnedTargetLabel`, so it
        // is a live `ThinArc` allocation whose ownership we are resuming.
        OwnedTargetLabel(unsafe {
            ThinArc::from_raw(ptr::with_exposed_provenance::<()>(raw) as *const _)
        })
    }

    unsafe fn deref<'a>(raw: Self::Raw) -> Self::Ref<'a> {
        // SAFETY: `raw` came from `into_raw`, and the allocation stays live:
        // entries are never removed from the immortal global interner.
        TargetLabel(unsafe { ThinArcBorrow::from_raw(ptr::with_exposed_provenance(raw)) })
    }
}

/// 'TargetLabel' that uniquely maps to a 'target'
/// It contains a 'Package' which is the 'Package' defined by the build fine
/// that contains this 'target', and a 'name' which is a 'TargetName'
/// representing the target name given to the particular target.
///
/// Every distinct label is interned exactly once in a global, immortal
/// interner, so this type is a `Copy` handle: cloning and dropping it is free,
/// and equality is pointer equality. The backing allocation is never freed.
#[derive(Copy, Clone, derive_more::Display)]
#[display("{}", self.as_ref())]
pub struct TargetLabel(ThinArcBorrow<'static, TargetLabelHeader, u8>);

// SAFETY: `TargetLabel` points to an allocation owned by the global interner,
// which is immutable after construction and is never freed, so sharing the
// pointer across threads is sound.
unsafe impl Send for TargetLabel {}
// SAFETY: as above; all access is to immutable data.
unsafe impl Sync for TargetLabel {}

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

impl PartialEq for TargetLabel {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        // Sound because every `TargetLabel` is created by the global interner:
        // one allocation exists per distinct (package, name).
        ptr::eq(self.as_raw(), other.as_raw())
    }
}

impl Eq for TargetLabel {}

#[allow(clippy::derived_hash_with_manual_eq)]
impl Hash for TargetLabel {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.header().hash.hash(state);
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
        global_intern(pkg, name)
    }

    #[inline]
    fn header(&self) -> &'static TargetLabelHeader {
        self.0.with_arc(|arc| {
            let header: *const TargetLabelHeader = &arc.header.header;
            // SAFETY: the allocation is owned by the global interner and is
            // never freed or mutated, so extending the borrow to `'static`
            // is sound.
            unsafe { &*header }
        })
    }

    #[inline]
    fn name_bytes(&self) -> &'static [u8] {
        self.0.with_arc(|arc| {
            let slice: *const [u8] = &arc.slice;
            // SAFETY: as in `header`.
            unsafe { &*slice }
        })
    }

    #[inline]
    pub fn pkg(&self) -> PackageLabel {
        self.header().pkg.dupe()
    }

    #[inline]
    pub fn name(&self) -> &'static TargetNameRef {
        // SAFETY(utf8): the bytes were copied from a valid `str` at
        // construction and are immutable.
        let name = unsafe { str::from_utf8_unchecked(self.name_bytes()) };
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

    pub(crate) fn as_raw(&self) -> *const () {
        self.0.as_ptr()
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

impl Allocative for TargetLabel {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut Visitor<'b>) {
        let mut visitor = visitor.enter_self_sized::<Self>();
        // `ThinArc`'s impl already deduplicates by allocation pointer via
        // `enter_shared`, so the payload is counted once per report no matter
        // how many handles or the interner itself visit it.
        self.0.with_arc(|arc| arc.visit(&mut visitor));
        visitor.exit();
    }
}

impl ArcErase for TargetLabel {
    type Weak = ();

    fn dupe_strong(&self) -> Self {
        *self
    }

    fn erase_type() -> impl ArcEraseType {
        StdArcEraseType::<Self>::new()
    }

    fn identity(&self) -> usize {
        // One allocation exists per distinct label, and it is never freed,
        // so the address is a stable unique identity.
        self.as_raw() as usize
    }

    fn downgrade(&self) -> Option<Self::Weak> {
        None
    }

    fn serialize_inner(&self, ser: &mut dyn PagableSerializer) -> pagable::Result<()> {
        self.pkg().pagable_serialize(ser)?;
        self.name().as_str().pagable_serialize(ser)
    }

    fn deserialize_inner<'de, D: PagableDeserializer<'de> + ?Sized>(
        deser: &mut D,
    ) -> pagable::Result<Self> {
        let pkg = PackageLabel::pagable_deserialize(deser)?;
        let name = String::pagable_deserialize(deser)?;
        // Re-validate: a label minted here becomes a permanent entry in the
        // global interner, so corrupted storage must not be trusted.
        let name = TargetNameRef::new(&name)
            .map_err(|e| pagable::anyhow!("Invalid target name in paged data: {e:#}"))?;
        Ok(TargetLabel::new(pkg, name))
    }
}

impl PagableSerialize for TargetLabel {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> pagable::Result<()> {
        serializer.serialize_arc(self)
    }
}

impl<'de> PagableDeserialize<'de> for TargetLabel {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Self> {
        deserialize_arc::<Self, _>(deserializer)
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
