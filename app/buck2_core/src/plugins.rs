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

use allocative::Allocative;
use buck2_util::hash::BuckHasher;
use derive_more::Display;
use dupe::Dupe;
use pagable::Pagable;
use pagable::PagableDeserialize;
use pagable::PagableDeserializer;
use pagable::PagableSerialize;
use pagable::PagableSerializer;
use serde::Deserialize;
use serde::Serialize;
use starlark_map::ordered_map::OrderedMap;
use starlark_map::small_map::Entry;
use static_interner::Intern;
use static_interner::interner;

use crate::cells::cell_path::CellPath;
use crate::target::label::label::TargetLabel;

#[derive(
    Clone,
    Pagable,
    strong_hash::StrongHash,
    Debug,
    Display,
    Eq,
    PartialEq,
    Hash,
    Ord,
    PartialOrd,
    Allocative,
    Serialize,
    Deserialize
)]
#[display("{name}")]
struct PluginKindInner {
    // The name and cell path aren't used for anything except that they serve as a unique identifier
    // for the plugin kind. This allows us to treat `plugins.kind()` as if it returns a new value
    // each time it's invoked.
    name: String,
    cell: CellPath,
}

impl<'a> From<&'a PluginKindInner> for PluginKindInner {
    fn from(x: &'a PluginKindInner) -> Self {
        x.clone()
    }
}

#[derive(
    Clone,
    Dupe,
    Debug,
    Display,
    Eq,
    PartialEq,
    Hash,
    Ord,
    PartialOrd,
    Allocative,
    strong_hash::StrongHash,
    Pagable
)]
pub struct PluginKind(Intern<PluginKindInner>);

interner!(PLUGIN_KIND_INTERNER, BuckHasher, PluginKindInner);

impl PluginKind {
    /// Creates a new `PluginKind` instance.
    pub fn new(name: String, cell: CellPath) -> Self {
        // FIXME(JakobDegen): Interning is overkill here, this is never called with the same
        // arguments twice. However, we do want pointer equality and pre-hashing, is there an easier
        // way to get that?
        Self(PLUGIN_KIND_INTERNER.intern(PluginKindInner { name, cell }))
    }

    pub fn as_str(&self) -> &str {
        &self.0.deref_static().name
    }
}

/// This type is pretty tailor-made for storing the values of `pulls_plugins` and
/// `pulls_and_pushes_plugins` on `attrs.dep()`.
///
/// It stores a list of plugin kinds and a bool for each indicating whether that kind is pushed in
/// addition to pulled.
#[derive(Copy, Clone, Dupe)]
pub struct PluginKindSet(*const ());

impl PagableSerialize for PluginKindSet {
    fn pagable_serialize(&self, serializer: &mut dyn PagableSerializer) -> pagable::Result<()> {
        self.unpack().pagable_serialize(serializer)
    }
}

impl<'de> PagableDeserialize<'de> for PluginKindSet {
    fn pagable_deserialize<D: PagableDeserializer<'de> + ?Sized>(
        deserializer: &mut D,
    ) -> pagable::Result<Self> {
        Ok(PluginKindSet::pack(
            PluginKindSetUnpacked::pagable_deserialize(deserializer)?,
        ))
    }
}

/// We'd ideally like to just let this type be the definition of `PluginKindSet`. Unfortunately,
/// this type is 16 bytes in size. So instead, we store `PluginKindSet` as a pointer with `0`
/// indicating `None` and `1` indicating `All`
#[derive(
    Clone,
    Dupe,
    Debug,
    Eq,
    PartialEq,
    Hash,
    Ord,
    PartialOrd,
    Allocative,
    strong_hash::StrongHash,
    Pagable
)]
enum PluginKindSetUnpacked {
    None,
    All,
    Interned(Intern<PluginKindSetData>),
}

#[derive(
    Clone,
    Debug,
    Eq,
    PartialEq,
    Hash,
    Ord,
    PartialOrd,
    Allocative,
    strong_hash::StrongHash,
    Pagable
)]
struct PluginKindSetData(Vec<(PluginKind, bool)>);

static_assertions::assert_eq_size!(PluginKindSet, usize);
static_assertions::assert_eq_size!(PluginKindSetUnpacked, [usize; 2]);

interner!(
    PLUGIN_KIND_SET_INTERNER,
    BuckHasher,
    PluginKindSetData,
    Vec<(PluginKind, bool)>,
    [(PluginKind, bool)]
);

impl PluginKindSet {
    pub const EMPTY: Self = Self::pack(PluginKindSetUnpacked::None);
    pub const ALL: Self = Self::pack(PluginKindSetUnpacked::All);

    pub fn new(
        pulls: Vec<PluginKind>,
        pulls_and_pushes: Vec<PluginKind>,
    ) -> buck2_error::Result<Self> {
        if pulls.is_empty() && pulls_and_pushes.is_empty() {
            return Ok(Self::EMPTY);
        }

        let mut kinds = BTreeMap::new();
        for kind in pulls {
            kinds.insert(kind, false);
        }
        for kind in pulls_and_pushes {
            kinds.insert(kind, true);
        }
        let kinds = kinds.into_iter().collect::<Vec<_>>();

        Ok(Self::pack(PluginKindSetUnpacked::Interned(
            PLUGIN_KIND_SET_INTERNER.intern(PluginKindSetData(kinds)),
        )))
    }

    pub fn is_empty(&self) -> bool {
        *self == Self::EMPTY
    }

    pub fn get(&self, kind: &PluginKind) -> Option<bool> {
        match self.unpack() {
            PluginKindSetUnpacked::None => None,
            PluginKindSetUnpacked::All => Some(true),
            PluginKindSetUnpacked::Interned(i) => i
                .iter()
                .find_map(|(k, v)| if k == kind { Some(*v) } else { None }),
        }
    }

    fn unpack(self) -> PluginKindSetUnpacked {
        if self.0 as usize == 0 {
            PluginKindSetUnpacked::None
        } else if self.0 as usize == 1 {
            PluginKindSetUnpacked::All
        } else {
            // SAFETY: Instances of this type are only created by `pack`
            PluginKindSetUnpacked::Interned(unsafe { Intern::from_ptr(self.0 as *const _) })
        }
    }

    const fn pack(unpacked: PluginKindSetUnpacked) -> Self {
        match unpacked {
            PluginKindSetUnpacked::None => PluginKindSet(std::ptr::null::<()>()),
            PluginKindSetUnpacked::All => PluginKindSet(std::ptr::dangling::<()>()),
            PluginKindSetUnpacked::Interned(i) => {
                PluginKindSet(i.deref_static() as *const _ as *const ())
            }
        }
    }
}

impl std::fmt::Debug for PluginKindSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.unpack(), f)
    }
}

impl PartialEq<PluginKindSet> for PluginKindSet {
    fn eq(&self, other: &Self) -> bool {
        PartialEq::eq(&self.unpack(), &other.unpack())
    }
}

impl Eq for PluginKindSet {}

impl std::hash::Hash for PluginKindSet {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::hash::Hash::hash(&self.unpack(), state)
    }
}

impl strong_hash::StrongHash for PluginKindSet {
    fn strong_hash<H: std::hash::Hasher>(&self, state: &mut H) {
        strong_hash::StrongHash::strong_hash(&self.unpack(), state)
    }
}

impl PartialOrd for PluginKindSet {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PluginKindSet {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        Ord::cmp(&self.unpack(), &other.unpack())
    }
}

impl Allocative for PluginKindSet {
    fn visit<'a, 'b: 'a>(&self, visitor: &'a mut allocative::Visitor<'b>) {
        Allocative::visit(&self.unpack(), visitor)
    }
}

// SAFETY: `PluginKindSet` is a trivial wrapper only changing the representation
unsafe impl Sync for PluginKindSet where PluginKindSetUnpacked: Sync {}
unsafe impl Send for PluginKindSet where PluginKindSetUnpacked: Send {}

/// Elements in the plugin list come in three kinds: Either they appear as direct `plugin_dep`s on
/// the rule, or they arrive indirectly and may or may not need to be propagated.
///
/// Note that the `Ord` impl on this type is semantically meaningful - larger values indicate
/// stronger kinds of membership, and when a plugin enters the plugin lists twice the larger of the
/// two values is preferred.
#[derive(
    Copy, Clone, Dupe, Debug, Display, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative, Pagable
)]
pub enum PluginListElemKind {
    NoPropagate,
    Propagate,
    Direct,
}

// TODO(JakobDegen): Representation with fewer allocations
#[derive(
    Clone, Debug, Default, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative, Pagable
)]
pub struct PluginLists(BTreeMap<PluginKind, OrderedMap<TargetLabel, PluginListElemKind>>);

impl PluginLists {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&mut self, kind: PluginKind, target: TargetLabel, elem_kind: PluginListElemKind) {
        match self.0.entry(kind).or_default().entry(target) {
            Entry::Occupied(mut occupied) => {
                let current = occupied.get_mut();
                if *current < elem_kind {
                    *current = elem_kind;
                }
            }
            Entry::Vacant(vacant) => {
                vacant.insert(elem_kind);
            }
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&PluginKind, &TargetLabel, &PluginListElemKind)> {
        self.0
            .iter()
            .flat_map(|(k, v)| v.iter().map(move |t| (k, t.0, t.1)))
    }

    pub fn iter_by_kind(
        &self,
    ) -> impl Iterator<
        Item = (
            &PluginKind,
            impl Iterator<Item = (&TargetLabel, &PluginListElemKind)>,
        ),
    > {
        self.0.iter().map(|(k, v)| (k, v.iter()))
    }

    pub fn iter_for_kind(
        &self,
        kind: &PluginKind,
    ) -> impl Iterator<Item = (&TargetLabel, &PluginListElemKind)> + use<'_> {
        self.0.get(kind).into_iter().flatten()
    }
}
