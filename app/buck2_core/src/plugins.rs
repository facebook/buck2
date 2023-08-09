/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeMap;

use allocative::Allocative;
use buck2_util::collections::ordered_map::OrderedMap;
use derive_more::Display;
use dupe::Dupe;
use internment_tweaks::Intern;
use internment_tweaks::StaticInterner;

use crate::cells::cell_path::CellPath;
use crate::target::label::TargetLabel;

#[derive(
    Clone, Debug, Display, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative
)]
#[display(fmt = "{name}")]
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
    Clone, Dupe, Debug, Display, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative
)]
pub struct PluginKind(Intern<PluginKindInner>);

static PLUGIN_KIND_INTERNER: StaticInterner<PluginKindInner> = StaticInterner::new();

impl PluginKind {
    /// Creates a new `PluginKind` instance.
    pub fn new(name: String, cell: CellPath) -> Self {
        // FIXME(JakobDegen): Interning is overkill here, this is never called with the same
        // arguments twice. However, we do want pointer equality and pre-hashing, is there an easier
        // way to get that?
        Self(PLUGIN_KIND_INTERNER.intern(&PluginKindInner { name, cell }))
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
#[derive(Clone, Dupe, Debug, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative)]
pub struct PluginKindSet(Option<Intern<Box<[(PluginKind, bool)]>>>);

static PLUGIN_KIND_SET_INTERNER: StaticInterner<Box<[(PluginKind, bool)]>> = StaticInterner::new();

impl PluginKindSet {
    pub const EMPTY: Self = Self(None);

    pub fn new(pulls: Vec<PluginKind>, pulls_and_pushes: Vec<PluginKind>) -> anyhow::Result<Self> {
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

        Ok(Self(Some(PLUGIN_KIND_SET_INTERNER.intern(&kinds[..]))))
    }

    pub fn is_empty(&self) -> bool {
        *self == Self::EMPTY
    }

    pub fn get(&self, kind: &PluginKind) -> Option<bool> {
        self.0
            .as_ref()?
            .iter()
            .find_map(|(k, v)| if k == kind { Some(*v) } else { None })
    }
}

/// Elements in the plugin list come in three kinds: Either they appear as direct `plugin_dep`s on
/// the rule, or they arrive indirectly and may or may not need to be propagated.
#[derive(
    Copy, Clone, Dupe, Debug, Display, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative
)]
pub enum PluginListElemKind {
    Direct,
    Propagate,
    NoPropagate,
}

// TODO(JakobDegen): Representation with fewer allocations
#[derive(
    Clone, Debug, Default, Eq, PartialEq, Hash, Ord, PartialOrd, Allocative
)]
pub struct PluginLists(BTreeMap<PluginKind, OrderedMap<TargetLabel, PluginListElemKind>>);

impl PluginLists {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn insert(&mut self, kind: PluginKind, target: TargetLabel, elem_kind: PluginListElemKind) {
        // FIXME(JakobDegen): This should more carefully think about how repeated insertion should work
        self.0.entry(kind).or_default().insert(target, elem_kind);
    }

    pub fn iter<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&'a PluginKind, &'a TargetLabel, &'a PluginListElemKind)> {
        self.0
            .iter()
            .flat_map(|(k, v)| v.iter().map(move |t| (k, t.0, t.1)))
    }

    pub fn iter_by_kind<'a>(
        &'a self,
    ) -> impl Iterator<
        Item = (
            &'a PluginKind,
            impl Iterator<Item = (&'a TargetLabel, &'a PluginListElemKind)>,
        ),
    > {
        self.0.iter().map(|(k, v)| (k, v.iter()))
    }
}
