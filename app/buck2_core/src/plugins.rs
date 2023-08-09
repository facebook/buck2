/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use derive_more::Display;
use dupe::Dupe;
use internment_tweaks::Intern;
use internment_tweaks::StaticInterner;

use crate::cells::cell_path::CellPath;

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
