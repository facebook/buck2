/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use buck2_common::ignores::ignore_set::IgnoreSet;
use buck2_core::cells::CellResolver;
use buck2_core::cells::name::CellName;
use buck2_core::fs::i_project_rel_path::IProjectRelativePath;
use buck2_hash::BuckMutMap;

/// Type that file watchers use to determine if a file is ignored
///
/// Although we also check for ignores within dice after reading a file/directory/etc., we need to
/// check this here also specifically so that adding an ignored file doesn't invalidate the listing
/// for that file's parent directory.
#[derive(Allocative, Clone)]
pub struct FileWatcherIgnores {
    // FIXME(JakobDegen): This is implemented by storing a cell resolver and the `project.ignore`
    // data from daemon startup, but that's completely wrong - either of these might change, and if
    // they do, we'll just report incorrect file changes.
    cells: CellResolver,
    specs: BuckMutMap<CellName, IgnoreSet>,
}

impl FileWatcherIgnores {
    pub fn new(cells: CellResolver, specs: BuckMutMap<CellName, IgnoreSet>) -> Self {
        Self { cells, specs }
    }

    pub(crate) fn is_ignored(&self, path: IProjectRelativePath) -> bool {
        let cell_path = self.cells.get_cell_path(&path);
        self.specs
            .get(&cell_path.cell())
            .map(|ignore| ignore.is_match(cell_path.path()))
            // This shouldn't ever really happen. However, because of the bugs caused by just
            // storing the `CellResolver` in the watcher permanently, sometimes it can, so we just
            // default to not ignoring the file in that case
            .unwrap_or(false)
    }
}
