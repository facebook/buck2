/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::bzl::ImportPath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::paths::CellRelativePathBuf;
use buck2_core::cells::CellResolver;

pub fn prelude_path(cell_resolver: &CellResolver) -> anyhow::Result<ImportPath> {
    let alias_resolver = cell_resolver.root_cell_cell_alias_resolver();
    let prelude_cell = alias_resolver.resolve("prelude")?;
    let prelude_file = CellRelativePathBuf::unchecked_new("prelude.bzl".to_owned());
    ImportPath::new_same_cell(CellPath::new(prelude_cell, prelude_file))
}
