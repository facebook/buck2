/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::cells::name::CellName;
use buck2_external_cells_bundled::get_bundled_data;
use buck2_external_cells_bundled::BundledCell;

pub(crate) fn find_bundled_data(cell_name: CellName) -> anyhow::Result<BundledCell> {
    #[derive(buck2_error::Error, Debug)]
    #[error("No bundled cell named `{0}`, options are `{}`", _1.join(", "))]
    struct CellNotBundled(String, Vec<&'static str>);

    let cell_name = cell_name.as_str();

    get_bundled_data()
        .iter()
        .find(|data| data.name == cell_name)
        .copied()
        .ok_or_else(|| {
            CellNotBundled(
                cell_name.to_owned(),
                get_bundled_data()
                    .iter()
                    .filter(|data| !data.is_testing)
                    .map(|data| data.name)
                    .collect(),
            )
            .into()
        })
}
