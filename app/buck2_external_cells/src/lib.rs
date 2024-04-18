/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use async_trait::async_trait;
#[allow(unused)] // TODO(JakobDegen): Use in next diff
use buck2_external_cells_bundled::get_bundled_data as _unused;

struct ConcreteExternalCellsImpl;

#[async_trait]
impl buck2_common::external_cells::ExternalCellsImpl for ConcreteExternalCellsImpl {}

pub fn init_late_bindings() {
    buck2_common::external_cells::EXTERNAL_CELLS_IMPL.init(&ConcreteExternalCellsImpl);
}
