/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::sync::Arc;

use buck2_core::cells::name::CellName;

/// Buckconfig trait.
///
/// There are two implementations:
/// * simple implementation which is backed by a buckconfig object, used in tests
/// * DICE-backed implementation which records a dependency on buckconfig property in DICE
pub trait LegacyBuckConfigView: Debug {
    fn get(&self, section: &str, key: &str) -> anyhow::Result<Option<Arc<str>>>;
}

/// All cell buckconfigs traits.
pub trait LegacyBuckConfigsView {
    fn get<'a>(&'a self, cell_name: CellName) -> anyhow::Result<&'a dyn LegacyBuckConfigView>;
    fn iter<'a>(
        &'a self,
    ) -> Box<dyn Iterator<Item = (CellName, &'a dyn LegacyBuckConfigView)> + 'a>;
}
