/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use buck2_util::hash::BuckHasher;
use cmp_any::PartialEqAny;
use dupe::Dupe;

/// `Display` should not include type name (like `dice::Key`).
pub trait BuildSignalsNodeKeyImpl:
    Eq + PartialEq + Hash + Display + Debug + Send + Sync + 'static
{
    fn critical_path_entry_proto(&self) -> Option<buck2_data::critical_path_entry2::Entry> {
        None
    }
}

pub trait BuildSignalsNodeKeyDyn: Send + Sync + 'static {
    fn eq_token(&self) -> PartialEqAny;
    fn dislpay(&self) -> &dyn Display;
    fn debug(&self) -> &dyn Debug;
    fn critical_path_entry_proto(&self) -> Option<buck2_data::critical_path_entry2::Entry>;
    fn hash(&self) -> u64;
}

impl<T: BuildSignalsNodeKeyImpl> BuildSignalsNodeKeyDyn for T {
    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }

    fn dislpay(&self) -> &dyn Display {
        self
    }

    fn debug(&self) -> &dyn Debug {
        self
    }

    fn critical_path_entry_proto(&self) -> Option<buck2_data::critical_path_entry2::Entry> {
        self.critical_path_entry_proto()
    }

    fn hash(&self) -> u64 {
        let mut hasher = BuckHasher::new();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

#[derive(Clone, Dupe)]
pub struct BuildSignalsNodeKey(Arc<dyn BuildSignalsNodeKeyDyn>);

impl BuildSignalsNodeKey {
    pub fn new<T: BuildSignalsNodeKeyImpl + 'static>(key: T) -> Self {
        BuildSignalsNodeKey(Arc::new(key))
    }

    pub fn critical_path_entry_proto(&self) -> Option<buck2_data::critical_path_entry2::Entry> {
        self.0.critical_path_entry_proto()
    }
}

impl PartialEq for BuildSignalsNodeKey {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_token() == other.0.eq_token()
    }
}

impl Eq for BuildSignalsNodeKey {}

impl Display for BuildSignalsNodeKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.0.dislpay(), f)
    }
}

impl Debug for BuildSignalsNodeKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.0.debug(), f)
    }
}

impl Hash for BuildSignalsNodeKey {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.0.hash());
    }
}
