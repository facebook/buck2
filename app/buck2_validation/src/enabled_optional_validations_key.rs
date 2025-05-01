/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeSet;
use std::sync::Arc;

use allocative::Allocative;
use derive_more::Display;
use dice::DiceTransactionUpdater;
use dice::InjectedKey;
use dupe::Dupe;

#[derive(Display, Debug, Hash, Eq, Clone, Dupe, PartialEq, Allocative)]

// DICE key that corresponds to optional validations that are enabled via command line flag.
pub(crate) struct EnabledOptionalValidationsKey;

pub trait SetEnabledOptionalValidations {
    fn set_enabled_optional_validations(
        &mut self,
        validations: Vec<String>,
    ) -> buck2_error::Result<()>;
}

impl InjectedKey for EnabledOptionalValidationsKey {
    type Value = Arc<BTreeSet<String>>;

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

impl SetEnabledOptionalValidations for DiceTransactionUpdater {
    fn set_enabled_optional_validations(
        &mut self,
        validations: Vec<String>,
    ) -> buck2_error::Result<()> {
        Ok(self.changed_to(vec![(
            EnabledOptionalValidationsKey,
            Arc::new(BTreeSet::from_iter(validations)),
        )])?)
    }
}
