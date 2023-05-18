/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_artifact::actions::key::ActionKey;
use buck2_artifact::deferred::data::DeferredData;

use crate::actions::RegisteredAction;

pub trait ActionKeyExt {
    #[allow(clippy::new_ret_no_self)]
    fn new(key: DeferredData<Arc<RegisteredAction>>) -> ActionKey;
    fn deferred_data(&self) -> &DeferredData<Arc<RegisteredAction>>;
}

impl ActionKeyExt for ActionKey {
    fn new(key: DeferredData<Arc<RegisteredAction>>) -> ActionKey {
        ActionKey::unchecked_new(key.into_deferred_key())
    }

    fn deferred_data(&self) -> &DeferredData<Arc<RegisteredAction>> {
        DeferredData::unchecked_new_ref(self.deferred_key())
    }
}
