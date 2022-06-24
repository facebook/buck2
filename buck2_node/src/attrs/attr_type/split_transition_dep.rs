/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_core::configuration::transition::id::TransitionId;

use crate::attrs::attr_type::dep::ProviderIdSet;

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct SplitTransitionDepAttrType {
    pub required_providers: Option<Arc<ProviderIdSet>>,
    pub transition: Arc<TransitionId>,
}

impl SplitTransitionDepAttrType {
    pub fn new(required_providers: ProviderIdSet, transition: Arc<TransitionId>) -> Self {
        let required_providers = if required_providers.is_empty() {
            None
        } else {
            Some(Arc::new(required_providers))
        };
        SplitTransitionDepAttrType {
            required_providers,
            transition,
        }
    }
}
