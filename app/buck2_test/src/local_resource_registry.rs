/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::local_resource_state::LocalResourceState;
use buck2_common::result::SharedResult;
use buck2_core::target::label::ConfiguredTargetLabel;
use dashmap::DashMap;
use futures::future::BoxFuture;
use futures::future::Shared;

pub(crate) struct LocalResourceRegistry<'a>(
    pub DashMap<ConfiguredTargetLabel, Shared<BoxFuture<'a, SharedResult<LocalResourceState>>>>,
);

impl<'a> LocalResourceRegistry<'a> {
    pub(crate) fn new() -> Self {
        LocalResourceRegistry(DashMap::new())
    }
}
