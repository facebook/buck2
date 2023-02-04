/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::io::Write;

use allocative::Allocative;
use dupe::Dupe;
use serde::Serializer;

use crate::api::transaction::DiceTransactionUpdater;
use crate::api::user_data::UserComputationData;
use crate::DetectCycles;

#[derive(Allocative, Debug, Dupe, Clone)]
pub(crate) struct DiceModern;

impl DiceModern {
    pub fn updater(&self) -> DiceTransactionUpdater {
        unimplemented!("todo")
    }

    pub fn updater_with_data(&self, _extra: UserComputationData) -> DiceTransactionUpdater {
        unimplemented!("todo")
    }

    pub fn serialize_tsv(
        &self,
        _nodes: impl Write,
        _edges: impl Write,
        _nodes_currently_running: impl Write,
    ) -> anyhow::Result<()> {
        unimplemented!("todo")
    }

    pub fn serialize_serde<S>(&self, _serializer: S) -> Result<(), S::Error>
    where
        S: Serializer,
    {
        unimplemented!("todo")
    }

    pub fn detect_cycles(&self) -> &DetectCycles {
        unimplemented!("todo")
    }

    /// Wait until all active versions have exited.
    pub fn wait_for_idle(&self) -> impl Future<Output = ()> + 'static {
        async move { unimplemented!("todo") }
    }
}
