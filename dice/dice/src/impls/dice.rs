/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;
use std::future::Future;
use std::io::Write;
use std::sync::Arc;

use allocative::Allocative;
use dupe::Dupe;
use serde::Serializer;

use crate::api::transaction::DiceTransactionUpdater;
use crate::api::user_data::UserComputationData;
use crate::impls::key_index::DiceKeyIndex;
use crate::impls::transaction::TransactionUpdater;
use crate::transaction_update::DiceTransactionUpdaterImpl;
use crate::DetectCycles;

#[derive(Allocative)]
pub(crate) struct DiceModern {
    pub(crate) key_index: DiceKeyIndex,
}

impl Debug for DiceModern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DiceModern").finish_non_exhaustive()
    }
}

impl DiceModern {
    #[allow(unused)]
    pub(crate) fn new() -> Arc<Self> {
        Arc::new(DiceModern {
            key_index: Default::default(),
        })
    }

    pub fn updater(self: &Arc<Self>) -> DiceTransactionUpdater {
        DiceTransactionUpdater(DiceTransactionUpdaterImpl::Modern(TransactionUpdater::new(
            self.dupe(),
        )))
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
