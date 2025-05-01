/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use async_trait::async_trait;
use buck2_util::late_binding::LateBinding;
use dice::UserComputationData;
use dupe::Dupe;

use crate::ctx::ServerCommandContextTrait;
use crate::partial_result_dispatcher::NoPartialResult;
use crate::partial_result_dispatcher::PartialResultDispatcher;

#[async_trait]
pub trait BxlServerCommands: Send + Sync + 'static {
    async fn bxl(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<buck2_cli_proto::StdoutBytes>,
        req: buck2_cli_proto::BxlRequest,
    ) -> buck2_error::Result<buck2_cli_proto::BxlResponse>;
    async fn bxl_profile(
        &self,
        ctx: &dyn ServerCommandContextTrait,
        partial_result_dispatcher: PartialResultDispatcher<NoPartialResult>,
        req: buck2_cli_proto::ProfileRequest,
    ) -> buck2_error::Result<buck2_cli_proto::ProfileResponse>;
}

pub static BXL_SERVER_COMMANDS: LateBinding<&'static dyn BxlServerCommands> =
    LateBinding::new("BXL_SERVER_COMMANDS");

#[derive(Debug, Default)]
pub struct BxlStreamingTracker {
    streaming_output_called: AtomicBool,
}

impl BxlStreamingTracker {
    pub fn mark_as_called(&self) {
        self.streaming_output_called.store(true, Ordering::SeqCst);
    }

    pub fn was_called(&self) -> bool {
        self.streaming_output_called.load(Ordering::SeqCst)
    }
}

pub trait InitBxlStreamingTracker {
    fn init_bxl_streaming_tracker(&mut self);
}

pub trait GetBxlStreamingTracker {
    fn get_bxl_streaming_tracker(&self) -> Option<Arc<BxlStreamingTracker>>;
}

impl InitBxlStreamingTracker for UserComputationData {
    fn init_bxl_streaming_tracker(&mut self) {
        self.data.set(Arc::new(BxlStreamingTracker::default()));
    }
}

impl GetBxlStreamingTracker for UserComputationData {
    fn get_bxl_streaming_tracker(&self) -> Option<Arc<BxlStreamingTracker>> {
        self.data
            .get::<Arc<BxlStreamingTracker>>()
            .map(|v| v.dupe())
            .ok()
    }
}
