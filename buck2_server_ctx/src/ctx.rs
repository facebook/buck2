/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use async_trait::async_trait;
use buck2_common::result::SharedResult;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_events::dispatch::EventDispatcher;
use dice::DiceTransaction;

use crate::raw_output::RawOuputGuard;

#[async_trait]
pub trait ServerCommandContextTrait: Send + Sync + 'static {
    fn working_dir(&self) -> &ProjectRelativePath;

    async fn dice_ctx(&self) -> SharedResult<DiceTransaction>;

    fn events(&self) -> &EventDispatcher;

    fn stdout(&mut self) -> anyhow::Result<RawOuputGuard<'_>>;

    fn request_metadata(&self) -> anyhow::Result<HashMap<String, String>>;
}
