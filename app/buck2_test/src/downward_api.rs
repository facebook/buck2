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
use downward_api::DownwardApi;
use tracing::Level;

pub struct BuckTestDownwardApi;

#[async_trait]
impl DownwardApi for BuckTestDownwardApi {
    async fn console(&self, _level: Level, msg: String) -> anyhow::Result<()> {
        // TODO(brasselsprouts): use the level and hook it up with our superconsole
        eprintln!("{}", msg);
        Ok(())
    }

    async fn log(&self, _level: Level, _msg: String) -> anyhow::Result<()> {
        unimplemented!("TODO(bobyf)")
    }

    async fn external(&self, _data: HashMap<String, String>) -> anyhow::Result<()> {
        unimplemented!("need buck event stream to implement")
    }
}
