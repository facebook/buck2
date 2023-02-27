/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod action_digest;
pub mod blobs;
pub mod blocking;
pub mod claim;
pub mod clean_output_paths;
pub mod command_executor;
pub mod dice_data;
pub mod environment_inheritance;
pub mod inputs_directory;
pub mod kind;
pub mod manager;
pub mod output;
pub mod prepared;
pub mod request;
pub mod result;
pub mod target;
pub mod testing_dry_run;

use std::future::Future;

use buck2_events::dispatch::span_async;
use futures::FutureExt;

pub fn executor_stage_async<F: Future>(
    stage: impl Into<buck2_data::executor_stage_start::Stage>,
    f: F,
) -> impl Future<Output = <F as Future>::Output> {
    // We avoid using `async fn` or `async move` here to avoid doubling the
    // future size. See https://github.com/rust-lang/rust/issues/62958
    let event = buck2_data::ExecutorStageStart {
        stage: Some(stage.into()),
    };
    span_async(event, f.map(|v| (v, buck2_data::ExecutorStageEnd {})))
}
