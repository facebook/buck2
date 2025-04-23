/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_artifact::actions::key::ActionKey;

#[derive(Clone)]
pub struct ActionExecutionMetrics {
    pub key: ActionKey,
    pub execution_time_ms: u64,
    pub execution_kind: buck2_data::ActionExecutionKind,
    pub output_size_bytes: u64,
}
