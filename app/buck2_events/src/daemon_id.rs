/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_wrapper_common::invocation_id::TraceId;
use once_cell::sync::Lazy;

pub static DAEMON_UUID: Lazy<TraceId> = Lazy::new(TraceId::new);
