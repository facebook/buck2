/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_wrapper_common::invocation_id::TraceId;

/// Information about the current command, such as session or build ids.
pub struct SessionInfo {
    pub trace_id: TraceId,
    pub test_session: Option<buck2_data::TestSessionInfo>,
    pub legacy_dice: bool,
}
