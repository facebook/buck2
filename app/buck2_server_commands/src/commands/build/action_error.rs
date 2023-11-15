/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Schema for the structured action error within the build report.

use serde::Serialize;

use crate::commands::build::BuildReportCollector;

#[derive(Debug, Clone, Serialize, PartialOrd, Ord, PartialEq, Eq)]
struct BuildReportActionName {
    category: String,
    identifier: String,
}

#[derive(Debug, Clone, Serialize, PartialOrd, Ord, PartialEq, Eq)]
struct BuildReportActionKey {
    owner: String,
}

/// DO NOT UPDATE WITHOUT UPDATING `docs/users/build_observability/build_report.md`!
#[derive(Debug, Clone, Serialize, PartialOrd, Ord, PartialEq, Eq)]
struct BuildReportActionError {
    name: BuildReportActionName,
    key: BuildReportActionKey,
    digest: String,
    error_content: u64,
    stderr_content: u64,
    stdout_content: u64,
}
