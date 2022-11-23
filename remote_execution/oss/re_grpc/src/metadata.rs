/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#[derive(Clone, Default)]
pub struct TProperty {
    pub name: String,
    pub value: String,
    pub _dot_dot: (),
}

#[derive(Clone, Default)]
pub struct TPlatform {
    pub properties: Vec<TProperty>,
    pub _dot_dot: (),
}

#[derive(Clone, Default)]
pub struct ActionHistoryInfo {
    pub action_key: String,
    pub disable_retry_on_oom: bool,
    pub _dot_dot: (),
}

#[derive(Clone, Default)]
pub struct HostResourceRequirements {
    pub affinity_keys: Vec<String>,
    pub input_files_bytes: i64,
    pub _dot_dot: (),
}

#[derive(Clone, Default)]
pub struct RemoteExecutionMetadata {
    pub action_history_info: Option<ActionHistoryInfo>,
    pub host_resource_requirements: Option<HostResourceRequirements>,
    pub platform: Option<TPlatform>,
    pub use_case_id: String,
    pub _dot_dot: (),
}
