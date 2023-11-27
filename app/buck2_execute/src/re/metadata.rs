/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_events::dispatch::get_dispatcher;
use remote_execution::{ActionHistoryInfo, BuckInfo, HostResourceRequirements, RemoteExecutionMetadata};
use crate::re::action_identity::ReActionIdentity;

pub trait RemoteExecutionMetadataExt {
    fn metadata(&self, identity: Option<&ReActionIdentity>) -> RemoteExecutionMetadata;
}

impl RemoteExecutionMetadataExt for RemoteExecutorUseCase {
    fn metadata(&self, identity: Option<&ReActionIdentity>) -> RemoteExecutionMetadata {
        let trace_id = get_dispatcher().trace_id().to_owned();
        RemoteExecutionMetadata {
            use_case_id: self.as_str().to_owned(),
            buck_info: Some(BuckInfo{
                build_id: trace_id.to_string(),
                ..Default::default()
            }),
            action_history_info: identity.map(|identity| ActionHistoryInfo {
                action_key: identity.action_key.clone(),
                disable_retry_on_oom: false,
                ..Default::default()
            }),
            host_resource_requirements: identity.map(|identity| HostResourceRequirements {
                affinity_keys: vec![identity.affinity_key.clone()],
                input_files_bytes: identity.paths.input_files_bytes() as i64,
                ..Default::default()
            }),
            ..Default::default()
        }
    }
}
