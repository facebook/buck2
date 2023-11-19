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
use remote_execution::{BuckInfo, RemoteExecutionMetadata};

pub trait RemoteExecutionMetadataExt {
    fn metadata(&self) -> RemoteExecutionMetadata;
}

impl RemoteExecutionMetadataExt for RemoteExecutorUseCase {
    fn metadata(&self) -> RemoteExecutionMetadata {
        let trace_id = get_dispatcher().trace_id().to_owned();
        RemoteExecutionMetadata {
            use_case_id: self.as_str().to_owned(),
            buck_info: Some(BuckInfo{
                build_id: trace_id.to_string(),
                ..Default::default()
            }),
            ..Default::default()
        }
    }
}
