/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::executor_config::RemoteExecutorUseCase;
use remote_execution::RemoteExecutionMetadata;

pub trait RemoteExecutionMetadataExt {
    fn metadata(&self) -> RemoteExecutionMetadata;
}

impl RemoteExecutionMetadataExt for RemoteExecutorUseCase {
    fn metadata(&self) -> RemoteExecutionMetadata {
        RemoteExecutionMetadata {
            use_case_id: self.as_str().to_owned(),
            ..Default::default()
        }
    }
}
