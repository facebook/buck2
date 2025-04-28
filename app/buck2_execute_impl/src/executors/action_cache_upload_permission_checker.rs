/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::hash::Hash;
use std::sync::Arc;

use buck2_core::async_once_cell::AsyncOnceCell;
use buck2_core::execution_types::executor_config::RePlatformFields;
use buck2_core::execution_types::executor_config::RemoteExecutorUseCase;
use buck2_error::BuckErrorContext;
use buck2_execute::re::error::RemoteExecutionError;
use buck2_execute::re::manager::ManagedRemoteExecutionClient;
use dashmap::DashMap;
use dupe::Dupe;
use remote_execution::TCode;

use crate::executors::empty_action_result::empty_action_result;
use crate::executors::to_re_platform::RePlatformFieldsToRePlatform;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
struct CacheKey {
    re_use_case: RemoteExecutorUseCase,
    platform: RePlatformFields,
}

struct CacheValue {
    has_permission_to_upload_to_cache: AsyncOnceCell<Result<(), String>>,
}

/// Check permission to upload to action cache and cache result.
pub struct ActionCacheUploadPermissionChecker {
    /// Permission check does not depend on RE use case,
    /// but since we use these to upload, it is safer to cache the result by them.
    has_permission_to_upload_to_cache: DashMap<CacheKey, Arc<CacheValue>>,
}

impl ActionCacheUploadPermissionChecker {
    pub fn new() -> ActionCacheUploadPermissionChecker {
        ActionCacheUploadPermissionChecker {
            has_permission_to_upload_to_cache: DashMap::new(),
        }
    }

    async fn do_has_permission_to_upload_to_cache(
        &self,
        re_client: &ManagedRemoteExecutionClient,
        platform: &RePlatformFields,
    ) -> buck2_error::Result<Result<(), String>> {
        let (action, action_result) = empty_action_result(platform)?;

        // This is CAS upload, if it fails, something is very broken.
        re_client
            .upload_files_and_directories(Vec::new(), Vec::new(), action.blobs.to_inlined_blobs())
            .await?;

        // This operation requires permission to write.
        let result = re_client
            .write_action_result(
                action.action,
                action_result.clone(),
                &platform.to_re_platform(),
            )
            .await;
        match result {
            Ok(_) => Ok(Ok(())),
            Err(e) => match e.find_typed_context::<RemoteExecutionError>() {
                Some(e) if e.code == TCode::PERMISSION_DENIED => Ok(Err(e.message.clone())),
                _ => Err(e),
            },
        }
    }

    fn cache_value(
        &self,
        re_use_case: RemoteExecutorUseCase,
        platform: &RePlatformFields,
    ) -> Arc<CacheValue> {
        self.has_permission_to_upload_to_cache
            .entry(CacheKey {
                re_use_case,
                platform: platform.clone(),
            })
            .or_insert_with(|| {
                Arc::new(CacheValue {
                    has_permission_to_upload_to_cache: AsyncOnceCell::new(),
                })
            })
            .dupe()
    }

    pub(crate) async fn has_permission_to_upload_to_cache(
        &self,
        re_client: &ManagedRemoteExecutionClient,
        platform: &RePlatformFields,
    ) -> buck2_error::Result<Result<(), String>> {
        let cache_value = self.cache_value(re_client.use_case, platform);
        cache_value
            .has_permission_to_upload_to_cache
            .get_or_try_init(self.do_has_permission_to_upload_to_cache(re_client, platform))
            .await
            .cloned()
            .buck_error_context("Upload for permission check")
    }
}
