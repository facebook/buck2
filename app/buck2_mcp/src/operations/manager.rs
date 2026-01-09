/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Operation manager for tracking async operations.

use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use tokio::sync::RwLock;
use uuid::Uuid;

use crate::mcp::error::McpError;
use crate::operations::operation::Operation;
use crate::operations::operation::OperationId;
use crate::operations::operation::OperationProgress;
use crate::operations::operation::OperationResult;
use crate::operations::operation::OperationStatus;

/// Manager for async operations.
pub struct OperationManager {
    operations: RwLock<HashMap<OperationId, Operation>>,
}

impl OperationManager {
    /// Create a new operation manager.
    pub fn new() -> Self {
        Self {
            operations: RwLock::new(HashMap::new()),
        }
    }

    /// Create a new operation and return its ID.
    pub async fn create_operation(
        &self,
        operation_type: &str,
    ) -> (OperationId, tokio::sync::oneshot::Receiver<()>) {
        let id = Uuid::new_v4().to_string();
        let (operation, cancel_receiver) = Operation::new(id.clone(), operation_type.to_owned());

        let mut operations = self.operations.write().await;
        operations.insert(id.clone(), operation);

        (id, cancel_receiver)
    }

    /// Update the progress of an operation.
    pub async fn update_progress(&self, id: &OperationId, progress: OperationProgress) {
        let mut operations = self.operations.write().await;
        if let Some(op) = operations.get_mut(id) {
            op.update_progress(progress);
        }
    }

    /// Complete an operation with a result.
    pub async fn complete_operation(&self, id: &OperationId, result: OperationResult) {
        let mut operations = self.operations.write().await;
        if let Some(op) = operations.get_mut(id) {
            op.complete(result);
        }
    }

    /// Fail an operation with an error.
    pub async fn fail_operation(&self, id: &OperationId, error: String) {
        let mut operations = self.operations.write().await;
        if let Some(op) = operations.get_mut(id) {
            op.fail(error);
        }
    }

    /// Cancel an operation.
    pub async fn cancel_operation(&self, id: &OperationId) -> buck2_error::Result<()> {
        let mut operations = self.operations.write().await;
        if let Some(op) = operations.get_mut(id) {
            op.cancel();
            Ok(())
        } else {
            Err(McpError::OperationNotFound(id.clone()).into())
        }
    }

    /// Get the status of an operation.
    pub async fn get_status(&self, id: &OperationId) -> buck2_error::Result<OperationStatus> {
        let operations = self.operations.read().await;
        if let Some(op) = operations.get(id) {
            Ok(op.status.clone())
        } else {
            Err(McpError::OperationNotFound(id.clone()).into())
        }
    }

    /// Wait for an operation to complete with optional timeout.
    pub async fn wait_for_result(
        self: &Arc<Self>,
        id: &OperationId,
        timeout: Option<Duration>,
    ) -> buck2_error::Result<OperationStatus> {
        let deadline = timeout.map(|t| std::time::Instant::now() + t);

        loop {
            let status = self.get_status(id).await?;
            match &status {
                OperationStatus::Running { .. } => {
                    if let Some(deadline) = deadline
                        && std::time::Instant::now() >= deadline
                    {
                        return Ok(status);
                    }
                    tokio::time::sleep(Duration::from_millis(100)).await;
                }
                _ => return Ok(status),
            }
        }
    }

    /// List all operations.
    pub async fn list_operations(&self, include_completed: bool) -> Vec<OperationInfo> {
        let operations = self.operations.read().await;
        operations
            .values()
            .filter(|op| include_completed || matches!(op.status, OperationStatus::Running { .. }))
            .map(|op| OperationInfo {
                id: op.id.clone(),
                operation_type: op.operation_type.clone(),
                status: op.status.clone(),
            })
            .collect()
    }

    /// Clean up old completed operations (call periodically).
    pub async fn cleanup_old_operations(&self, max_age: Duration) {
        let mut operations = self.operations.write().await;
        let now = chrono::Utc::now();

        operations.retain(|_, op| match &op.status {
            OperationStatus::Running { .. } => true,
            _ => {
                let age = now.signed_duration_since(op.started_at);
                age.to_std().unwrap_or(Duration::ZERO) < max_age
            }
        });
    }
}

impl Default for OperationManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Summary info about an operation.
#[derive(Debug, Clone, serde::Serialize)]
pub struct OperationInfo {
    pub id: OperationId,
    pub operation_type: String,
    pub status: OperationStatus,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_create_operation() {
        let manager = OperationManager::new();
        let (id, _cancel_receiver) = manager.create_operation("build").await;

        assert!(!id.is_empty());

        let status = manager.get_status(&id).await.unwrap();
        assert!(matches!(status, OperationStatus::Running { .. }));
    }

    #[tokio::test]
    async fn test_complete_operation() {
        let manager = OperationManager::new();
        let (id, _cancel_receiver) = manager.create_operation("build").await;

        manager
            .complete_operation(
                &id,
                OperationResult {
                    success: true,
                    output: "Build complete".to_owned(),
                },
            )
            .await;

        let status = manager.get_status(&id).await.unwrap();
        match status {
            OperationStatus::Completed { result } => {
                assert!(result.success);
                assert_eq!(result.output, "Build complete");
            }
            _ => panic!("Expected completed status"),
        }
    }

    #[tokio::test]
    async fn test_fail_operation() {
        let manager = OperationManager::new();
        let (id, _cancel_receiver) = manager.create_operation("build").await;

        manager.fail_operation(&id, "Build failed".to_owned()).await;

        let status = manager.get_status(&id).await.unwrap();
        match status {
            OperationStatus::Failed { error } => {
                assert_eq!(error, "Build failed");
            }
            _ => panic!("Expected failed status"),
        }
    }

    #[tokio::test]
    async fn test_cancel_operation() {
        let manager = OperationManager::new();
        let (id, _cancel_receiver) = manager.create_operation("build").await;

        manager.cancel_operation(&id).await.unwrap();

        let status = manager.get_status(&id).await.unwrap();
        assert!(matches!(status, OperationStatus::Cancelled));
    }

    #[tokio::test]
    async fn test_list_operations() {
        let manager = OperationManager::new();

        let (id1, _) = manager.create_operation("build").await;
        let (id2, _) = manager.create_operation("query").await;

        manager
            .complete_operation(
                &id1,
                OperationResult {
                    success: true,
                    output: "Done".to_owned(),
                },
            )
            .await;

        // List only running operations
        let running = manager.list_operations(false).await;
        assert_eq!(running.len(), 1);
        assert_eq!(running[0].id, id2);

        // List all operations
        let all = manager.list_operations(true).await;
        assert_eq!(all.len(), 2);
    }

    #[tokio::test]
    async fn test_get_nonexistent_operation() {
        let manager = OperationManager::new();
        let result = manager.get_status(&"nonexistent".to_owned()).await;
        assert!(result.is_err());
    }
}
