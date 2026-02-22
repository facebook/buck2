/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Operation state types.

use chrono::DateTime;
use chrono::Utc;
use serde::Serialize;
use tokio::sync::oneshot;

/// Unique identifier for an operation.
pub type OperationId = String;

/// Operation status.
#[derive(Debug, Clone, Serialize)]
#[serde(tag = "status", rename_all = "snake_case")]
pub enum OperationStatus {
    Running { progress: OperationProgress },
    Completed { result: OperationResult },
    Failed { error: String },
    Cancelled,
}

/// Progress information for a running operation.
#[derive(Debug, Clone, Default, Serialize)]
pub struct OperationProgress {
    pub started_at: String,
    pub elapsed_secs: f64,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub actions_completed: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub actions_total: Option<u64>,
}

impl OperationProgress {
    pub fn new(started_at: DateTime<Utc>) -> Self {
        let elapsed = Utc::now().signed_duration_since(started_at);
        Self {
            started_at: started_at.to_rfc3339(),
            elapsed_secs: elapsed.num_milliseconds() as f64 / 1000.0,
            message: None,
            actions_completed: None,
            actions_total: None,
        }
    }
}

/// Result of a completed operation.
#[derive(Debug, Clone, Serialize)]
pub struct OperationResult {
    pub success: bool,
    pub output: String,
}

/// An async operation.
pub struct Operation {
    pub id: OperationId,
    pub operation_type: String,
    pub started_at: DateTime<Utc>,
    pub status: OperationStatus,
    pub cancel_sender: Option<oneshot::Sender<()>>,
}

impl Operation {
    pub fn new(id: OperationId, operation_type: String) -> (Self, oneshot::Receiver<()>) {
        let (cancel_sender, cancel_receiver) = oneshot::channel();
        let operation = Self {
            id,
            operation_type,
            started_at: Utc::now(),
            status: OperationStatus::Running {
                progress: OperationProgress::new(Utc::now()),
            },
            cancel_sender: Some(cancel_sender),
        };
        (operation, cancel_receiver)
    }

    pub fn update_progress(&mut self, progress: OperationProgress) {
        if matches!(self.status, OperationStatus::Running { .. }) {
            self.status = OperationStatus::Running { progress };
        }
    }

    pub fn complete(&mut self, result: OperationResult) {
        self.status = OperationStatus::Completed { result };
        self.cancel_sender = None;
    }

    pub fn fail(&mut self, error: String) {
        self.status = OperationStatus::Failed { error };
        self.cancel_sender = None;
    }

    pub fn cancel(&mut self) {
        if let Some(sender) = self.cancel_sender.take() {
            let _ = sender.send(());
        }
        self.status = OperationStatus::Cancelled;
    }
}
