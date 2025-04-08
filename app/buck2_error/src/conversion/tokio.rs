/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_data::error::ErrorTag;

use crate::conversion::from_any_with_tag;

impl<T: Sync + Send + 'static> From<tokio::sync::mpsc::error::SendError<T>> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: tokio::sync::mpsc::error::SendError<T>) -> Self {
        from_any_with_tag(value, ErrorTag::Tokio)
    }
}

impl From<tokio::task::JoinError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: tokio::task::JoinError) -> Self {
        from_any_with_tag(value, ErrorTag::Tokio)
    }
}

impl From<tokio::sync::AcquireError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: tokio::sync::AcquireError) -> Self {
        from_any_with_tag(value, ErrorTag::Tokio)
    }
}

impl From<tokio::time::error::Elapsed> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: tokio::time::error::Elapsed) -> Self {
        from_any_with_tag(value, ErrorTag::Tokio)
    }
}

impl From<tokio::sync::oneshot::error::RecvError> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: tokio::sync::oneshot::error::RecvError) -> Self {
        from_any_with_tag(value, ErrorTag::Tokio)
    }
}
