/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_error::ErrorTag;
use buck2_error::TypedContext;
use remote_execution::REClientError;
use remote_execution::TCode;
use remote_execution::TCodeReasonGroup;

pub fn get_re_error_tag(tcode: &TCode) -> ErrorTag {
    match *tcode {
        TCode::CANCELLED => ErrorTag::ReCancelled,
        TCode::UNKNOWN => ErrorTag::ReUnknown,
        TCode::INVALID_ARGUMENT => ErrorTag::ReInvalidArgument,
        TCode::DEADLINE_EXCEEDED => ErrorTag::ReDeadlineExceeded,
        TCode::NOT_FOUND => ErrorTag::ReNotFound,
        TCode::ALREADY_EXISTS => ErrorTag::ReAlreadyExists,
        TCode::PERMISSION_DENIED => ErrorTag::RePermissionDenied,
        TCode::RESOURCE_EXHAUSTED => ErrorTag::ReResourceExhausted,
        TCode::FAILED_PRECONDITION => ErrorTag::ReFailedPrecondition,
        TCode::ABORTED => ErrorTag::ReAborted,
        TCode::OUT_OF_RANGE => ErrorTag::ReOutOfRange,
        TCode::UNIMPLEMENTED => ErrorTag::ReUnimplemented,
        TCode::INTERNAL => ErrorTag::ReInternal,
        TCode::UNAVAILABLE => ErrorTag::ReUnavailable,
        TCode::DATA_LOSS => ErrorTag::ReDataLoss,
        TCode::UNAUTHENTICATED => ErrorTag::ReUnauthenticated,
        _ => ErrorTag::ReUnknownTcode,
    }
}

#[derive(Allocative, Debug, Clone, buck2_error::Error)]
#[error("Remote Execution Error on {} for ReSession {}\nError: ({})", .re_action, .re_session_id, .message)]
#[buck2(tag = get_re_error_tag(code))]
pub struct RemoteExecutionError {
    re_action: String,
    re_session_id: String,
    pub message: String,
    #[allocative(skip)]
    pub code: TCode,
    #[allocative(skip)]
    pub group: TCodeReasonGroup,
}

impl TypedContext for RemoteExecutionError {
    fn eq(&self, other: &dyn TypedContext) -> bool {
        match (other as &dyn std::any::Any).downcast_ref::<Self>() {
            Some(right) => self.eq(right),
            None => false,
        }
    }

    fn should_display(&self) -> bool {
        false
    }
}

fn re_error(
    re_action: &str,
    re_session_id: &str,
    message: String,
    code: TCode,
    group: TCodeReasonGroup,
) -> buck2_error::Error {
    let err = RemoteExecutionError {
        re_action: re_action.to_owned(),
        re_session_id: re_session_id.to_owned(),
        message,
        code,
        group,
    };
    let buck2_error: buck2_error::Error = err.clone().into();

    buck2_error.context(err).context_for_key(&group.to_string())
}

pub(crate) async fn with_error_handler<T>(
    re_action: &str,
    re_session_id: &str,
    result: anyhow::Result<T>,
) -> buck2_error::Result<T> {
    match result {
        Ok(val) => Ok(val),
        Err(e) => {
            let (code, group) = e
                .downcast_ref::<REClientError>()
                .map(|e| (e.code, e.group))
                .unwrap_or((TCode::UNKNOWN, TCodeReasonGroup::UNKNOWN));

            Err(re_error(re_action, re_session_id, format!("{:#}", e), code, group).into())
        }
    }
}

pub fn test_re_error(message: &str, code: TCode) -> buck2_error::Error {
    re_error(
        "test",
        "test",
        message.to_owned(),
        code,
        TCodeReasonGroup::UNKNOWN,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_re_error() {
        let error: buck2_error::Error = re_error(
            "test",
            "test",
            "test".to_owned(),
            TCode::UNKNOWN,
            TCodeReasonGroup::UNKNOWN,
        );

        let err = error.find_typed_context::<RemoteExecutionError>().unwrap();
        assert_eq!(err.code, TCode::UNKNOWN);
    }
}
