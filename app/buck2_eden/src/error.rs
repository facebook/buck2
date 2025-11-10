/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_certs::validate::validate_certs;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_error::ErrorTag;
use dupe::Dupe;
use edenfs::BinaryHash;
use edenfs::EdenErrorType;
use edenfs::FileAttributeDataOrErrorV2;
use edenfs::FileAttributeDataV2;
use edenfs::PathString;
use edenfs::SourceControlType;
use edenfs_clients::errors::ListMountsError;
use fbthrift::ApplicationException;
use fbthrift::ApplicationExceptionErrorCode;
use sorted_vector_map::SortedVectorMap;

#[derive(buck2_error::Error, Debug)]
#[buck2(tag = IoEdenMountNotReady)]
#[error("Mount never became ready: `{mount}`")]
pub struct MountNeverBecameReady {
    pub mount: AbsPathBuf,
}

#[derive(buck2_error::Error, Debug)]
pub enum IsMountReadyError {
    #[error("Mount does not exist in Eden: `{mount}`")]
    #[buck2(tag = IoEdenMountDoesNotExist)]
    MountDoesNotExist { mount: AbsPathBuf },
    #[error(transparent)]
    #[buck2(tag = IoEdenRequestError)]
    RequestError(ListMountsError),
}

#[derive(buck2_error::Error, Debug)]
pub enum ConnectAndRequestError<E> {
    #[error(transparent)]
    #[buck2(tag = IoEdenConnectionError)]
    ConnectionError(buck2_error::Error),
    #[error("Eden Request Failed: {0:?}")]
    #[buck2(tag = IoEdenRequestError)]
    RequestError(E),
}

fn is_app_exception_from_hanging(e: &ApplicationException) -> bool {
    // Timeout indicates that a single Thrift request took too long. Loadshedding indicates "Queue
    // Timeout" errors which are typically caused by heavy load on the Thrift server.
    e.type_ == ApplicationExceptionErrorCode::Timeout
        || e.type_ == ApplicationExceptionErrorCode::Loadshedding
}

pub trait ErrorFromHangingMount {
    fn is_caused_by_hanging_mount(&self) -> bool;
}

impl<E: ErrorFromHangingMount> ErrorFromHangingMount for ConnectAndRequestError<E> {
    fn is_caused_by_hanging_mount(&self) -> bool {
        match self {
            // NOTE: This is slightly incorrect, but the only user of this trait is the EdenFS
            // health check. The health check short circuits on ConnectionErrors, and therefore
            // this codepath will never be hit. This code can be fixed using structured logging to
            // return a new flavor of ConnectionError for hangs during client creation.
            Self::ConnectionError(_) => false,
            Self::RequestError(e) => e.is_caused_by_hanging_mount(),
        }
    }
}

macro_rules! impl_error_from_hanging_mount {
    ($err: ident) => {
        impl ErrorFromHangingMount for ::edenfs_clients::errors::$err {
            fn is_caused_by_hanging_mount(&self) -> bool {
                match self {
                    // For now, we assume all Thrift Errors are not caused by a hanging mount.
                    Self::ThriftError(..) => false,
                    Self::ApplicationException(e) if is_app_exception_from_hanging(e) => true,
                    _ => false,
                }
            }
        }
    };
}

impl_error_from_hanging_mount!(GetDaemonInfoError);

impl<E> std::error::Error for ConnectAndRequestError<E>
where
    E: std::error::Error,
    Self: std::fmt::Debug + std::fmt::Display + std::marker::Send + std::marker::Sync + 'static,
{
    fn source(&self) -> std::option::Option<&(dyn std::error::Error + 'static)> {
        use buck2_error::__for_macro::AsDynError;
        match self {
            ConnectAndRequestError::ConnectionError { 0: transparent } => {
                std::error::Error::source(transparent.as_dyn_error())
            }
            ConnectAndRequestError::RequestError { 0: transparent } => {
                std::error::Error::source(transparent.as_dyn_error())
            }
        }
    }
}

#[derive(Copy, Clone, Dupe, PartialEq, Eq)]
pub enum ErrorHandlingStrategy {
    Reconnect,
    Retry,
    Abort,
}

pub trait HasErrorHandlingStrategy {
    fn error_handling_strategy(&self) -> ErrorHandlingStrategy;
}

impl<E: HasErrorHandlingStrategy> HasErrorHandlingStrategy for ConnectAndRequestError<E> {
    fn error_handling_strategy(&self) -> ErrorHandlingStrategy {
        match self {
            Self::ConnectionError(..) => ErrorHandlingStrategy::Reconnect,
            Self::RequestError(e) => e.error_handling_strategy(),
        }
    }
}

impl HasErrorHandlingStrategy for IsMountReadyError {
    fn error_handling_strategy(&self) -> ErrorHandlingStrategy {
        match self {
            Self::MountDoesNotExist { .. } => ErrorHandlingStrategy::Abort,
            Self::RequestError(e) => e.error_handling_strategy(),
        }
    }
}

macro_rules! impl_has_error_handling_strategy {
    ($err: ident) => {
        impl HasErrorHandlingStrategy for ::edenfs_clients::errors::$err {
            fn error_handling_strategy(&self) -> ErrorHandlingStrategy {
                match self {
                    Self::ThriftError(..) => ErrorHandlingStrategy::Reconnect,
                    Self::ApplicationException(..) => ErrorHandlingStrategy::Retry,
                    Self::ex(..) => ErrorHandlingStrategy::Abort,
                }
            }
        }
    };
}

impl_has_error_handling_strategy!(GetAttributesFromFilesV2Error);
impl_has_error_handling_strategy!(GlobFilesError);
impl_has_error_handling_strategy!(ListMountsError);
impl_has_error_handling_strategy!(SynchronizeWorkingCopyError);
impl_has_error_handling_strategy!(SetPathObjectIdError);
impl_has_error_handling_strategy!(RemoveRecursivelyError);
impl_has_error_handling_strategy!(EnsureMaterializedError);
impl_has_error_handling_strategy!(ReaddirError);
impl_has_error_handling_strategy!(GetFileContentError);
impl_has_error_handling_strategy!(GetSHA1Error);
impl_has_error_handling_strategy!(GetCurrentJournalPositionError);
impl_has_error_handling_strategy!(ChangesSinceV2Error);
impl_has_error_handling_strategy!(GetDaemonInfoError);

fn eden_posix_error_tag(code: i32) -> ErrorTag {
    match code {
        libc::ENOENT => ErrorTag::IoEdenFileNotFound,
        libc::EACCES | libc::EPERM => ErrorTag::IoPermissionDenied,
        libc::ETIMEDOUT => ErrorTag::IoTimeout,
        libc::EBUSY => ErrorTag::IoExecutableFileBusy,
        libc::EPIPE => ErrorTag::IoBrokenPipe,
        libc::ENOSPC => ErrorTag::IoStorageFull,
        libc::ECONNABORTED => ErrorTag::IoConnectionAborted,
        libc::ENOTCONN => ErrorTag::IoNotConnected,
        libc::EBADMSG => ErrorTag::IoEdenDataCorruption,
        _ => ErrorTag::IoEdenUncategorized,
    }
}

fn eden_network_error_tag(code_opt: Option<i32>) -> ErrorTag {
    const CURLE_OPERATION_TIMEDOUT: i32 = curl_sys::CURLE_OPERATION_TIMEDOUT as i32;
    const CURLE_RECV_ERROR: i32 = curl_sys::CURLE_RECV_ERROR as i32;
    const CURLE_SSL_CERTPROBLEM: i32 = curl_sys::CURLE_SSL_CERTPROBLEM as i32;

    match code_opt {
        Some(code) => {
            match code {
                CURLE_OPERATION_TIMEDOUT => ErrorTag::IoEdenNetworkCurlTimedout, // 28
                CURLE_RECV_ERROR | CURLE_SSL_CERTPROBLEM => ErrorTag::IoEdenNetworkTls, // 56 and 58
                401 => ErrorTag::HttpUnauthorized, // http::StatusCode::UNAUTHORIZED
                403 => ErrorTag::HttpForbidden,    // http::StatusCode::FORBIDDEN
                503 => ErrorTag::HttpServiceUnavailable, // http::StatusCode::SERVICE_UNAVAILABLE
                _ => ErrorTag::IoEdenNetworkUncategorized,
            }
        }
        None => ErrorTag::IoEdenNetworkUncategorized,
    }
}

fn eden_service_error_tag(error: &edenfs::EdenError) -> ErrorTag {
    match error.errorType {
        EdenErrorType::WIN32_ERROR => ErrorTag::IoEdenWin32Error,
        EdenErrorType::HRESULT_ERROR => ErrorTag::IoEdenHresultError,
        EdenErrorType::ARGUMENT_ERROR => ErrorTag::IoEdenArgumentError,
        EdenErrorType::GENERIC_ERROR => ErrorTag::IoEdenGenericError,
        EdenErrorType::MOUNT_GENERATION_CHANGED => ErrorTag::IoEdenMountGenerationChanged,
        EdenErrorType::JOURNAL_TRUNCATED => ErrorTag::IoEdenJournalTruncated,
        EdenErrorType::CHECKOUT_IN_PROGRESS => ErrorTag::IoEdenCheckoutInProgress,
        EdenErrorType::OUT_OF_DATE_PARENT => ErrorTag::IoEdenOutOfDateParent,
        EdenErrorType::ATTRIBUTE_UNAVAILABLE => ErrorTag::IoEdenAttributeUnavailable,
        _ => ErrorTag::IoEdenUncategorized,
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = IoEden)]
pub enum EdenError {
    #[error("Eden POSIX error (code = {code}): {0}", error.message)]
    #[buck2(tag = eden_posix_error_tag(code))]
    PosixError { error: edenfs::EdenError, code: i32 },

    #[error("Eden network error (code = {code:?}): {0}", error.message)]
    #[buck2(tag = eden_network_error_tag(code))]
    NetworkError {
        error: edenfs::EdenError,
        code: Option<i32>,
    },

    #[error("Eden service error: {0}", error.message)]
    #[buck2(tag = eden_service_error_tag(&error))]
    ServiceError { error: edenfs::EdenError },

    #[error("Eden returned an unexpected field: {field}")]
    #[buck2(tag = IoEdenUnknownField)]
    UnknownField { field: i32 },
}

impl From<edenfs::EdenError> for EdenError {
    fn from(error: edenfs::EdenError) -> Self {
        if error.errorType == EdenErrorType::POSIX_ERROR {
            if let Some(error_code) = error.errorCode {
                return Self::PosixError {
                    error,
                    code: error_code,
                };
            }
        } else if error.errorType == EdenErrorType::NETWORK_ERROR {
            let code_opt = error.errorCode;
            return Self::NetworkError {
                error,
                code: code_opt,
            };
        } else if error.errorType == EdenErrorType::GENERIC_ERROR {
            // TODO(minglunli): Hacky solution to check if Eden errors are cert related
            if let Err(e) = futures::executor::block_on(validate_certs()) {
                let eden_err = edenfs::EdenError {
                    message: format!("{e}"),
                    ..error
                };

                return Self::ServiceError { error: eden_err };
            }
        }

        Self::ServiceError { error }
    }
}

pub trait EdenDataIntoResult {
    type Data;

    fn into_result(self) -> Result<Self::Data, EdenError>;
}

macro_rules! impl_eden_data_into_result {
    ($typ: ident, $data: ty, $ok_variant: ident) => {
        impl EdenDataIntoResult for ::edenfs::$typ {
            type Data = $data;

            fn into_result(self) -> Result<Self::Data, EdenError> {
                match self {
                    Self::$ok_variant(data) => Ok(data),
                    Self::error(e) => Err(e.into()),
                    Self::UnknownField(field) => Err(EdenError::UnknownField { field }),
                }
            }
        }
    };
}

impl_eden_data_into_result!(
    SourceControlTypeOrError,
    SourceControlType,
    sourceControlType
);

impl_eden_data_into_result!(
    FileAttributeDataOrErrorV2,
    FileAttributeDataV2,
    fileAttributeData
);

impl_eden_data_into_result!(SizeOrError, i64, size);
    SortedVectorMap<PathString, FileAttributeDataOrErrorV2>,
    dirListAttributeData
);

#[cfg(test)]
mod tests {
    use super::*;

    // Helper function to create a basic EdenError for testing
    fn create_eden_error(error_type: EdenErrorType, error_code: Option<i32>) -> edenfs::EdenError {
        edenfs::EdenError {
            message: "Test error message".to_string(),
            errorType: error_type,
            errorCode: error_code,
            ..Default::default()
        }
    }

    #[test]
    fn test_eden_network_error_tag_with_curl_timeout() {
        const CURLE_OPERATION_TIMEDOUT: i32 = curl_sys::CURLE_OPERATION_TIMEDOUT as i32;
        let tag = eden_network_error_tag(Some(CURLE_OPERATION_TIMEDOUT));
        assert_eq!(tag, ErrorTag::IoEdenNetworkCurlTimedout);
    }

    #[test]
    fn test_eden_network_error_tag_with_curl_recv_error() {
        const CURLE_RECV_ERROR: i32 = curl_sys::CURLE_RECV_ERROR as i32;
        let tag = eden_network_error_tag(Some(CURLE_RECV_ERROR));
        assert_eq!(tag, ErrorTag::IoEdenNetworkTls);
    }

    #[test]
    fn test_eden_network_error_tag_with_curl_ssl_cert_problem() {
        const CURLE_SSL_CERTPROBLEM: i32 = curl_sys::CURLE_SSL_CERTPROBLEM as i32;
        let tag = eden_network_error_tag(Some(CURLE_SSL_CERTPROBLEM));
        assert_eq!(tag, ErrorTag::IoEdenNetworkTls);
    }

    #[test]
    fn test_eden_network_error_tag_with_http_401() {
        let tag = eden_network_error_tag(Some(401));
        assert_eq!(tag, ErrorTag::HttpUnauthorized);
    }

    #[test]
    fn test_eden_network_error_tag_with_http_403() {
        let tag = eden_network_error_tag(Some(403));
        assert_eq!(tag, ErrorTag::HttpForbidden);
    }

    #[test]
    fn test_eden_network_error_tag_with_http_503() {
        let tag = eden_network_error_tag(Some(503));
        assert_eq!(tag, ErrorTag::HttpServiceUnavailable);
    }

    #[test]
    fn test_eden_network_error_tag_with_unknown_code() {
        let tag = eden_network_error_tag(Some(999));
        assert_eq!(tag, ErrorTag::IoEdenNetworkUncategorized);
    }

    #[test]
    fn test_eden_network_error_tag_with_none() {
        // This is the key test for the new functionality - handling None case
        let tag = eden_network_error_tag(None);
        assert_eq!(tag, ErrorTag::IoEdenNetworkUncategorized);
    }

    #[test]
    fn test_eden_network_error_tag_with_zero_code() {
        let tag = eden_network_error_tag(Some(0));
        assert_eq!(tag, ErrorTag::IoEdenNetworkUncategorized);
    }

    #[test]
    fn test_eden_network_error_tag_with_negative_code() {
        let tag = eden_network_error_tag(Some(-1));
        assert_eq!(tag, ErrorTag::IoEdenNetworkUncategorized);
    }

    #[test]
    fn test_eden_posix_error_tag_enoent() {
        let tag = eden_posix_error_tag(libc::ENOENT);
        assert_eq!(tag, ErrorTag::IoEdenFileNotFound);
    }

    #[test]
    fn test_eden_posix_error_tag_eacces() {
        let tag = eden_posix_error_tag(libc::EACCES);
        assert_eq!(tag, ErrorTag::IoPermissionDenied);
    }

    #[test]
    fn test_eden_posix_error_tag_eperm() {
        let tag = eden_posix_error_tag(libc::EPERM);
        assert_eq!(tag, ErrorTag::IoPermissionDenied);
    }

    #[test]
    fn test_eden_posix_error_tag_etimedout() {
        let tag = eden_posix_error_tag(libc::ETIMEDOUT);
        assert_eq!(tag, ErrorTag::IoTimeout);
    }

    #[test]
    fn test_eden_posix_error_tag_ebusy() {
        let tag = eden_posix_error_tag(libc::EBUSY);
        assert_eq!(tag, ErrorTag::IoExecutableFileBusy);
    }

    #[test]
    fn test_eden_posix_error_tag_epipe() {
        let tag = eden_posix_error_tag(libc::EPIPE);
        assert_eq!(tag, ErrorTag::IoBrokenPipe);
    }

    #[test]
    fn test_eden_posix_error_tag_enospc() {
        let tag = eden_posix_error_tag(libc::ENOSPC);
        assert_eq!(tag, ErrorTag::IoStorageFull);
    }

    #[test]
    fn test_eden_posix_error_tag_econnaborted() {
        let tag = eden_posix_error_tag(libc::ECONNABORTED);
        assert_eq!(tag, ErrorTag::IoConnectionAborted);
    }

    #[test]
    fn test_eden_posix_error_tag_enotconn() {
        let tag = eden_posix_error_tag(libc::ENOTCONN);
        assert_eq!(tag, ErrorTag::IoNotConnected);
    }

    #[test]
    fn test_eden_posix_error_tag_ebadmsg() {
        let tag = eden_posix_error_tag(libc::EBADMSG);
        assert_eq!(tag, ErrorTag::IoEdenDataCorruption);
    }

    #[test]
    fn test_eden_posix_error_tag_unknown() {
        let tag = eden_posix_error_tag(9999);
        assert_eq!(tag, ErrorTag::IoEdenUncategorized);
    }

    #[test]
    fn test_eden_service_error_tag_win32_error() {
        let error = create_eden_error(EdenErrorType::WIN32_ERROR, None);
        let tag = eden_service_error_tag(&error);
        assert_eq!(tag, ErrorTag::IoEdenWin32Error);
    }

    #[test]
    fn test_eden_service_error_tag_hresult_error() {
        let error = create_eden_error(EdenErrorType::HRESULT_ERROR, None);
        let tag = eden_service_error_tag(&error);
        assert_eq!(tag, ErrorTag::IoEdenHresultError);
    }

    #[test]
    fn test_eden_service_error_tag_argument_error() {
        let error = create_eden_error(EdenErrorType::ARGUMENT_ERROR, None);
        let tag = eden_service_error_tag(&error);
        assert_eq!(tag, ErrorTag::IoEdenArgumentError);
    }

    #[test]
    fn test_eden_service_error_tag_generic_error() {
        let error = create_eden_error(EdenErrorType::GENERIC_ERROR, None);
        let tag = eden_service_error_tag(&error);
        assert_eq!(tag, ErrorTag::IoEdenGenericError);
    }

    #[test]
    fn test_eden_service_error_tag_mount_generation_changed() {
        let error = create_eden_error(EdenErrorType::MOUNT_GENERATION_CHANGED, None);
        let tag = eden_service_error_tag(&error);
        assert_eq!(tag, ErrorTag::IoEdenMountGenerationChanged);
    }

    #[test]
    fn test_eden_service_error_tag_journal_truncated() {
        let error = create_eden_error(EdenErrorType::JOURNAL_TRUNCATED, None);
        let tag = eden_service_error_tag(&error);
        assert_eq!(tag, ErrorTag::IoEdenJournalTruncated);
    }

    #[test]
    fn test_eden_service_error_tag_checkout_in_progress() {
        let error = create_eden_error(EdenErrorType::CHECKOUT_IN_PROGRESS, None);
        let tag = eden_service_error_tag(&error);
        assert_eq!(tag, ErrorTag::IoEdenCheckoutInProgress);
    }

    #[test]
    fn test_eden_service_error_tag_out_of_date_parent() {
        let error = create_eden_error(EdenErrorType::OUT_OF_DATE_PARENT, None);
        let tag = eden_service_error_tag(&error);
        assert_eq!(tag, ErrorTag::IoEdenOutOfDateParent);
    }

    #[test]
    fn test_eden_service_error_tag_attribute_unavailable() {
        let error = create_eden_error(EdenErrorType::ATTRIBUTE_UNAVAILABLE, None);
        let tag = eden_service_error_tag(&error);
        assert_eq!(tag, ErrorTag::IoEdenAttributeUnavailable);
    }

    #[test]
    fn test_eden_error_from_posix_error_with_code() {
        let eden_err = create_eden_error(EdenErrorType::POSIX_ERROR, Some(libc::ENOENT));
        let error: EdenError = eden_err.into();
        
        match error {
            EdenError::PosixError { code, .. } => {
                assert_eq!(code, libc::ENOENT);
            }
            _ => panic!("Expected PosixError variant"),
        }
    }

    #[test]
    fn test_eden_error_from_posix_error_without_code() {
        let eden_err = create_eden_error(EdenErrorType::POSIX_ERROR, None);
        let error: EdenError = eden_err.into();
        
        // Without error code, should fall through to ServiceError
        match error {
            EdenError::ServiceError { .. } => {
                // Expected behavior
            }
            _ => panic!("Expected ServiceError variant when POSIX_ERROR has no code"),
        }
    }

    #[test]
    fn test_eden_error_from_network_error_with_code() {
        let eden_err = create_eden_error(EdenErrorType::NETWORK_ERROR, Some(401));
        let error: EdenError = eden_err.into();
        
        match error {
            EdenError::NetworkError { code, .. } => {
                assert_eq!(code, Some(401));
            }
            _ => panic!("Expected NetworkError variant"),
        }
    }

    #[test]
    fn test_eden_error_from_network_error_without_code() {
        // This is the key test for the new functionality
        let eden_err = create_eden_error(EdenErrorType::NETWORK_ERROR, None);
        let error: EdenError = eden_err.into();
        
        match error {
            EdenError::NetworkError { code, .. } => {
                assert_eq!(code, None);
            }
            _ => panic!("Expected NetworkError variant"),
        }
    }

    #[test]
    fn test_eden_error_from_network_error_with_timeout_code() {
        const CURLE_OPERATION_TIMEDOUT: i32 = curl_sys::CURLE_OPERATION_TIMEDOUT as i32;
        let eden_err = create_eden_error(EdenErrorType::NETWORK_ERROR, Some(CURLE_OPERATION_TIMEDOUT));
        let error: EdenError = eden_err.into();
        
        match error {
            EdenError::NetworkError { code, .. } => {
                assert_eq!(code, Some(CURLE_OPERATION_TIMEDOUT));
            }
            _ => panic!("Expected NetworkError variant"),
        }
    }

    #[test]
    fn test_eden_error_from_network_error_with_zero_code() {
        let eden_err = create_eden_error(EdenErrorType::NETWORK_ERROR, Some(0));
        let error: EdenError = eden_err.into();
        
        match error {
            EdenError::NetworkError { code, .. } => {
                assert_eq!(code, Some(0));
            }
            _ => panic!("Expected NetworkError variant"),
        }
    }

    #[test]
    fn test_eden_error_display_network_error_with_code() {
        let eden_err = create_eden_error(EdenErrorType::NETWORK_ERROR, Some(401));
        let error: EdenError = eden_err.into();
        let display_str = format!("{}", error);
        
        // Should include "code = Some(401)" in the display output
        assert!(display_str.contains("401"));
        assert!(display_str.contains("Test error message"));
    }

    #[test]
    fn test_eden_error_display_network_error_without_code() {
        let eden_err = create_eden_error(EdenErrorType::NETWORK_ERROR, None);
        let error: EdenError = eden_err.into();
        let display_str = format!("{}", error);
        
        // Should include "code = None" in the display output
        assert!(display_str.contains("None"));
        assert!(display_str.contains("Test error message"));
    }

    #[test]
    fn test_eden_error_from_generic_error() {
        let eden_err = create_eden_error(EdenErrorType::GENERIC_ERROR, None);
        let error: EdenError = eden_err.into();
        
        match error {
            EdenError::ServiceError { .. } => {
                // Expected behavior
            }
            _ => panic!("Expected ServiceError variant for GENERIC_ERROR"),
        }
    }

    #[test]
    fn test_eden_error_debug_format() {
        let eden_err = create_eden_error(EdenErrorType::NETWORK_ERROR, Some(503));
        let error: EdenError = eden_err.into();
        let debug_str = format!("{:?}", error);
        
        // Debug format should contain the variant name
        assert!(debug_str.contains("NetworkError"));
    }

    #[test]
    fn test_network_error_with_various_http_codes() {
        let test_cases = vec![
            (Some(200), ErrorTag::IoEdenNetworkUncategorized),
            (Some(401), ErrorTag::HttpUnauthorized),
            (Some(403), ErrorTag::HttpForbidden),
            (Some(404), ErrorTag::IoEdenNetworkUncategorized),
            (Some(500), ErrorTag::IoEdenNetworkUncategorized),
            (Some(503), ErrorTag::HttpServiceUnavailable),
        ];

        for (code_opt, expected_tag) in test_cases {
            let tag = eden_network_error_tag(code_opt);
            assert_eq!(
                tag, expected_tag,
                "Failed for code {:?}: expected {:?}, got {:?}",
                code_opt, expected_tag, tag
            );
        }
    }

    #[test]
    fn test_network_error_roundtrip_with_all_curl_codes() {
        const CURLE_OPERATION_TIMEDOUT: i32 = curl_sys::CURLE_OPERATION_TIMEDOUT as i32;
        const CURLE_RECV_ERROR: i32 = curl_sys::CURLE_RECV_ERROR as i32;
        const CURLE_SSL_CERTPROBLEM: i32 = curl_sys::CURLE_SSL_CERTPROBLEM as i32;

        let test_codes = vec![
            CURLE_OPERATION_TIMEDOUT,
            CURLE_RECV_ERROR,
            CURLE_SSL_CERTPROBLEM,
        ];

        for code in test_codes {
            let eden_err = create_eden_error(EdenErrorType::NETWORK_ERROR, Some(code));
            let error: EdenError = eden_err.into();
            
            match error {
                EdenError::NetworkError { code: result_code, .. } => {
                    assert_eq!(result_code, Some(code));
                }
                _ => panic!("Expected NetworkError variant for curl code {}", code),
            }
        }
    }

    #[test]
    fn test_eden_error_boundary_conditions() {
        // Test with i32::MAX
        let tag = eden_network_error_tag(Some(i32::MAX));
        assert_eq!(tag, ErrorTag::IoEdenNetworkUncategorized);

        // Test with i32::MIN
        let tag = eden_network_error_tag(Some(i32::MIN));
        assert_eq!(tag, ErrorTag::IoEdenNetworkUncategorized);
    }

    #[test]
    fn test_multiple_error_type_conversions() {
        // Test that different error types are correctly converted
        let error_types = vec![
            (EdenErrorType::POSIX_ERROR, Some(libc::ENOENT)),
            (EdenErrorType::NETWORK_ERROR, Some(401)),
            (EdenErrorType::NETWORK_ERROR, None),
            (EdenErrorType::GENERIC_ERROR, None),
        ];

        for (error_type, error_code) in error_types {
            let eden_err = create_eden_error(error_type, error_code);
            let _error: EdenError = eden_err.into();
            // Just ensure conversion doesn't panic
        }
    }

    #[test]
    fn test_posix_error_preserves_code() {
        let test_codes = vec![
            libc::ENOENT,
            libc::EACCES,
            libc::EPERM,
            libc::ETIMEDOUT,
            libc::EBUSY,
        ];

        for code in test_codes {
            let eden_err = create_eden_error(EdenErrorType::POSIX_ERROR, Some(code));
            let error: EdenError = eden_err.into();
            
            match error {
                EdenError::PosixError { code: result_code, .. } => {
                    assert_eq!(result_code, code, "Code should be preserved for {}", code);
                }
                _ => panic!("Expected PosixError variant"),
            }
        }
    }
}
impl_eden_data_into_result!(Blake3OrError, BinaryHash, blake3);

impl_eden_data_into_result!(
    DirListAttributeDataOrError,
    SortedVectorMap<PathString, FileAttributeDataOrErrorV2>,
    dirListAttributeData
);