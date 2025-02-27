/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_certs::validate::validate_certs;
use buck2_core::fs::paths::abs_path::AbsPathBuf;
use buck2_error::ErrorTag;
use dupe::Dupe;
use edenfs::BinaryHash;
use edenfs::EdenErrorType;
use edenfs::FileAttributeData;
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

impl_has_error_handling_strategy!(GetAttributesFromFilesError);
impl_has_error_handling_strategy!(GetAttributesFromFilesV2Error);
impl_has_error_handling_strategy!(GlobFilesError);
impl_has_error_handling_strategy!(ListMountsError);
impl_has_error_handling_strategy!(SynchronizeWorkingCopyError);
impl_has_error_handling_strategy!(SetPathObjectIdError);
impl_has_error_handling_strategy!(RemoveRecursivelyError);
impl_has_error_handling_strategy!(EnsureMaterializedError);
impl_has_error_handling_strategy!(ReaddirError);
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
        _ => ErrorTag::IoEden,
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
        _ => ErrorTag::IoEden,
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = IoEden)]
pub enum EdenError {
    #[error("Eden POSIX error (code = {code}): {0}", error.message)]
    #[buck2(tag = eden_posix_error_tag(code))]
    PosixError { error: edenfs::EdenError, code: i32 },

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
        } else if error.errorType == EdenErrorType::GENERIC_ERROR {
            // TODO(minglunli): Hacky solution to check if Eden errors are cert related
            if let Err(e) = futures::executor::block_on(validate_certs()) {
                let eden_err = edenfs::EdenError {
                    message: format!("{}", e),
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

impl_eden_data_into_result!(FileAttributeDataOrError, FileAttributeData, data);

impl_eden_data_into_result!(
    FileAttributeDataOrErrorV2,
    FileAttributeDataV2,
    fileAttributeData
);

impl_eden_data_into_result!(SizeOrError, i64, size);

impl_eden_data_into_result!(Sha1OrError, BinaryHash, sha1);

impl_eden_data_into_result!(Blake3OrError, BinaryHash, blake3);

impl_eden_data_into_result!(
    DirListAttributeDataOrError,
    SortedVectorMap<PathString, FileAttributeDataOrErrorV2>,
    dirListAttributeData
);
