/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;

use buck2_data::error::ErrorTag;

// https://learn.microsoft.com/en-us/windows/win32/debug/system-error-codes--0-499-
// "The process cannot access the file because it is being used by another process."
const ERROR_SHARING_VIOLATION: i32 = 32;

pub(crate) fn io_error_kind_to_error_tag(kind: io::ErrorKind) -> ErrorTag {
    match kind {
        io::ErrorKind::NotFound => ErrorTag::IoNotFound,
        io::ErrorKind::PermissionDenied => ErrorTag::IoPermissionDenied,
        io::ErrorKind::TimedOut => ErrorTag::IoTimeout,
        io::ErrorKind::ExecutableFileBusy => ErrorTag::IoExecutableFileBusy,
        io::ErrorKind::BrokenPipe => ErrorTag::IoBrokenPipe,
        io::ErrorKind::StorageFull => ErrorTag::IoStorageFull,
        io::ErrorKind::ConnectionAborted => ErrorTag::IoConnectionAborted,
        io::ErrorKind::ReadOnlyFilesystem => ErrorTag::IoReadOnlyFilesystem,
        _ => ErrorTag::IoSystem,
    }
}

fn io_error_kind_tag(e: &io::Error) -> ErrorTag {
    let kind_tag = io_error_kind_to_error_tag(e.kind());
    if kind_tag != ErrorTag::IoSystem {
        return kind_tag;
    }

    if let Some(os_error_code) = e.raw_os_error() {
        'from_os: {
            let from_os = match os_error_code {
                libc::ENOTCONN => ErrorTag::IoNotConnected,
                libc::ECONNABORTED => ErrorTag::IoConnectionAborted,
                _ => break 'from_os,
            };
            return from_os;
        }

        if cfg!(windows) && os_error_code == ERROR_SHARING_VIOLATION {
            return ErrorTag::IoWindowsSharingViolation;
        }
    }

    ErrorTag::IoSystem
}

impl From<io::Error> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: std::io::Error) -> Self {
        let error_tag = io_error_kind_tag(&value);
        crate::conversion::from_any_with_tag(value, error_tag)
    }
}
