/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use dupe::Dupe;

use crate::TIME_TO_FIX_USE_BETTER_ERROR;

#[derive(Debug, buck2_error::Error)]
pub(crate) enum FileOpsError {
    #[error("File not found: {0}")]
    // File not found errors are not inherently always user errors; however, we only use these
    // methods with source files, and in that case this is correct
    #[buck2(input)]
    #[buck2(tag = IoNotFound)]
    FileNotFound(String),
}

pub enum FileReadError {
    NotFound(String),
    Buck(buck2_error::Error),
}

impl FileReadError {
    pub fn with_package_context_information(self, package_path: String) -> buck2_error::Error {
        match self {
            FileReadError::NotFound(path) => {
                let err_message = if *TIME_TO_FIX_USE_BETTER_ERROR.get().unwrap() {
                    format!(
                        "`{}`.\n     Included in `{}` but does not exist",
                        path, package_path
                    )
                } else {
                    path
                };

                FileOpsError::FileNotFound(err_message).into()
            }
            FileReadError::Buck(err) => err.dupe(),
        }
    }

    pub fn without_package_context_information(self) -> buck2_error::Error {
        match self {
            FileReadError::NotFound(path) => FileOpsError::FileNotFound(path).into(),
            FileReadError::Buck(err) => err.dupe(),
        }
    }
}

pub trait FileReadErrorContext<T> {
    fn with_package_context_information(self, package_path: String) -> buck2_error::Result<T>;
    fn without_package_context_information(self) -> buck2_error::Result<T>;
}

impl<T> FileReadErrorContext<T> for std::result::Result<T, FileReadError> {
    fn with_package_context_information(self, package_path: String) -> buck2_error::Result<T> {
        self.map_err(|e| e.with_package_context_information(package_path))
    }

    fn without_package_context_information(self) -> buck2_error::Result<T> {
        self.map_err(|e| e.without_package_context_information())
    }
}
