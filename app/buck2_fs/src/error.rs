/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Error types for filesystem operations.

use std::io;

use buck2_error::ErrorTag;

use crate::paths::abs_path::AbsPath;

/// The inner error type that IoError can hold.
#[derive(Debug)]
pub enum IoErrorSource {
    Io(io::Error),
    Internal(buck2_error::Error),
}

impl From<io::Error> for IoErrorSource {
    fn from(e: io::Error) -> Self {
        IoErrorSource::Io(e)
    }
}

impl IoError {
    /// Create an error from an io::Error or buck2_error::Error.
    pub fn new<T: Into<IoErrorSource>>(e: T) -> Self {
        Self {
            source: e.into(),
            context: Vec::new(),
            tags: Vec::new(),
            is_eden: false,
        }
    }

    pub fn internal(e: buck2_error::Error) -> Self {
        Self {
            source: IoErrorSource::Internal(e),
            context: Vec::new(),
            tags: Vec::new(),
            is_eden: false,
        }
    }

    /// Set operation and is_eden based on provided path.
    pub fn new_with_path<T: Into<IoErrorSource>, P: AsRef<AbsPath>>(
        op: &str,
        path: P,
        e: T,
    ) -> Self {
        let path = path.as_ref();
        IoError::new(e)
            .context(format!("{}({})", op, path.display()))
            .check_eden(path)
    }

    pub fn context(mut self, op: impl Into<String>) -> Self {
        self.context.push(op.into());
        self
    }

    pub fn tag(mut self, tag: ErrorTag) -> Self {
        self.tags.push(tag);
        self
    }

    /// Set the is_eden flag if provided path is on an eden fs
    #[cfg(fbcode_build)]
    pub fn check_eden(mut self, path: &AbsPath) -> Self {
        self.is_eden |= path
            .parent()
            .and_then(|p| detect_eden::is_eden(p.to_path_buf()).ok())
            .unwrap_or(false);
        self
    }

    #[cfg(not(fbcode_build))]
    pub fn check_eden(self, _path: &AbsPath) -> Self {
        self
    }

    fn convert_to_buck2_error(self) -> buck2_error::Error {
        let mut tags = vec![ErrorTag::IoSystem];
        if self.is_eden
            && let IoErrorSource::Io(e) = &self.source
        {
            match e.kind() {
                io::ErrorKind::NotFound => tags.push(ErrorTag::IoEdenFileNotFound),
                // Eden timeouts are most likely caused by network issues.
                // TODO check network health to be sure.
                io::ErrorKind::TimedOut => tags.push(ErrorTag::Environment),
                _ => tags.push(ErrorTag::IoEden),
            }
        }
        tags.extend(self.tags);
        let context = self.context.clone();
        let source_error: buck2_error::Error = match self.source {
            IoErrorSource::Io(e) => e.into(),
            IoErrorSource::Internal(e) => e,
        };
        let mut result = source_error.tag(tags);
        for ctx in context.into_iter().rev() {
            result = result.context(ctx);
        }
        result
    }

    pub fn categorize_for_source_file(self) -> buck2_error::Error {
        let is_not_found = match &self.source {
            IoErrorSource::Io(e) => e.kind() == io::ErrorKind::NotFound,
            IoErrorSource::Internal(_) => false,
        };
        if is_not_found {
            buck2_error::Error::from(self).tag([ErrorTag::Input]).into()
        } else {
            self.into()
        }
    }

    pub fn inner_error(self) -> IoErrorSource {
        self.source
    }

    pub fn io_error_kind(&self) -> Option<io::ErrorKind> {
        match &self.source {
            IoErrorSource::Io(e) => Some(e.kind()),
            IoErrorSource::Internal(_) => None,
        }
    }
}

#[derive(Debug)]
pub struct IoError {
    pub(crate) source: IoErrorSource,
    pub(crate) context: Vec<String>,
    pub(crate) tags: Vec<ErrorTag>,
    pub(crate) is_eden: bool,
}

// TODO remove this
impl From<IoError> for buck2_error::Error {
    fn from(e: IoError) -> Self {
        e.convert_to_buck2_error()
    }
}
