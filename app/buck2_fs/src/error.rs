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

impl IoError {
    pub fn new(op: String, e: io::Error) -> Self {
        Self {
            op,
            e,
            is_eden: false,
        }
    }

    pub fn new_with_path<P: AsRef<AbsPath>>(op: &str, path: P, e: io::Error) -> Self {
        let path = P::as_ref(&path);
        #[cfg(fbcode_build)]
        let is_eden = path
            .parent()
            .and_then(|p| detect_eden::is_eden(p.to_path_buf()).ok())
            .unwrap_or(false);
        #[cfg(not(fbcode_build))]
        let is_eden = false;

        let op = format!("{}({})", op, path.display());
        Self { op, e, is_eden }
    }

    pub fn categorize_for_source_file(self) -> buck2_error::Error {
        if self.e.kind() == io::ErrorKind::NotFound {
            buck2_error::Error::from(self).tag([ErrorTag::Input]).into()
        } else {
            self.into()
        }
    }

    pub fn inner_error(self) -> io::Error {
        self.e
    }
}

fn io_error_tags(e: &io::Error, is_eden: bool) -> Vec<ErrorTag> {
    let mut tags = vec![ErrorTag::IoSystem];
    if is_eden {
        match e.kind() {
            io::ErrorKind::NotFound => tags.push(ErrorTag::IoEdenFileNotFound),
            // Eden timeouts are most likely caused by network issues.
            // TODO check network health to be sure.
            io::ErrorKind::TimedOut => tags.push(ErrorTag::Environment),
            _ => tags.push(ErrorTag::IoEden),
        }
    }
    tags
}

#[derive(buck2_error::Error, Debug)]
#[buck2(tags = io_error_tags(e, *is_eden))]
#[error("{op}")]
pub struct IoError {
    pub(crate) op: String,
    #[source]
    pub(crate) e: io::Error,
    pub(crate) is_eden: bool,
}
