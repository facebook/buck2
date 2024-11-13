/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use crate::invocation_paths::InvocationPaths;

#[derive(Clone)]
pub enum InvocationPathsResult {
    OtherError(buck2_error::Error),
    Paths(InvocationPaths),
    OutsideOfRepo(buck2_error::Error), // this error ignored for creating invocation record for log commands
}

impl InvocationPathsResult {
    pub fn get_result(self) -> buck2_error::Result<InvocationPaths> {
        match self {
            InvocationPathsResult::OtherError(e) => Err(e.into()),
            InvocationPathsResult::Paths(paths) => Ok(paths),
            InvocationPathsResult::OutsideOfRepo(e) => Err(e.into()),
        }
    }
}
