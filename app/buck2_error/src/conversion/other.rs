/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::Arc;

use crate::any::CrateAsStdError;

impl From<crate::Error> for anyhow::Error {
    #[cold]
    #[track_caller]
    fn from(value: crate::Error) -> Self {
        Into::into(CrateAsStdError(value))
    }
}

impl From<Arc<crate::Error>> for crate::Error {
    #[cold]
    #[track_caller]
    fn from(value: Arc<crate::Error>) -> Self {
        (*value).clone()
    }
}
