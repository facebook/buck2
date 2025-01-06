/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::soft_error;
use buck2_error::ErrorTag;
use starlark::eval::SoftErrorHandler;
use starlark::ErrorKind;
pub struct Buck2StarlarkSoftErrorHandler;

/// When starlark deprecates something, we propagate it to our `soft_error!` handler.
impl SoftErrorHandler for Buck2StarlarkSoftErrorHandler {
    fn soft_error(&self, category: &str, error: starlark::Error) -> Result<(), starlark::Error> {
        let error = buck2_error::Error::from(error).tag([ErrorTag::StarlarkError]);
        soft_error!(&format!("starlark_rust_{category}"), error, deprecation: true, quiet:true)
            .map_err(|e| starlark::Error::new_kind(ErrorKind::Other(e.into())))?;
        Ok(())
    }
}
