/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::eval::Evaluator;
use starlark::values::ProvidesStaticType;

/// A tag that is only available when running in Bxl, to guard Bxl
/// functions from a non-Bxl context.
#[derive(ProvidesStaticType)]
pub(crate) struct BxlEvalExtraTag;

#[derive(Debug, buck2_error::Error)]
pub(crate) enum BxlContextError {
    #[error("This function can only be called from Bxl")]
    UnavailableOutsideBxl,
}

impl BxlEvalExtraTag {
    pub(crate) fn from_context<'v, 'a1, 'a2>(
        eval: &Evaluator<'v, 'a1, 'a2>,
    ) -> anyhow::Result<&'a1 BxlEvalExtraTag> {
        let f = || eval.extra?.downcast_ref::<BxlEvalExtraTag>();
        f().ok_or_else(|| BxlContextError::UnavailableOutsideBxl.into())
    }
}
