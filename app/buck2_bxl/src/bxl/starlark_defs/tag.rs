/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::rc::Rc;

use starlark::eval::Evaluator;
use starlark::values::ProvidesStaticType;

use super::context::starlark_async::BxlDiceComputations;

/// A tag that is only available when running in Bxl, to guard Bxl
/// functions from a non-Bxl context.
#[derive(ProvidesStaticType)]
pub(crate) struct BxlEvalExtraTag<'e> {
    #[allow(unused)]
    pub(crate) dice: Rc<RefCell<dyn BxlDiceComputations + 'e>>,
}

#[derive(Debug, buck2_error::Error)]
pub(crate) enum BxlContextError {
    #[error("This function can only be called from Bxl")]
    UnavailableOutsideBxl,
}

impl<'e> BxlEvalExtraTag<'e> {
    pub(crate) fn new(dice: Rc<RefCell<dyn BxlDiceComputations + 'e>>) -> Self {
        Self { dice }
    }

    pub(crate) fn from_context<'v, 'a>(
        eval: &Evaluator<'v, 'a, 'e>,
    ) -> anyhow::Result<&'a BxlEvalExtraTag<'e>> {
        let f = || eval.extra?.downcast_ref::<BxlEvalExtraTag>();
        f().ok_or_else(|| BxlContextError::UnavailableOutsideBxl.into())
    }
}
