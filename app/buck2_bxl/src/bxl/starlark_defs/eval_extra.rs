/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::RefCell;
use std::io::Write;
use std::rc::Rc;

use buck2_events::dispatch::console_message;
use starlark::eval::Evaluator;
use starlark::values::ProvidesStaticType;

use crate::bxl::starlark_defs::context::BxlContextCoreData;
use crate::bxl::starlark_defs::context::ErrorPrinter;
use crate::bxl::starlark_defs::context::output::OutputStreamState;
use crate::bxl::starlark_defs::context::starlark_async::BxlDiceComputations;

enum BxlEvalExtraType {
    Root { stream_state: OutputStreamState },
    Dynamic,
    AnonTarget,
}

/// A tag that is only available when running in Bxl, to guard Bxl
/// functions from a non-Bxl context.
#[derive(ProvidesStaticType)]
pub(crate) struct BxlEvalExtra<'e> {
    pub(crate) dice: Rc<RefCell<dyn BxlDiceComputations + 'e>>,
    core: Rc<BxlContextCoreData>,
    eval_extra_type: BxlEvalExtraType,
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
pub(crate) enum BxlScopeError {
    #[error("This function can only be called from Bxl")]
    UnavailableOutsideBxl,
}

impl<'e> BxlEvalExtra<'e> {
    pub(crate) fn new(
        dice: Rc<RefCell<dyn BxlDiceComputations + 'e>>,
        core: Rc<BxlContextCoreData>,
        stream_state: OutputStreamState,
    ) -> Self {
        Self {
            dice,
            core,
            eval_extra_type: BxlEvalExtraType::Root { stream_state },
        }
    }

    pub(crate) fn new_dynamic(
        dice: Rc<RefCell<dyn BxlDiceComputations + 'e>>,
        core: Rc<BxlContextCoreData>,
    ) -> Self {
        Self {
            dice,
            core,
            eval_extra_type: BxlEvalExtraType::Dynamic,
        }
    }

    pub(crate) fn new_anon(
        dice: Rc<RefCell<dyn BxlDiceComputations + 'e>>,
        core: Rc<BxlContextCoreData>,
    ) -> Self {
        Self {
            dice,
            core,
            eval_extra_type: BxlEvalExtraType::AnonTarget,
        }
    }

    pub(crate) fn from_context<'v, 'a>(
        eval: &Evaluator<'v, 'a, 'e>,
    ) -> buck2_error::Result<&'a BxlEvalExtra<'e>> {
        let f = || eval.extra?.downcast_ref::<BxlEvalExtra>();
        f().ok_or_else(|| BxlScopeError::UnavailableOutsideBxl.into())
    }

    pub(crate) fn via_dice<'a, T>(
        &'a self,
        f: impl for<'x> FnOnce(
            &'x mut dyn BxlDiceComputations,
            &'a BxlContextCoreData,
        ) -> buck2_error::Result<T>,
    ) -> buck2_error::Result<T> {
        let core = &self.core;
        f(&mut *self.dice.borrow_mut(), core)
    }
}

impl<'e> ErrorPrinter for BxlEvalExtra<'e> {
    fn print_to_error_stream(&self, msg: String) -> buck2_error::Result<()> {
        match &self.eval_extra_type {
            BxlEvalExtraType::Root { stream_state } => writeln!(stream_state.error(), "{}", msg)?,
            BxlEvalExtraType::Dynamic => console_message(msg),
            BxlEvalExtraType::AnonTarget => console_message(msg),
        }
        Ok(())
    }
}
