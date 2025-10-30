/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

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
pub(crate) struct BxlEvalExtra<'d> {
    pub(crate) dice: Box<dyn BxlDiceComputations<'d> + 'd>,
    core: Rc<BxlContextCoreData>,
    eval_extra_type: BxlEvalExtraType,
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
pub(crate) enum BxlScopeError {
    #[error("This function can only be called from Bxl")]
    UnavailableOutsideBxl,
}

impl<'d> BxlEvalExtra<'d> {
    pub(crate) fn new(
        dice: Box<dyn BxlDiceComputations<'d> + 'd>,
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
        dice: Box<dyn BxlDiceComputations<'d> + 'd>,
        core: Rc<BxlContextCoreData>,
    ) -> Self {
        Self {
            dice,
            core,
            eval_extra_type: BxlEvalExtraType::Dynamic,
        }
    }

    pub(crate) fn new_anon(
        dice: Box<dyn BxlDiceComputations<'d> + 'd>,
        core: Rc<BxlContextCoreData>,
    ) -> Self {
        Self {
            dice,
            core,
            eval_extra_type: BxlEvalExtraType::AnonTarget,
        }
    }

    pub(crate) fn from_context<'s, 'v, 'a>(
        eval: &'s mut Evaluator<'v, 'a, 'd>,
    ) -> buck2_error::Result<&'s mut BxlEvalExtra<'d>> {
        match &mut eval.extra_mut {
            Some(extra) => extra.downcast_mut::<BxlEvalExtra>(),
            None => None,
        }
        .ok_or_else(|| BxlScopeError::UnavailableOutsideBxl.into())
    }

    pub(crate) fn via_dice<'a, T, E>(
        &'a mut self,
        f: impl FnOnce(&'a mut dyn BxlDiceComputations<'d>, &'a BxlContextCoreData) -> Result<T, E>,
    ) -> Result<T, E> {
        let core = &self.core;
        f(&mut *self.dice, core)
    }
}

impl<'e> ErrorPrinter for BxlEvalExtra<'e> {
    fn print_to_error_stream(&self, msg: String) -> buck2_error::Result<()> {
        match &self.eval_extra_type {
            BxlEvalExtraType::Root { stream_state } => writeln!(stream_state.error(), "{msg}")?,
            BxlEvalExtraType::Dynamic => console_message(msg),
            BxlEvalExtraType::AnonTarget => console_message(msg),
        }
        Ok(())
    }
}
