/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::key::BuckconfigKeyRef;
use buck2_error::conversion::from_any_with_tag;
use buck2_error::internal_error;
use dice::DiceComputations;
use dupe::Dupe;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::eval::Evaluator;

use crate::dice::starlark_debug::HasStarlarkDebugger;
use crate::dice::starlark_provider::CancellationPoller;
use crate::dice::starlark_provider::StarlarkEvalKind;
use crate::starlark_debug::StarlarkDebugController;
use crate::starlark_profiler::data::ProfileTarget;
use crate::starlark_profiler::data::StarlarkProfileDataAndStats;
use crate::starlark_profiler::profiler::StarlarkProfiler;

pub struct StarlarkEvaluatorProvider {
    profiler: StarlarkProfiler,
    debugger: Option<Box<dyn StarlarkDebugController>>,
    starlark_max_callstack_size: Option<usize>,
}

/// Holds profiler data associated with completed Starlark evaluation.
///
/// The caller is responsible for calling finish() to ensure that profile data collection is completed.
///
/// TODO(cjhopman): Once we are automatically reporting profile data, make it a soft error to leave this
/// unfinished (silent by default, non-silent when profiling actually enabled)
#[must_use]
pub struct FinishedStarlarkEvaluation {
    profiler: StarlarkProfiler,
}

impl FinishedStarlarkEvaluation {
    /// Collect all profiling data.
    pub fn finish(
        self,
        frozen_module: Option<&FrozenModule>,
    ) -> buck2_error::Result<Option<StarlarkProfileDataAndStats>> {
        self.profiler.finish(frozen_module)
    }
}

impl StarlarkEvaluatorProvider {
    /// Trivial provider that just constructs an Evaluator. Useful for tests (but not necessarily limited to them).
    pub fn passthrough() -> Self {
        Self {
            profiler: StarlarkProfiler::new(None, ProfileTarget::Unknown),
            debugger: None,
            starlark_max_callstack_size: None,
        }
    }

    /// This constructs an appropriate StarlarkEvaluatorProvider to set up
    /// profiling/instrumentation/debugging in a starlark Evaluator for buck.
    /// The kind is used for the thread name when debugging and for enabling pattern-based profiling.
    pub async fn new(
        ctx: &mut DiceComputations<'_>,
        kind: &StarlarkEvalKind,
        profiler: StarlarkProfiler,
    ) -> buck2_error::Result<StarlarkEvaluatorProvider> {
        let root_buckconfig = ctx.get_legacy_root_config_on_dice().await?;

        let starlark_max_callstack_size =
            root_buckconfig.view(ctx).parse::<usize>(BuckconfigKeyRef {
                section: "buck2",
                property: "starlark_max_callstack_size",
            })?;

        let debugger_handle = ctx.get_starlark_debugger_handle();
        let debugger = match debugger_handle {
            Some(v) => Some(v.start_eval(&kind.to_string()).await?),
            None => None,
        };

        Ok(Self {
            profiler,
            debugger,
            starlark_max_callstack_size,
        })
    }

    /// Construct a "reentrant" evaluator, one where we can have multiple stages of interacting with
    /// the Evaluator. The underlying starlark Evaluator will persist across calls to
    /// ReentrantStarlarkEvaluator::with_evaluator.
    ///
    /// For example, this is used in analysis evaluations to:
    ///  (1) evaluate main analysis phase
    ///  (2) async wait for anon targets
    ///  (3) re-enter evaluation to resolve promises.
    pub fn make_reentrant_evaluator<'x, 'v, 'a, 'e>(
        mut self,
        module: &'v Module,
        cancellation: CancellationPoller<'a>,
    ) -> buck2_error::Result<ReentrantStarlarkEvaluator<'x, 'v, 'a, 'e>> {
        let (_, _v) = (buck2_error::Ok(()), 1);
        let mut eval = Evaluator::new(module);
        if let Some(stack_size) = self.starlark_max_callstack_size {
            eval.set_max_callstack_size(stack_size)
                .map_err(|e| from_any_with_tag(e, buck2_error::ErrorTag::Tier0))?;
        }
        match cancellation.dupe() {
            CancellationPoller::None => {}
            CancellationPoller::Context(c) => {
                eval.set_check_cancelled(Box::new(|| c.is_cancellation_requested()))
            }
            CancellationPoller::Observer(o) => {
                eval.set_check_cancelled(Box::new(move || o.is_cancellation_requested()))
            }
        }

        let is_profiling_enabled = self.profiler.profiler_data.initialize(&mut eval)?;
        if let Some(v) = &mut self.debugger {
            v.initialize(&mut eval)?;
        }

        Ok(ReentrantStarlarkEvaluator::Normal {
            eval,
            provider: self,
            is_profiling_enabled,
        })
    }

    /// Applies a closure to a starlark Evaluator. Taking this via a closure ensures that the Evaluator
    /// isn't used in an async context and allows us to do things like the block_in_place required
    /// when debugging.
    pub fn with_evaluator<'v, 'a, 'e: 'a, R>(
        self,
        module: &'v Module,
        cancellation: CancellationPoller<'a>,
        closure: impl FnOnce(&mut Evaluator<'v, 'a, 'e>, bool) -> buck2_error::Result<R>,
    ) -> buck2_error::Result<(FinishedStarlarkEvaluation, R)> {
        let mut reentrant_eval: ReentrantStarlarkEvaluator<'_, 'v, '_, '_> =
            self.make_reentrant_evaluator(module, cancellation)?;
        let is_profiling_enabled = reentrant_eval.is_profiling_enabled();
        let res = reentrant_eval.with_evaluator(|eval| closure(eval, is_profiling_enabled))?;
        Ok((reentrant_eval.finish_evaluation()?, res))
    }
}

#[allow(clippy::large_enum_variant)]
pub enum ReentrantStarlarkEvaluator<'x, 'v, 'a, 'e: 'a> {
    Normal {
        eval: Evaluator<'v, 'a, 'e>,
        provider: StarlarkEvaluatorProvider,
        is_profiling_enabled: bool,
    },
    // This is awkward. It's used by bxl when doing a blocking resolve of anon target promises.
    Wrapped {
        eval: &'x mut Evaluator<'v, 'a, 'e>,
        is_debugging_enabled: bool,
    },
}

impl<'x, 'v, 'a, 'e: 'a> ReentrantStarlarkEvaluator<'x, 'v, 'a, 'e> {
    pub fn wrap_evaluator_without_profiling(
        ctx: &mut DiceComputations<'_>,
        eval: &'x mut Evaluator<'v, 'a, 'e>,
    ) -> Self {
        let is_debugging_enabled = ctx.get_starlark_debugger_handle().is_some();
        Self::Wrapped {
            eval,
            is_debugging_enabled,
        }
    }

    pub fn with_evaluator<R>(
        &mut self,
        closure: impl FnOnce(&mut Evaluator<'v, 'a, 'e>) -> buck2_error::Result<R>,
    ) -> buck2_error::Result<R> {
        // If we're debugging, we need to move this to a tokio blocking task.
        //
        // This is required because the debugger itself is running on the
        // tokio worker tasks, and if we have a starlark breakpoint in common
        // code we could get a lot of evaluators all blocked waiting on the debugger
        // and those could block all the tokio worker tasks and the debugger wouldn't
        // even get a chance to resume them.
        //
        // It's the debuggers responsibility to ensure that we don't run too many
        // evaluations concurrently (in the non-debugger case they are limited by the
        // tokio worker tasks, but once in a blocking task that limit is greatly
        // increased).

        // TODO(cjhopman): It would be nicer if we could have this functionality be
        // provided by the debugger handle, but I couldn't figure out a nice clean
        // way to do that. Potentially the thing would be to invert the dependencies
        // so we could operate against a concrete type rather than injecting a trait
        // implementation.

        if self.is_debugging_enabled() {
            tokio::task::block_in_place(|| closure(self.eval()))
        } else {
            closure(self.eval())
        }
    }

    fn eval(&mut self) -> &mut Evaluator<'v, 'a, 'e> {
        match self {
            ReentrantStarlarkEvaluator::Normal { eval, .. } => eval,
            ReentrantStarlarkEvaluator::Wrapped { eval, .. } => eval,
        }
    }

    fn is_debugging_enabled(&self) -> bool {
        match self {
            ReentrantStarlarkEvaluator::Normal { provider, .. } => provider.debugger.is_some(),
            ReentrantStarlarkEvaluator::Wrapped {
                is_debugging_enabled,
                ..
            } => *is_debugging_enabled,
        }
    }

    fn is_profiling_enabled(&self) -> bool {
        match self {
            ReentrantStarlarkEvaluator::Normal {
                is_profiling_enabled,
                ..
            } => *is_profiling_enabled,
            ReentrantStarlarkEvaluator::Wrapped { .. } => false,
        }
    }

    pub fn finish_evaluation(self) -> buck2_error::Result<FinishedStarlarkEvaluation> {
        match self {
            ReentrantStarlarkEvaluator::Normal {
                mut eval,
                mut provider,
                ..
            } => {
                provider
                    .profiler
                    .profiler_data
                    .evaluation_complete(&mut eval);
                Ok(FinishedStarlarkEvaluation {
                    profiler: provider.profiler,
                })
            }
            ReentrantStarlarkEvaluator::Wrapped { .. } => {
                Err(internal_error!("Wrapped evaluator cannot be finished"))
            }
        }
    }
}
