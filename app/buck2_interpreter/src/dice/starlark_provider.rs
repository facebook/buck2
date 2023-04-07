/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use dice::DiceComputations;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::eval::Evaluator;

use crate::dice::starlark_debug::HasStarlarkDebugger;
use crate::factory::StarlarkEvaluatorProvider;
use crate::starlark_debug::StarlarkDebugController;
use crate::starlark_profiler::StarlarkProfilerOrInstrumentation;

/// This constructs an appropriate StarlarkEvaluatorProvider to set up
/// profiling/instrumentation/debugging in a starlark Evaluator for buck.
///
/// Taking this via a closure ensures that the Evaluator isn't used in an
/// async context and allows us to do things like the block_in_place required
/// when debugging.
///
/// The description is used for the thread name when debugging.
///
/// The provided closure will be invoked and passed an appropriate
/// StarlarkEvaluatorProvider.
pub async fn with_starlark_eval_provider<R>(
    ctx: &DiceComputations,
    profiler_instrumentation: &mut StarlarkProfilerOrInstrumentation<'_>,
    description: String,
    closure: impl FnOnce(&mut dyn StarlarkEvaluatorProvider) -> anyhow::Result<R>,
) -> anyhow::Result<R> {
    let debugger_handle = ctx.get_starlark_debugger_handle();
    let debugger = match debugger_handle {
        Some(v) => Some(v.start_eval(&description).await?),
        None => None,
    };

    struct EvalProvider<'a, 'b> {
        profiler: &'a mut StarlarkProfilerOrInstrumentation<'b>,
        debugger: Option<Box<dyn StarlarkDebugController>>,
    }

    impl StarlarkEvaluatorProvider for EvalProvider<'_, '_> {
        fn make<'v, 'a>(&mut self, module: &'v Module) -> anyhow::Result<Evaluator<'v, 'a>> {
            let mut eval = Evaluator::new(module);
            self.profiler.initialize(&mut eval)?;
            if let Some(v) = &mut self.debugger {
                v.initialize(&mut eval)?;
            }
            Ok(eval)
        }

        fn evaluation_complete(&mut self, eval: &mut Evaluator) -> anyhow::Result<()> {
            self.profiler.evaluation_complete(eval)
        }

        fn visit_frozen_module(&mut self, module: Option<&FrozenModule>) -> anyhow::Result<()> {
            self.profiler.visit_frozen_module(module)
        }
    }

    {
        let mut provider = EvalProvider {
            profiler: profiler_instrumentation,
            debugger,
        };

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
        if debugger_handle.is_some() {
            tokio::task::block_in_place(move || closure(&mut provider))
        } else {
            closure(&mut provider)
        }
    }
}
