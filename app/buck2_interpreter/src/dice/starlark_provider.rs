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

use crate::factory::StarlarkEvaluatorProvider;
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
    _ctx: &DiceComputations,
    profiler_instrumentation: &mut StarlarkProfilerOrInstrumentation<'_>,
    _description: String,
    closure: impl FnOnce(&mut dyn StarlarkEvaluatorProvider) -> anyhow::Result<R>,
) -> anyhow::Result<R> {
    struct EvalProvider<'a, 'b> {
        profiler: &'a mut StarlarkProfilerOrInstrumentation<'b>,
    }

    impl StarlarkEvaluatorProvider for EvalProvider<'_, '_> {
        fn make<'v, 'a>(&mut self, module: &'v Module) -> anyhow::Result<Evaluator<'v, 'a>> {
            let mut eval = Evaluator::new(module);
            self.profiler.initialize(&mut eval)?;
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
        };

        closure(&mut provider)
    }
}
