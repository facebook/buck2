/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::environment::Module;
use starlark::eval::Evaluator;

/// Provides a starlark Evaluator.
pub trait StarlarkEvaluatorProvider {
    /// Creates an Evaluator for a module. The evaluator will be configured for instrumenting/profiling/debugging
    /// as appropriate. Also returns whether profiling is enabled.
    fn make<'v, 'a, 'e>(
        &mut self,
        module: &'v Module,
    ) -> buck2_error::Result<(Evaluator<'v, 'a, 'e>, bool)>;

    fn evaluation_complete(&mut self, eval: &mut Evaluator) -> buck2_error::Result<()>;
}

/// Trivial provider that just constructs an Evaluator. Useful for tests (but not necessarily limited to them).
pub struct StarlarkPassthroughProvider;

impl StarlarkEvaluatorProvider for StarlarkPassthroughProvider {
    fn make<'v, 'a, 'e>(
        &mut self,
        module: &'v Module,
    ) -> buck2_error::Result<(Evaluator<'v, 'a, 'e>, bool)> {
        Ok((Evaluator::new(module), true))
    }

    fn evaluation_complete(&mut self, _eval: &mut Evaluator) -> buck2_error::Result<()> {
        Ok(())
    }
}
