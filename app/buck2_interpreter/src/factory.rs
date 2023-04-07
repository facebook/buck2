/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::eval::Evaluator;

/// Provides a starlark Evaluator.
pub trait StarlarkEvaluatorProvider {
    /// Creates an Evaluator for a module. The evaluator will be configured for instrumenting/profiling/debugging
    /// as appropriate.
    fn make<'v, 'a>(&mut self, module: &'v Module) -> anyhow::Result<Evaluator<'v, 'a>>;

    fn evaluation_complete(&mut self, eval: &mut Evaluator) -> anyhow::Result<()>;

    fn visit_frozen_module(&mut self, module: Option<&FrozenModule>) -> anyhow::Result<()>;
}

/// Trivial provider that just constructs an Evaluator. Useful for tests (but not necessarily limited to them).
pub struct StarlarkPassthroughProvider;

impl StarlarkEvaluatorProvider for StarlarkPassthroughProvider {
    fn make<'v, 'a>(&mut self, module: &'v Module) -> anyhow::Result<Evaluator<'v, 'a>> {
        Ok(Evaluator::new(module))
    }

    fn evaluation_complete(&mut self, _eval: &mut Evaluator) -> anyhow::Result<()> {
        Ok(())
    }

    fn visit_frozen_module(&mut self, _module: Option<&FrozenModule>) -> anyhow::Result<()> {
        Ok(())
    }
}
