/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use crate::environment::FrozenModuleData;
use crate::eval::Evaluator;
use crate::eval::compiler::Compiler;
use crate::eval::compiler::stmt::OptimizeOnFreezeContext;
use crate::values::FrozenHeap;
use crate::values::Heap;

pub(crate) trait OptCtxEval<'v, 'a, 'e, 'fm> {
    fn heap(&self) -> Heap<'v>;
    fn frozen_heap(&self) -> FrozenHeap<'fm>;
    fn eval(&mut self) -> Option<&mut Evaluator<'v, 'a, 'e>>;
    fn frozen_module(&self) -> Option<&FrozenModuleData<'fm>>;
}

impl<'v, 'a, 'e, 'fv> OptCtxEval<'v, 'a, 'e, 'fv> for OptimizeOnFreezeContext<'v, 'a, 'fv> {
    fn heap(&self) -> Heap<'v> {
        self.heap
    }

    fn frozen_heap(&self) -> FrozenHeap<'fv> {
        self.frozen_heap
    }

    fn eval(&mut self) -> Option<&mut Evaluator<'v, 'a, 'e>> {
        None
    }

    fn frozen_module(&self) -> Option<&FrozenModuleData<'fv>> {
        Some(self.module)
    }
}

impl<'v, 'a, 'e, 'x, 'fm> OptCtxEval<'v, 'a, 'e, 'fm> for Compiler<'v, 'a, 'e, 'x, 'fm> {
    fn heap(&self) -> Heap<'v> {
        self.eval.heap()
    }

    fn frozen_heap(&self) -> FrozenHeap<'fm> {
        self.fh
    }

    fn eval(&mut self) -> Option<&mut Evaluator<'v, 'a, 'e>> {
        Some(self.eval)
    }

    fn frozen_module(&self) -> Option<&FrozenModuleData<'fm>> {
        None
    }
}

/// Optimization context.
///
/// We perform optimization
/// * during compilation of AST to IR, and
/// * when freezing the heap.
pub(crate) struct OptCtx<'v: 'a, 'a, 'e: 'a, 'x, 'fm> {
    pub(crate) eval: &'x mut dyn OptCtxEval<'v, 'a, 'e, 'fm>,
    /// Current function parameter slot count. Zero when compiling module.
    pub(crate) param_count: u32,
}

impl<'v, 'a, 'e: 'a, 'x, 'fm> OptCtx<'v, 'a, 'e, 'x, 'fm> {
    pub(crate) fn new(
        eval: &'x mut dyn OptCtxEval<'v, 'a, 'e, 'fm>,
        param_count: u32,
    ) -> OptCtx<'v, 'a, 'e, 'x, 'fm> {
        OptCtx { eval, param_count }
    }

    pub(crate) fn heap(&self) -> Heap<'v> {
        self.eval.heap()
    }

    pub(crate) fn frozen_heap(&self) -> FrozenHeap<'fm> {
        self.eval.frozen_heap()
    }

    pub(crate) fn eval(&mut self) -> Option<&mut Evaluator<'v, 'a, 'e>> {
        self.eval.eval()
    }

    pub(crate) fn frozen_module(&self) -> Option<&FrozenModuleData<'fm>> {
        self.eval.frozen_module()
    }
}
