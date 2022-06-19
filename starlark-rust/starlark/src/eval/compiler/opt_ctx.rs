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

use crate::{
    eval::{compiler::stmt::OptimizeOnFreezeContext, Evaluator},
    values::{FrozenHeap, Heap},
};

pub(crate) trait OptCtxEval<'v, 'a> {
    fn heap(&self) -> &'v Heap;
    fn frozen_heap(&self) -> &FrozenHeap;
    fn eval(&mut self) -> Option<&mut Evaluator<'v, 'a>>;
}

impl<'v, 'a> OptCtxEval<'v, 'a> for OptimizeOnFreezeContext<'v, 'a> {
    fn heap(&self) -> &'v Heap {
        self.heap
    }

    fn frozen_heap(&self) -> &FrozenHeap {
        self.frozen_heap
    }

    fn eval(&mut self) -> Option<&mut Evaluator<'v, 'a>> {
        None
    }
}

impl<'v, 'a> OptCtxEval<'v, 'a> for Evaluator<'v, 'a> {
    fn heap(&self) -> &'v Heap {
        self.heap()
    }

    fn frozen_heap(&self) -> &FrozenHeap {
        self.frozen_heap()
    }

    fn eval(&mut self) -> Option<&mut Evaluator<'v, 'a>> {
        Some(self)
    }
}

/// Optimization context.
///
/// We perform optimization
/// * during compilation of AST to IR, and
/// * when freezing the heap.
pub(crate) struct OptCtx<'v, 'a, 'e> {
    pub(crate) eval: &'e mut dyn OptCtxEval<'v, 'a>,
}

impl<'v, 'a, 'e> OptCtx<'v, 'a, 'e> {
    pub(crate) fn new(eval: &'e mut dyn OptCtxEval<'v, 'a>) -> OptCtx<'v, 'a, 'e> {
        OptCtx { eval }
    }

    pub(crate) fn heap(&self) -> &'v Heap {
        self.eval.heap()
    }

    pub(crate) fn frozen_heap(&self) -> &FrozenHeap {
        self.eval.frozen_heap()
    }

    pub(crate) fn eval(&mut self) -> Option<&mut Evaluator<'v, 'a>> {
        self.eval.eval()
    }
}
