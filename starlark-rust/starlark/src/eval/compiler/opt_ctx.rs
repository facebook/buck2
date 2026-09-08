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
use crate::eval::compiler::def_inline::local_as_value::LocalAsValue;
use crate::eval::compiler::stmt::OptimizeOnFreezeContext;
use crate::eval::runtime::slots::LocalSlotId;
use crate::values::FrozenHeap;
use crate::values::Heap;
use crate::values::HeapEdge;
use crate::values::Value;
use crate::values::ValueTyped;

/// The context the optimizer runs in: a module's value heap, which it speculates on, and the
/// module's frozen heap, where the IR it produces is allocated.
///
/// # Safety
///
/// `heap()` and `frozen_heap()` must be the two heaps of one `ModuleHeaps`, used within the scope
/// that type hands the frozen heap out in. [`OptCtx::demote`] moves frozen values from the first
/// to the second on the strength of that pairing.
pub(crate) unsafe trait OptCtxEval<'v, 'a, 'e, 'fm> {
    fn heap(&self) -> Heap<'v>;
    fn frozen_heap(&self) -> FrozenHeap<'fm>;
    /// The edge from the value heap to the frozen heap, see `ModuleHeaps`.
    fn edge(&self) -> HeapEdge<'v, 'fm>;
    fn eval(&mut self) -> Option<&mut Evaluator<'v, 'a, 'e>>;
    fn frozen_module(&self) -> Option<&FrozenModuleData<'fm>>;
    /// Storage for [`OptCtx::local_as_values`].
    fn local_as_values(&self) -> &[ValueTyped<'fm, LocalAsValue>];
    fn local_as_values_mut(&mut self) -> &mut Vec<ValueTyped<'fm, LocalAsValue>>;
}

// SAFETY: Constructed by `Def::post_freeze` alone, from the heaps `Module::freeze_impl` is
// sealing with.
unsafe impl<'v, 'a, 'e, 'fv> OptCtxEval<'v, 'a, 'e, 'fv> for OptimizeOnFreezeContext<'v, 'a, 'fv> {
    fn heap(&self) -> Heap<'v> {
        self.heap
    }

    fn frozen_heap(&self) -> FrozenHeap<'fv> {
        self.frozen_heap
    }

    fn edge(&self) -> HeapEdge<'v, 'fv> {
        self.edge
    }

    fn eval(&mut self) -> Option<&mut Evaluator<'v, 'a, 'e>> {
        None
    }

    fn frozen_module(&self) -> Option<&FrozenModuleData<'fv>> {
        Some(self.module)
    }

    fn local_as_values(&self) -> &[ValueTyped<'fv, LocalAsValue>] {
        &self.local_as_values
    }

    fn local_as_values_mut(&mut self) -> &mut Vec<ValueTyped<'fv, LocalAsValue>> {
        &mut self.local_as_values
    }
}

// SAFETY: Constructed by `Evaluator::eval_module` alone, with the evaluator's module heap and
// the frozen heap that module hands out.
unsafe impl<'v, 'a, 'e, 'x, 'fm> OptCtxEval<'v, 'a, 'e, 'fm> for Compiler<'v, 'a, 'e, 'x, 'fm> {
    fn heap(&self) -> Heap<'v> {
        self.eval.heap()
    }

    fn frozen_heap(&self) -> FrozenHeap<'fm> {
        self.fh
    }

    fn edge(&self) -> HeapEdge<'v, 'fm> {
        self.edge
    }

    fn eval(&mut self) -> Option<&mut Evaluator<'v, 'a, 'e>> {
        Some(self.eval)
    }

    fn frozen_module(&self) -> Option<&FrozenModuleData<'fm>> {
        None
    }

    fn local_as_values(&self) -> &[ValueTyped<'fm, LocalAsValue>] {
        &self.local_as_values
    }

    fn local_as_values_mut(&mut self) -> &mut Vec<ValueTyped<'fm, LocalAsValue>> {
        &mut self.local_as_values
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

    /// The edge from the value heap to the frozen heap: how the optimizer brings IR constants to
    /// `'v` to evaluate on them.
    pub(crate) fn edge(&self) -> HeapEdge<'v, 'fm> {
        self.eval.edge()
    }

    pub(crate) fn eval(&mut self) -> Option<&mut Evaluator<'v, 'a, 'e>> {
        self.eval.eval()
    }

    pub(crate) fn frozen_module(&self) -> Option<&FrozenModuleData<'fm>> {
        self.eval.frozen_module()
    }

    /// Bring a frozen value the optimizer observed at `'v` to the brand the IR is allocated at,
    /// or `None` if it is not frozen.
    ///
    /// The optimizer evaluates speculatively at `'v` (calls, attribute reads, operators, module
    /// slots) and folds frozen results into IR stored at `'fm`. No [`HeapEdge`] certifies that
    /// direction; this is the one place it is taken, and it rests on where a frozen value at `'v`
    /// can live: in the module's own frozen heap; in a heap that heap references (the globals,
    /// `load`ed modules); in `'static` data; or in a foreign heap that only the value heap
    /// references, which `ModuleHeaps` copies into the frozen heap when it is sealed. Each of
    /// those is kept alive as long as anything at `'fm`. The `branding` module lists this among
    /// the brand changes that rest on such a contract rather than on an edge.
    pub(crate) fn demote(&self, v: Value<'v>) -> Option<Value<'fm>> {
        if !v.is_frozen() {
            return None;
        }
        // SAFETY: `OptCtxEval`'s contract pairs the two heaps, and the reasoning above then covers
        // every frozen heap the value can live in.
        Some(unsafe { v.rebrand_frozen_unchecked() })
    }

    /// The placeholders for the first `count` local slots, see [`LocalAsValue`]: one allocation
    /// per slot for the whole compilation, grown on demand.
    pub(crate) fn local_as_values(&mut self, count: u32) -> &[ValueTyped<'fm, LocalAsValue>] {
        let frozen_heap = self.frozen_heap();
        let cache = self.eval.local_as_values_mut();
        while cache.len() < count as usize {
            cache.push(frozen_heap.alloc_simple_typed(LocalAsValue {
                local: LocalSlotId(cache.len() as u32),
            }));
        }
        &self.eval.local_as_values()[..count as usize]
    }
}
