/*
 * Copyright 2018 The Starlark in Rust Authors.
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

use crate::any::IsStaticType;
use crate::values::FrozenHeap;
use crate::values::Heap;
use crate::values::HeapEdge;
use crate::values::HeapSendable;
use crate::values::HeapSyncable;
use crate::values::OwnedFrozen;
use crate::values::OwnedFrozenHeap;
use crate::values::layout::heap::heap_type::FrozenHeapName;

/// The two heaps of a [`Module`](crate::environment::Module): the value heap and the frozen heap
/// it is building, owned together.
///
/// Values allocated on the frozen heap are handed out at the value heap's brand, through the
/// [`HeapEdge`] that [`frozen_heap`](ModuleHeaps::frozen_heap) provides. That is sound because the
/// builder never leaves this type unsealed: [`seal_with`](ModuleHeaps::seal_with) and [`Drop`] are its
/// only exits, and both seal it into the value heap's references. The edge therefore exists from the
/// moment the two heaps do, with nothing to remember at the end.
#[derive(Debug)]
pub(crate) struct ModuleHeaps<'v> {
    heap: Heap<'v>,
    /// `None` only after `seal_with` has taken the builder out; `Drop` then has nothing to do.
    frozen: Option<OwnedFrozenHeap>,
}

impl<'v> ModuleHeaps<'v> {
    pub(crate) fn new(heap: Heap<'v>) -> Self {
        Self {
            heap,
            frozen: Some(OwnedFrozenHeap::new()),
        }
    }

    pub(crate) fn heap(&self) -> Heap<'v> {
        self.heap
    }

    /// Allocate on the frozen heap, see [`Module::frozen_heap`](crate::environment::Module::frozen_heap).
    pub(crate) fn frozen_heap<R>(
        &self,
        f: impl for<'fm> FnOnce(FrozenHeap<'fm>, HeapEdge<'v, 'fm>) -> R,
    ) -> R {
        self.frozen().with(|fh| {
            // SAFETY: This type is the only owner of the builder, and the builder only leaves it
            // through `seal_with` or `Drop`, both of which seal it into `heap`'s references. So
            // `'fm`'s allocations live as long as the value heap, which is as long as anything at
            // `'v` can. `'fm` is closure-introduced, so it is a true brand.
            let edge = unsafe { HeapEdge::unchecked_new() };
            f(fh, edge)
        })
    }

    pub(crate) fn frozen_heap_allocated_bytes(&self) -> usize {
        self.frozen().allocated_bytes()
    }

    fn frozen(&self) -> &OwnedFrozenHeap {
        self.frozen
            .as_ref()
            .expect("the builder is only taken by `seal_with`, which consumes `self`")
    }

    /// Allocate the frozen heap's root value with `f`, then seal the heap into the value heap's
    /// references and return the root value kept alive by it.
    ///
    /// The heap is sealed whether or not `f` succeeds. The `name` is the sealed heap's, see
    /// [`OwnedFrozen::name`].
    pub(crate) fn seal_with<T, E>(
        mut self,
        name: Option<FrozenHeapName>,
        f: impl for<'fm> FnOnce(FrozenHeap<'fm>) -> Result<T::Reinfect<'fm>, E>,
    ) -> Result<OwnedFrozen<T>, E>
    where
        T: IsStaticType,
        for<'fv> T::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv> + Sized,
    {
        let frozen = self
            .frozen
            .take()
            .expect("the builder is only taken by `seal_with`, which consumes `self`");
        let (sealed, root) =
            frozen.seal_with_impl(name, || Some(self.heap.peak_allocated_bytes()), f);
        self.heap.add_reference(sealed.owner());
        root
    }
}

impl<'v> Drop for ModuleHeaps<'v> {
    fn drop(&mut self) {
        // A module that is dropped rather than frozen: its frozen heap has to outlive it all the
        // same, see the type doc. An empty builder would seal into the empty heap, which keeps
        // nothing alive, so it is skipped.
        if let Some(frozen) = self.frozen.take()
            && !frozen.is_empty()
        {
            let sealed = frozen.seal_impl(None, Some(self.heap.peak_allocated_bytes()));
            self.heap.add_reference(sealed.owner());
        }
    }
}
