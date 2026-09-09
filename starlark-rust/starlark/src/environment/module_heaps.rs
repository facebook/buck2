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
/// only exits, and both seal it into the value heap's references. `seal_with` does so whether its
/// closure returns `Ok` or `Err`, and `Drop` covers a module that is never frozen as well as a
/// closure that unwinds. The edge therefore exists from the moment the two heaps do, with nothing
/// to remember at the end.
///
/// The other direction is covered too: the sealed heap takes over every heap the value heap
/// references at the moment of sealing, so a frozen value that was copied by pointer out of one
/// of those heaps stays alive as long as the sealed heap does.
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
        self.frozen().with(|fh| f(fh, self.edge(fh)))
    }

    /// The edge from the value heap to the builder, for a handle that `with` opened on the builder.
    fn edge<'fm>(&self, _fh: FrozenHeap<'fm>) -> HeapEdge<'v, 'fm> {
        // SAFETY: This type is the only owner of the builder, and the builder only leaves it
        // through `seal_with` or `Drop`, both of which seal it into `heap`'s references. So
        // `'fm`'s allocations live as long as the value heap, which is as long as anything at
        // `'v` can. `'fm` is introduced by the `with` closure, so it is a true brand.
        unsafe { HeapEdge::unchecked_new() }
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
    /// references and return the root value kept alive by it. `f` gets the same edge as
    /// [`frozen_heap`](ModuleHeaps::frozen_heap).
    ///
    /// The heap is sealed whether `f` succeeds, fails or panics. The `name` is the sealed heap's,
    /// see [`OwnedFrozen::name`].
    pub(crate) fn seal_with<T, E>(
        mut self,
        name: Option<FrozenHeapName>,
        f: impl for<'fm> FnOnce(FrozenHeap<'fm>, HeapEdge<'v, 'fm>) -> Result<T::Reinfect<'fm>, E>,
    ) -> Result<OwnedFrozen<T>, E>
    where
        T: IsStaticType,
        for<'fv> T::Reinfect<'fv>: HeapSendable<'fv> + HeapSyncable<'fv> + Sized,
    {
        // The builder stays in `self` while `f` runs, so that if `f` unwinds, `Drop` seals it like
        // on every other exit: the value heap may already hold pointers into it.
        //
        // SAFETY: `'fm` is the brand of the builder, which is sealed right below into the owner
        // the value is paired with. Being closure-introduced, `'fm` names nothing else.
        let root = self
            .frozen()
            .with(|fh| f(fh, self.edge(fh)).map(|v| unsafe { OwnedFrozen::<T>::erase_brand(v) }));
        let frozen = self
            .frozen
            .take()
            .expect("the builder is only taken by `seal_with`, which consumes `self`");
        // Frozen values may point into any heap the value heap references (a `load`ed module's
        // heap, a value that `add_to_heap` brought over), so the sealed heap takes those references
        // over. This runs after `f` so that references `f` itself added are included.
        frozen.with(|fh| {
            for r in self.heap.referenced_heaps() {
                fh.add_reference(r.owner());
            }
        });
        let sealed = frozen.seal_impl(name, Some(self.heap.peak_allocated_bytes()));
        self.heap.add_reference(sealed.owner());
        // SAFETY: `sealed` is the heap that `'fm` named.
        root.map(|v| unsafe { OwnedFrozen::from_erased(sealed, v) })
    }
}

impl<'v> Drop for ModuleHeaps<'v> {
    fn drop(&mut self) {
        // A module that is dropped rather than frozen, or a `seal_with` closure that unwound: the
        // frozen heap has to outlive the module all the same, see the type doc. An empty builder
        // would seal into the empty heap, which keeps nothing alive, so it is skipped. The value
        // heap's references are not handed over here: the sealed heap is reachable only from the
        // value heap, which holds them itself.
        if let Some(frozen) = self.frozen.take()
            && !frozen.is_empty()
        {
            let sealed = frozen.seal_impl(None, Some(self.heap.peak_allocated_bytes()));
            self.heap.add_reference(sealed.owner());
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::environment::module_heaps::ModuleHeaps;
    use crate::values::Freezer;
    use crate::values::Heap;
    use crate::values::OwnedFrozen;
    use crate::values::Value;
    use crate::values::layout::heap::heap_type::StarlarkTestHeapName;

    /// A frozen value from a heap that only the value heap references is frozen by pointer copy,
    /// so the sealed heap has to reference that heap too, even when the reference was added
    /// while sealing.
    #[test]
    fn test_seal_with_inherits_references_added_while_sealing() {
        let expected = "a string that lives on a heap of its own".repeat(8);
        let foreign =
            OwnedFrozen::<Value<'static>>::build(StarlarkTestHeapName::frozen_heap_name(), |fh| {
                fh.alloc(expected.as_str())
            });
        let root = Heap::temp(|heap| {
            let heaps = ModuleHeaps::new(heap);
            heaps
                .seal_with::<Value<'static>, ()>(None, |fh, _edge| {
                    let v = foreign.as_ref().add_to_heap(heap);
                    Ok(Freezer::new(fh).freeze(v).unwrap())
                })
                .unwrap()
        });
        assert!(root.refs().any(|r| r == foreign.owner()));
        root.by_ref(|v| assert_eq!(v.unpack_str(), Some(expected.as_str())));
    }
}

// `catch_unwind` needs an unwinding panic runtime; under `panic = "abort"` the hole this guards
// against cannot be exercised, since a panic ends the process.
#[cfg(all(test, panic = "unwind"))]
mod unwind_tests {
    use std::panic::AssertUnwindSafe;
    use std::panic::catch_unwind;

    use crate::environment::module_heaps::ModuleHeaps;
    use crate::values::Heap;
    use crate::values::Value;
    use crate::values::list::ListRef;

    /// The value heap can hold pointers into the frozen heap before it is sealed, so a panic
    /// inside the sealing closure must still seal it.
    #[test]
    fn test_unwinding_out_of_seal_with_seals() {
        Heap::temp(|heap| {
            let heaps = ModuleHeaps::new(heap);
            let expected = "a string that lives on the module's frozen heap".repeat(8);
            let s = heaps.frozen_heap(|fh, edge| edge.rebrand(fh.alloc(expected.as_str())));
            let list = heap.alloc(vec![s]);
            let unwound = catch_unwind(AssertUnwindSafe(|| {
                heaps.seal_with::<Value<'static>, ()>(None, |_fh, _edge| panic!("sealing failed"))
            }));
            assert!(unwound.is_err());
            let list = ListRef::from_value(list).unwrap();
            assert_eq!(list.content()[0].unpack_str(), Some(expected.as_str()));
        })
    }
}
