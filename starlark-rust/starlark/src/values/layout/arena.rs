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

//! A heap storing AValue traits. The heap is a sequence of the
//! AValue vtable, followed by the payload.
//! Every payload must be at least 1 usize large (even ZST).
//! Some elements are created using reserve, in which case they point
//! to a BlackHole until they are filled in.
//!
//! Some elements can be overwritten (typically during GC) by a usize.
//! In these cases the bottom bit of the usize as used by the heap
//! to tag it as being a usize, and the word after is the size of the
//! item it replaced.

use std::alloc::Layout;
use std::cmp;
use std::collections::HashMap;
use std::hash::Hash;
use std::marker::PhantomData;
use std::mem;
use std::mem::ManuallyDrop;
use std::mem::MaybeUninit;
use std::ptr;
use std::slice;

use bumpalo::Bump;
use either::Either;
use gazebo::any::AnyLifetime;
use gazebo::prelude::*;

use crate::collections::StarlarkHashValue;
use crate::values::layout::avalue::starlark_str;
use crate::values::layout::avalue::AValue;
use crate::values::layout::avalue::BlackHole;
use crate::values::layout::vtable::AValueDyn;
use crate::values::layout::vtable::AValueVTable;
use crate::values::string::StarlarkStr;
use crate::values::StarlarkValue;

/// Min size of allocated object including header.
/// Should be able to fit `BlackHole` or forward.
const MIN_ALLOC: usize = {
    const fn max(a: usize, b: usize) -> usize {
        if a > b { a } else { b }
    }

    max(
        mem::size_of::<AValueForward>(),
        mem::size_of::<AValueRepr<BlackHole>>(),
    )
};

#[derive(Default)]
pub(crate) struct Arena {
    /// Arena for things which don't need dropping (e.g. strings)
    non_drop: Bump,
    /// Arena for things which might need dropping (e.g. Vec, with memory on heap)
    drop: Bump,
}

#[derive(Clone)]
#[repr(transparent)]
pub(crate) struct AValueHeader(&'static AValueVTable);

impl Hash for AValueHeader {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        ptr::hash(self.0, state);
    }
}

impl PartialEq for AValueHeader {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}

impl Eq for AValueHeader {}

// Implements Copy so this is fine
impl Dupe for AValueHeader {}

/// How object is represented in arena.
#[repr(C)]
pub(crate) struct AValueRepr<T> {
    pub(crate) header: AValueHeader,
    pub(crate) payload: T,
}

/// This is object written over [`AValueRepr`] during GC.
#[repr(C)]
pub(crate) struct AValueForward {
    /// Moved object pointer with lowest bit set.
    forward_ptr: usize,
    /// Size of `<T>`. Does not include [`AValueHeader`].
    object_size: usize,
}

impl AValueForward {
    /// Unpack forward pointer.
    fn forward_ptr(&self) -> usize {
        debug_assert!((self.forward_ptr & 1) != 0);
        self.forward_ptr & !1
    }
}

/// Object on the heap, either a real object or a forward.
#[repr(C)]
union AValueOrForward {
    // We intentionally do not implement `Copy` for these types
    // to avoid accidentally copying them.
    header: ManuallyDrop<AValueHeader>,
    forward: ManuallyDrop<AValueForward>,
    flags: usize,
}

impl AValueOrForward {
    /// Is this pointer a value or forward?
    fn is_forward(&self) -> bool {
        unsafe { (self.flags & 1) != 0 }
    }

    fn unpack(&self) -> Either<&AValueHeader, &AValueForward> {
        if self.is_forward() {
            Either::Right(unsafe { &self.forward })
        } else {
            Either::Left(unsafe { &self.header })
        }
    }
}

impl AValueForward {
    pub(crate) fn assert_does_not_overwrite_extra<'v, T: AValue<'v>>() {
        assert!(mem::size_of::<AValueForward>() <= AValueRepr::<T>::offset_of_extra());
    }
}

/// Reservation is morally a Reservation<T>, but we treat is as an
/// existential.
/// Tied to the lifetime of the heap.
pub(crate) struct Reservation<'v, 'v2, T: AValue<'v2>> {
    pointer: *mut AValueRepr<T>, // Secretly AValueObject<T>
    phantom: PhantomData<(&'v (), &'v2 T)>,
}

impl<'v, 'v2, T: AValue<'v2>> Reservation<'v, 'v2, T> {
    pub(crate) fn fill(self, x: T) {
        unsafe {
            ptr::write(
                self.pointer,
                AValueRepr {
                    header: AValueHeader::new::<T>(),
                    payload: x,
                },
            );
        }
    }

    pub(crate) fn ptr(&self) -> &'v AValueHeader {
        unsafe { &(*self.pointer).header }
    }
}

#[derive(Debug)]
/// Information about the data stored on a heap. Accessible through
/// the function `allocated_summary` available on [`Heap`](crate::values::Heap)
/// and [`FrozenHeap`](crate::values::FrozenHeap)
pub struct HeapSummary {
    /// For each type, give the (number of entries, size of all entries).
    /// The size may be approximate as it includes information from
    /// the approximate [`memory_size`](StarlarkValue::memory_size) function.
    pub summary: HashMap<String, (usize, usize)>,
}

impl Arena {
    pub fn allocated_bytes(&self) -> usize {
        self.drop.allocated_bytes() + self.non_drop.allocated_bytes()
    }

    pub fn available_bytes(&self) -> usize {
        self.drop.chunk_capacity() + self.non_drop.chunk_capacity()
    }

    fn alloc_uninit<'v, 'v2: 'v, T: AValue<'v2>>(
        bump: &'v Bump,
        extra_len: usize,
    ) -> (
        &'v mut MaybeUninit<AValueRepr<T>>,
        &'v mut [MaybeUninit<T::ExtraElem>],
    ) {
        assert!(
            mem::align_of::<T>() <= mem::align_of::<AValueHeader>(),
            "Unexpected alignment in Starlark arena. Type {} has alignment {}, expected <= {}",
            std::any::type_name::<T>(),
            mem::align_of::<T>(),
            mem::align_of::<AValueHeader>()
        );

        // We require at least usize space available for overwrite/blackhole
        let size = cmp::max(
            mem::size_of::<AValueHeader>() + T::memory_size_for_extra_len(extra_len),
            MIN_ALLOC,
        );
        let layout = Layout::from_size_align(size, mem::align_of::<AValueHeader>()).unwrap();
        let p = bump.alloc_layout(layout).as_ptr();
        unsafe {
            let repr = &mut *(p as *mut MaybeUninit<AValueRepr<T>>);
            let extra = slice::from_raw_parts_mut(
                (p as *mut u8).add(AValueRepr::<T>::offset_of_extra()) as *mut _,
                extra_len,
            );
            (repr, extra)
        }
    }

    fn bump_for_type<'v, T: AValue<'v>>(&self) -> &Bump {
        if mem::needs_drop::<T>() {
            &self.drop
        } else {
            &self.non_drop
        }
    }

    // Reservation should really be an incremental type
    pub(crate) fn reserve_with_extra<'v, 'v2: 'v, T: AValue<'v2>>(
        &'v self,
        extra_len: usize,
    ) -> (Reservation<'v, 'v2, T>, &'v mut [MaybeUninit<T::ExtraElem>]) {
        // We don't create reservations for strings because we don't need to,
        // but also because we need to be able to reconstruct a `Pointer`
        // from `AValueHeader` (with `TAG_STR` when appropriate).
        // `BlackHole` assumes it is created for non-string, so
        // it returns `false` from `is_str`.
        assert!(!T::IS_STR);

        let (p, extra) = Self::alloc_uninit::<T>(self.bump_for_type::<T>(), extra_len);
        // If we don't have a vtable we can't skip over missing elements to drop,
        // so very important to put in a current vtable
        // We always alloc at least one pointer worth of space, so can write in a one-ST blackhole

        let x = BlackHole(T::memory_size_for_extra_len(extra_len));
        let p = unsafe {
            transmute!(
                &mut MaybeUninit<AValueRepr<T>>,
                &mut MaybeUninit<AValueRepr<BlackHole>>,
                p
            )
        };
        let p = p.write(AValueRepr {
            header: AValueHeader(AValueVTable::new_black_hole()),
            payload: x,
        });
        let p = unsafe { transmute!(&mut AValueRepr<BlackHole>, &mut AValueRepr<T>, p) };

        (
            Reservation {
                pointer: p,
                phantom: PhantomData,
            },
            extra,
        )
    }

    /// Allocate a type `T`.
    pub(crate) fn alloc<'v, 'v2: 'v, T: AValue<'v2, ExtraElem = ()>>(
        &'v self,
        x: T,
    ) -> &'v AValueRepr<T> {
        debug_assert!(x.extra_len() == 0);
        let bump = self.bump_for_type::<T>();
        let (p, extra) = Self::alloc_uninit::<T>(bump, 0);
        debug_assert!(extra.is_empty());
        p.write(AValueRepr {
            header: AValueHeader::new::<T>(),
            payload: x,
        })
    }

    /// Allocate a type `T` plus `extra` bytes.
    ///
    /// The type `T` will never be dropped, so had better not do any memory allocation.
    pub(crate) fn alloc_extra<'v, 'v2: 'v, T: AValue<'v2>>(
        &'v self,
        x: T,
    ) -> (*mut AValueRepr<T>, &'v mut [MaybeUninit<T::ExtraElem>]) {
        let bump = self.bump_for_type::<T>();
        let (p, extra) = Self::alloc_uninit::<T>(bump, x.extra_len());
        let p = p.write(AValueRepr {
            header: AValueHeader::new::<T>(),
            payload: x,
        });
        (p, extra)
    }

    #[inline]
    pub(crate) fn alloc_str_init(
        &self,
        len: usize,
        hash: StarlarkHashValue,
        init: impl FnOnce(*mut u8),
    ) -> *mut AValueHeader {
        assert!(len > 1);
        let (v, extra) = self.alloc_extra::<_>(starlark_str(len, hash));
        debug_assert_eq!(StarlarkStr::payload_len_for_len(len), extra.len());
        unsafe {
            extra.last_mut().unwrap_unchecked().write(0usize);
        }
        init(extra.as_mut_ptr() as *mut u8);
        unsafe { &mut (*v).header }
    }

    #[inline]
    pub(crate) fn alloc_str(&self, x: &str) -> *mut AValueHeader {
        self.alloc_str_init(x.len(), StarlarkStr::UNINIT_HASH, |dest| unsafe {
            ptr::copy_nonoverlapping(x.as_ptr(), dest, x.len())
        })
    }

    fn iter_chunk<'a>(chunk: &'a [MaybeUninit<u8>], mut f: impl FnMut(&'a AValueHeader)) {
        unsafe {
            // We only allocate trait ptr then a payload immediately after
            // so find the first trait ptr, see how big it is, and keep skipping.
            let mut p = chunk.as_ptr();
            let end = chunk.as_ptr().add(chunk.len());
            while p < end {
                let or_forward = &*(p as *const AValueOrForward);
                let n = match or_forward.unpack() {
                    Either::Left(ptr) => {
                        f(&or_forward.header);
                        ptr.unpack().memory_size()
                    }
                    Either::Right(forward) => {
                        // Overwritten, so the next word will be the size of the memory
                        forward.object_size
                    }
                };
                let n = mem::size_of::<AValueHeader>() + n;
                let n = cmp::max(n, MIN_ALLOC);
                p = p.add(n);
                // We know the alignment requirements will never be greater than AValuePtr
                // since we check that in allocate_empty
                p = p.add(p.align_offset(mem::align_of::<AValueHeader>()));
            }
        }
    }

    // Iterate over the values in the heap in the order they
    // were added.
    pub fn for_each_ordered<'a>(&'a mut self, mut f: impl FnMut(&'a AValueHeader)) {
        // We get the chunks from most newest to oldest as per the bumpalo spec.
        // And within each chunk, the values are filled newest to oldest.
        // So need to do two sets of reversing.
        for bump in [&mut self.drop, &mut self.non_drop] {
            let chunks = bump.iter_allocated_chunks().collect::<Vec<_>>();
            // Use a single buffer to reduce allocations, but clear it after use
            let mut buffer = Vec::new();
            for chunk in chunks.iter().rev() {
                Self::iter_chunk(chunk, |x| buffer.push(x));
                buffer.iter().rev().for_each(|x| f(*x));
                buffer.clear();
            }
        }
    }

    // Iterate over the values in the drop bump in any order
    pub fn for_each_drop_unordered<'a>(&'a mut self, mut f: impl FnMut(&'a AValueHeader)) {
        self.drop
            .iter_allocated_chunks()
            .for_each(|chunk| Self::iter_chunk(chunk, &mut f))
    }

    // For each Rust-level type (the String) report how many entries there are in the heap, and how much size they consume
    pub fn allocated_summary(&self) -> HeapSummary {
        fn for_each<'a>(bump: &'a Bump, mut f: impl FnMut(&'a AValueHeader)) {
            // SAFE: We're consuming the iterator immediately and not allocating from the arena during.
            unsafe {
                bump.iter_allocated_chunks_raw().for_each(|(data, len)| {
                    Arena::iter_chunk(slice::from_raw_parts(data as *const _, len), &mut f)
                })
            }
        }

        // Record how many times each header occurs
        // We deliberately hash by the AValueHeader for higher performance, less type lookup
        let mut entries: HashMap<AValueHeader, (&'static str, (usize, usize))> = HashMap::new();
        let mut f = |x: &AValueHeader| {
            let v = x.unpack();
            let e = entries
                .entry(x.dupe())
                .or_insert_with(|| (v.get_type(), (0, 0)));
            e.1.0 += 1;
            e.1.1 += v.total_memory()
        };
        for_each(&self.drop, &mut f);
        for_each(&self.non_drop, &mut f);

        // For a given type, the AValueHeader isn't always unique
        // (if they get compiled in different translation units),
        // so not just a simple map.
        let mut summary = HashMap::new();
        for (_, (name, (count, memory))) in entries {
            let v = summary.entry(name.to_owned()).or_insert((0, 0));
            v.0 += count;
            v.1 += memory;
        }
        HeapSummary { summary }
    }
}

impl AValueHeader {
    pub(crate) fn new<'v, T: AValue<'v>>() -> AValueHeader {
        let header = AValueHeader::new_const::<T>();

        let vtable_ptr = header.0 as *const AValueVTable as usize;
        // Check that the LSB is not set, as we reuse that for overwrite
        debug_assert!(vtable_ptr & 1 == 0);

        header
    }

    pub(crate) const fn new_const<'v, T: AValue<'v>>() -> AValueHeader {
        AValueHeader(AValueVTable::new::<T>())
    }

    #[inline]
    pub(crate) fn payload_ptr(&self) -> *const () {
        let self_repr = self as *const AValueHeader as *const AValueRepr<()>;
        unsafe { &(*self_repr).payload }
    }

    pub(crate) unsafe fn payload<'v, T: StarlarkValue<'v>>(&self) -> &T {
        debug_assert_eq!(self.unpack().static_type_of_value(), T::static_type_id());
        &*(self.payload_ptr() as *const T)
    }

    pub(crate) fn unpack<'v>(&'v self) -> AValueDyn<'v> {
        unsafe {
            // TODO: this assertion does not belong here.
            //   Instead, `Value` should be a `Pointer<AValueOrForward>`
            //   instead of `Pointer<AValueHeader>`,
            //   and assertion should be where we unpack the pointer.
            debug_assert!(
                !(*(self as *const AValueHeader as *const AValueOrForward)).is_forward(),
                "value is a forward pointer; value cannot be unpacked during GC or freeze"
            );
        }
        unsafe {
            AValueDyn {
                value: &*self.payload_ptr(),
                vtable: self.0,
            }
        }
    }

    /// Unpack something that might have been overwritten.
    // TODO(nga): this function does not belong here, it should accept `AValueOrForward`.
    pub(crate) fn unpack_overwrite<'v>(&'v self) -> Either<usize, AValueDyn<'v>> {
        let x = unsafe { &*(self as *const AValueHeader as *const AValueOrForward) };
        match x.unpack() {
            Either::Left(header) => Either::Right(header.unpack()),
            Either::Right(forward) => Either::Left(forward.forward_ptr()),
        }
    }

    /// After performing the overwrite any existing pointers to this value
    /// are corrupted.
    pub unsafe fn overwrite_with_forward<'v, T: AValue<'v>>(
        me: *mut AValueRepr<T>,
        forward_ptr: usize,
    ) -> T {
        assert!(forward_ptr & 1 == 0, "Can't have the lowest bit set");

        // TODO(nga): we don't need to do virtual call to obtain memory size
        let sz = (*me).header.unpack().memory_size();
        let p = me as *const AValueRepr<T>;
        let res = ptr::read(p).payload;
        let p = me as *mut AValueForward;
        *p = AValueForward {
            forward_ptr: forward_ptr | 1,
            object_size: sz,
        };
        res
    }

    /// Cast header pointer to repr pointer.
    #[inline]
    pub(crate) unsafe fn as_repr<'v, A: AValue<'v>>(&self) -> &AValueRepr<A> {
        debug_assert_eq!(
            A::StarlarkValue::static_type_id(),
            self.unpack().static_type_of_value()
        );
        &*(self as *const AValueHeader as *const AValueRepr<A>)
    }
}

impl<T> AValueRepr<T> {
    pub(crate) const fn with_metadata(
        metadata: &'static AValueVTable,
        payload: T,
    ) -> AValueRepr<T> {
        AValueRepr {
            header: AValueHeader(metadata),
            payload,
        }
    }

    fn assert_no_padding_between_header_and_payload() {
        // We can make it work when there's padding, but we don't need to,
        // and for now just make an assertion.
        assert!(memoffset::offset_of!(Self, payload) == mem::size_of::<AValueHeader>());
    }

    /// Offset of value extra content relative to `AValueRepr` start.
    pub(crate) fn offset_of_extra<'v>() -> usize
    where
        T: AValue<'v>,
    {
        Self::assert_no_padding_between_header_and_payload();

        mem::size_of::<AValueHeader>() + T::offset_of_extra()
    }

    pub(crate) fn from_payload_ptr_mut(payload_ptr: *mut T) -> *mut AValueRepr<T> {
        let payload_ptr = payload_ptr as usize;
        let header_ptr = payload_ptr - mem::size_of::<AValueHeader>();
        header_ptr as *mut AValueRepr<T>
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        self.for_each_drop_unordered(|x| {
            // Safe to convert to *mut because we are the only owner
            let value = x.payload_ptr() as *mut ();
            x.0.drop_in_place(value);
        });
        self.non_drop.reset();
        self.drop.reset();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::values::any::StarlarkAny;
    use crate::values::layout::avalue::simple;

    fn to_repr(x: &AValueHeader) -> String {
        let mut s = String::new();
        x.unpack().collect_repr(&mut s);
        s
    }

    fn mk_str(x: &str) -> impl AValue<'static, ExtraElem = ()> {
        simple(StarlarkAny::new(x.to_owned()))
    }

    fn reserve_str<'v, T: AValue<'static>>(arena: &'v Arena, _: &T) -> Reservation<'v, 'static, T> {
        arena.reserve_with_extra::<T>(0).0
    }

    #[test]
    fn test_trait_arena_iteration() {
        // We want iteration to proceed in the same order as allocation,
        // otherwise profiling won't work
        const LIMIT: usize = 10000;
        let mut arena = Arena::default();
        let mut reserved = Vec::new();
        for i in 0..LIMIT {
            if i % 100 == 0 {
                let r = reserve_str(&arena, &mk_str(""));
                reserved.push((r, i));
            } else {
                arena.alloc(mk_str(&i.to_string()));
            }
        }
        assert!(!reserved.is_empty());
        for (r, i) in reserved {
            r.fill(mk_str(&i.to_string()));
        }

        // Not a functional part of the test, just makes sure we go through
        // the interesting cases (last time 56 was sufficient, so 10K is plenty of margin of error)
        assert!(
            arena.drop.iter_allocated_chunks().count() > 1,
            "Didn't allocate enough to test properly"
        );
        let mut j = 0;
        arena.for_each_ordered(|i| {
            assert_eq!(to_repr(i), j.to_string());
            j += 1;
        });
        assert_eq!(j, LIMIT);
        j = 0;
        arena.for_each_drop_unordered(|_| j += 1);
        assert_eq!(j, LIMIT);
    }

    #[test]
    // Make sure that even if there are some blackholes when we drop, we can still walk to heap
    fn drop_with_blackhole() {
        let mut arena = Arena::default();
        arena.alloc(mk_str("test"));
        // reserve but do not fill!
        reserve_str(&arena, &mk_str(""));
        arena.alloc(mk_str("hello"));
        let mut res = Vec::new();
        arena.for_each_ordered(|x| res.push(x));
        assert_eq!(res.len(), 3);
        assert_eq!(to_repr(res[0]), "test");
        assert_eq!(to_repr(res[2]), "hello");
    }

    #[test]
    fn test_allocated_summary() {
        let arena = Arena::default();
        arena.alloc(mk_str("test"));
        arena.alloc(mk_str("test"));
        let res = arena.allocated_summary().summary;
        assert_eq!(res.len(), 1);
        let entry = res.values().next().unwrap();
        assert_eq!(entry.0, 2);
        assert_eq!(entry.1, arena.allocated_bytes());
    }
}
