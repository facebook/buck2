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

use std::{
    alloc::{alloc, dealloc, Layout},
    cell::{Cell, RefCell},
    intrinsics::{likely, unlikely},
    mem,
    mem::MaybeUninit,
    ptr, slice,
};

/// We'd love to use the real `alloca`, but don't want to blow through the stack space,
/// so define our own wrapper.
/// We use a single continuous buffer. When it needs upgrading, we double it and keep the old one around.
pub(crate) struct Alloca {
    // An alternative design would be to bake the <T> into the type, so all allocations are of the same type.
    // Benchmarking that, even if the most optimistic scenario (just reallocating a single element)
    // the performance difference is only 2%, so keep the flexibility of not needing to predeclare the type.
    alloc: Cell<*mut usize>,
    end: Cell<*mut usize>,
    last_size_words: Cell<usize>,
    buffers: RefCell<Vec<(*mut usize, Layout)>>,
}

impl Drop for Alloca {
    fn drop(&mut self) {
        for (ptr, layout) in self.buffers.borrow_mut().drain(0..) {
            unsafe { dealloc(ptr as *mut u8, layout) }
        }
    }
}

const INITIAL_SIZE: usize = 1000000; // ~ 1Mb

impl Alloca {
    pub fn new() -> Self {
        Self::with_capacity(INITIAL_SIZE)
    }

    pub fn with_capacity(size_bytes: usize) -> Self {
        let size_words = (size_bytes + (mem::size_of::<usize>() - 1)) / mem::size_of::<usize>();
        let layout = Layout::array::<usize>(size_words).unwrap();
        let pointer = unsafe { alloc(layout) as *mut usize };
        Self {
            alloc: Cell::new(pointer),
            end: Cell::new(pointer.wrapping_add(size_words)),
            last_size_words: Cell::new(size_words),
            buffers: RefCell::new(vec![(pointer, layout)]),
        }
    }

    fn assert_state(&self) {
        unsafe {
            debug_assert!(self.end.get().offset_from(self.alloc.get()) >= 0);
            debug_assert!(
                self.end.get().offset_from(self.alloc.get()) as usize <= self.last_size_words.get()
            );
        }
    }

    #[cold]
    #[inline(never)]
    fn allocate_more(&self, len: usize, one: Layout) {
        let want =
            Layout::from_size_align(one.size().checked_mul(len).unwrap(), one.align()).unwrap();
        assert!(want.size() % mem::size_of::<usize>() == 0);
        assert!(want.align() % mem::size_of::<usize>() == 0);
        let size_words = self.last_size_words.get() * 2 + want.size() / mem::size_of::<usize>();
        let layout = Layout::array::<usize>(size_words).unwrap();
        let pointer = unsafe { alloc(layout) as *mut usize };
        self.buffers.borrow_mut().push((pointer, layout));
        self.alloc.set(pointer);
        self.last_size_words.set(size_words);
        self.end.set(pointer.wrapping_add(size_words));
    }

    /// Note that the `Drop` for the `T` will not be called. That's safe if there is no `Drop`,
    /// or you call it yourself.
    #[inline(always)]
    pub fn alloca_uninit<T, R>(&self, len: usize, k: impl FnOnce(&mut [MaybeUninit<T>]) -> R) -> R {
        self.assert_state();

        assert_eq!(mem::size_of::<T>() % mem::size_of::<usize>(), 0);
        assert_eq!(mem::align_of::<T>(), mem::size_of::<usize>());

        let mut start = self.alloc.get();

        let rem_words = unsafe { self.end.get().offset_from(start) as usize };
        let rem_in_t = rem_words * mem::size_of::<usize>() / mem::size_of::<T>();
        if unlikely(len > rem_in_t) {
            self.allocate_more(len, Layout::new::<T>());
            start = self.alloc.get();
        }

        // Multiplication won't overflow, `allocate_more` checked that.
        let size_words = mem::size_of::<T>() * len / mem::size_of::<usize>();

        let stop = start.wrapping_add(size_words);
        let old = start;
        self.alloc.set(stop);
        let data = start as *mut MaybeUninit<T>;
        let slice = unsafe { slice::from_raw_parts_mut(data, len) };
        let res = k(slice);

        // If the pointer changed, it means a callback called alloca again,
        // which allocated a new buffer. So we are abandoning the current allocation here,
        // and new allocations will use the new buffer even if the current buffer has space.
        if likely(self.alloc.get() == stop) {
            self.alloc.set(old);
        }

        self.assert_state();

        res
    }

    #[inline(always)]
    pub fn alloca_init<T, R>(
        &self,
        len: usize,
        mut init: impl FnMut() -> T,
        k: impl FnOnce(&mut [T]) -> R,
    ) -> R {
        self.alloca_uninit(len, |data| {
            for x in data.iter_mut() {
                x.write(init());
            }
            let data = unsafe { MaybeUninit::slice_assume_init_mut(data) };
            k(data)
        })
    }

    #[allow(dead_code)] // Dead, but morally a sensible API to provide, and useful for testing
    #[inline(always)]
    pub fn alloca_fill<T: Copy, R>(&self, len: usize, fill: T, k: impl FnOnce(&mut [T]) -> R) -> R {
        self.alloca_init(len, || fill, k)
    }

    fn alloca_concat_slow<T: Clone, R, F>(&self, x: &[T], y: &[T], k: F) -> R
    where
        F: FnOnce(&[T]) -> R,
    {
        // We clone the input into a slice allocated in `Alloca`,
        // slice does not invoke `Drop`, so we do drop explicitly.
        struct DropSliceGuard<A>(*mut [A]);
        impl<A> Drop for DropSliceGuard<A> {
            fn drop(&mut self) {
                unsafe {
                    ptr::drop_in_place(self.0);
                }
            }
        }

        self.alloca_uninit(x.len() + y.len(), |xy| {
            let (x_uninit, y_uninit) = xy.split_at_mut(x.len());
            let x = MaybeUninit::write_slice_cloned(x_uninit, x);
            let _x_drop_guard = DropSliceGuard(x);
            let y = MaybeUninit::write_slice_cloned(y_uninit, y);
            let _y_drop_guard = DropSliceGuard(y);
            let xy = unsafe { MaybeUninit::slice_assume_init_mut(xy) };
            k(xy)
        })
    }

    /// Concat two slices and invoke the callback with the result.
    /// Use either slice as is if the other is empty,
    /// otherwise clone the elements into a temporary slice.
    #[inline(always)]
    pub(crate) fn alloca_concat<T: Clone, R, F>(&self, x: &[T], y: &[T], k: F) -> R
    where
        F: FnOnce(&[T]) -> R,
    {
        if x.is_empty() {
            k(y)
        } else if y.is_empty() {
            k(x)
        } else {
            self.alloca_concat_slow(x, y, k)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_alloca() {
        // Use a small capacity to encourage overflow behaviour
        let a = Alloca::with_capacity(100);
        a.alloca_fill(3, 8usize, |xs| {
            xs[0] = 5;
            xs[2] = xs[0] + xs[1] + 15;
            a.alloca_fill(200, 18usize, |ys| {
                assert_eq!(xs[2], 8 + 5 + 15);
                assert_eq!(ys[0], 18);
                assert_eq!(ys[200 - 1], 18);
            });
            a.alloca_fill(3, 1u64, |_| {});
            assert_eq!(xs[2], 8 + 5 + 15);
        })
    }

    #[test]
    fn trigger_bug() {
        let a = Alloca::with_capacity(100);
        for _ in 0..100 {
            a.alloca_fill(10, 17usize, |_| {
                a.alloca_fill(1000, 19usize, |_| {});
            });
        }

        assert_eq!(2, a.buffers.borrow().len());
    }

    #[test]
    fn test_alloca_concat() {
        let a = Alloca::new();
        let x = vec!["ab".to_owned()];
        let y = vec!["cd".to_owned()];
        a.alloca_concat(&x, &[], |xy| {
            // No copy here.
            assert_eq!(xy.as_ptr(), x.as_ptr());
        });
        a.alloca_concat(&[], &x, |xy| {
            // No copy here.
            assert_eq!(xy.as_ptr(), x.as_ptr());
        });
        a.alloca_concat(&x, &y, |xy| {
            assert_eq!(&["ab".to_owned(), "cd".to_owned()], xy);
        });
    }
}
