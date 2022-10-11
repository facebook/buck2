//! Array multiple elements constructor syntax.
//!
//! While Rust does provide those, they require copy, and you cannot obtain the
//! index that will be created. This crate provides syntax that fixes both of
//! those issues.
//!
//! # Examples
//!
//! ```
//! # #[macro_use]
//! # extern crate array_macro;
//! # fn main() {
//! assert_eq!(array![String::from("x"); 2], [String::from("x"), String::from("x")]);
//! assert_eq!(array![|x| x; 3], [0, 1, 2]);
//! # }
//! ```

#![no_std]

#[doc(hidden)]
pub extern crate core as __core;

#[allow(unused_imports)]
use core::mem::MaybeUninit; // Rust 1.36+ required

#[doc(hidden)]
pub struct __ArrayVec<T> {
    start: *mut T,
    length: usize,
}

impl<T> __ArrayVec<T> {
    pub fn new(start: *mut T) -> Self {
        Self { start, length: 0 }
    }

    pub fn start(&self) -> *mut T {
        self.start
    }

    pub fn length(&mut self) -> *mut usize {
        &mut self.length
    }
}

impl<T> Drop for __ArrayVec<T> {
    fn drop(&mut self) {
        for i in 0..self.length {
            unsafe {
                core::ptr::drop_in_place(self.start.add(i));
            }
        }
    }
}

/// Array constructor macro.
///
/// This macro provides a way to repeat the same macro element multiple times
/// without requiring `Copy` implementation.
///
/// It's possible to define a callback by starting expression with `|` or `move`. As
/// every closure is it own unique type, it is not possible to have an array of
/// closures, so this syntax was reused for creating arrays with known indexes.
///
/// # Examples
///
/// ```
/// # #[macro_use]
/// # extern crate array_macro;
/// # fn main() {
/// assert_eq!(array!["string"; 3], ["string", "string", "string"]);
/// assert_eq!(array![|x| x; 3], [0, 1, 2]);
/// # }
/// ```
#[macro_export(local_inner_macros)]
macro_rules! array {
    [@INTERNAL $callback:expr; $count:expr] => {{
        const COUNT: usize = $count;
        #[allow(unsafe_code)]
        fn create_arr<T>(mut callback: impl FnMut(usize) -> T) -> [T; COUNT] {
            let mut arr = $crate::__core::mem::MaybeUninit::uninit();
            let mut vec = $crate::__ArrayVec::<T>::new((&mut arr).as_mut_ptr() as *mut T);
            unsafe {
                // Loop invariant: vec[..vec.length] is valid
                for i in 0..COUNT {
                    // On the first iteration the value of `i` is `0`, making this a no-op.
                    //
                    // We don't need to store length for the last iteration as `vec` is
                    // forgotten after leaving this loop.
                    //
                    // The value is set before writing the value to avoid need to perform
                    // addition by 1.
                    *(&mut vec).length() = i;
                    $crate::__core::ptr::write((&vec).start().add(i), callback(i));
                }
                // Loop escaped without panicking, avoid dropping elements.
                $crate::__core::mem::forget(vec);
                // All elements were written, assuming array is valid.
                arr.assume_init()
            }
        }
        create_arr($callback)
    }};
    [| $($rest:tt)*] => {
        array![@INTERNAL | $($rest)*]
    };
    [move $($rest:tt)*] => {
        array![@INTERNAL move $($rest)*]
    };
    [$expr:expr; $count:expr] => {
        array![|_| $expr; $count]
    };
}
