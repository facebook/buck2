// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

//! # Smart String
//!
//! [`SmartString`] is a wrapper around [`String`] which offers
//! automatic inlining of small strings. It comes in two flavours:
//! [`LazyCompact`], which takes up exactly as much space as a [`String`]
//! and is generally a little faster, and [`Compact`], which is the same as
//! [`LazyCompact`] except it will aggressively re-inline any expanded
//! [`String`]s which become short enough to do so.
//! [`LazyCompact`] is the default.
//!
//! ## What Is It For?
//!
//! The intended use for [`SmartString`] is as a key type for a
//! B-tree (such as [`std::collections::BTreeMap`]) or any kind of
//! array operation where cache locality is critical.
//!
//! In general, it's a nice data type for reducing your heap allocations and
//! increasing the locality of string data. If you use [`SmartString`]
//! as a drop-in replacement for [`String`], you're almost certain to see
//! a slight performance boost, as well as slightly reduced memory usage.
//!
//! ## How To Use It?
//!
//! Before using [`SmartString`], you should always call
//! [`smartstring::validate()`][validate], to make sure [`SmartString`] is safe to use.
//! Ideally, you should call it at the start of your `main()` function, and
//! from your test suite. [`SmartString`] will also attempt to crash early if
//! inconsistencies are detected, but this isn't foolproof.
//!
//! ```rust
//! fn main() {
//!     smartstring::validate();
//! }
//! ```
//!
//! [`SmartString`] has the exact same API as [`String`],
//! all the clever bits happen automatically behind the scenes, so you could just:
//!
//! ```rust
//! use smartstring::alias::String;
//! use std::fmt::Write;
//!
//! let mut string = String::new();
//! string.push_str("This is just a string!");
//! string.clear();
//! write!(string, "Hello Joe!");
//! assert_eq!("Hello Joe!", string);
//! ```
//!
//! ## Give Me The Details
//!
//! [`SmartString`] is the same size as [`String`] and
//! relies on pointer alignment to be able to store a discriminant bit in its
//! inline form that will never be present in its [`String`] form, thus
//! giving us 24 bytes (on 64-bit architectures) minus one bit to encode our
//! inline string. It uses 23 bytes to store the string data and the remaining
//! 7 bits to encode the string's length. When the available space is exceeded,
//! it swaps itself out with a [`String`] containing its previous
//! contents. Likewise, if the string's length should drop below its inline
//! capacity again, it deallocates the string and moves its contents inline.
//!
//! Given that we use the knowledge that a certain bit in the memory layout
//! of [`String`] will always be unset as a discriminant, you would be
//! able to call [`std::mem::transmute::<String>()`][std::mem::transmute] on a boxed
//! smart string and start using it as a normal [`String`] immediately -
//! there's no pointer tagging or similar trickery going on here.
//! (But please don't do that, there's an efficient [`Into<String>`][IntoString]
//! implementation that does the exact same thing with no need to go unsafe
//! in your own code.)
//!
//! In [`Compact`] mode, it is aggressive about inlining strings, meaning that if you modify a heap allocated
//! string such that it becomes short enough for inlining, it will be inlined immediately
//! and the allocated [`String`] will be dropped. This may cause multiple
//! unintended allocations if you repeatedly adjust your string's length across the
//! inline capacity threshold, so if your string's construction can get
//! complicated and you're relying on performance during construction, it might be better
//! to construct it as a [`String`] and convert it once construction is done.
//!
//! [`LazyCompact`] looks the same as [`Compact`], except
//! it never re-inlines a string that's already been heap allocated, instead
//! keeping the allocation around in case it needs it. This makes for less
//! cache local strings, but is the best choice if you're more worried about
//! time spent on unnecessary allocations than cache locality.
//!
//! ## Performance
//!
//! It doesn't aim to be more performant than [`String`] in the general case,
//! except that it doesn't trigger heap allocations for anything shorter than
//! its inline capacity and so can be reasonably expected to exceed
//! [`String`]'s performance perceptibly on shorter strings, as well as being more
//! memory efficient in these cases. There will always be a slight overhead on all
//! operations on boxed strings, compared to [`String`].
//!
//! ## Caveat
//!
//! The way `smartstring` gets by without a discriminant is dependent on the memory layout of the
//! [`std::string::String`] struct, which isn't something the Rust compiler and standard library make any
//! guarantees about. `smartstring` makes an assumption about how it's been laid out, which has held
//! basically since rustc came into existence, but is nonetheless not a safe assumption to make, and if
//! the layout ever changes, `smartstring` will stop working properly (at least on little-endian
//! architectures, the assumptions made on big-endian archs will hold regardless of the actual memory
//! layout). Its test suite does comprehensive validation of these assumptions, and as long as the
//! [CI build](https://travis-ci.org/github/bodil/smartstring) is passing for any given rustc version,
//! you can be sure it will do its job properly on all tested architectures. More directly, as mentioned
//! above, you should always call [`smartstring::validate()`][validate] before using [`SmartString`] to
//! remove any doubt.
//!
//! As an extra precaution, some runtime checks are made as well, so that if the memory layout
//! assumption no longer holds, `smartstring` will not work correctly, but there should be no security
//! implications and it should crash early.
//!
//! ## Feature Flags
//!
//! `smartstring` comes with optional support for the following crates through Cargo
//! feature flags. You can enable them in your `Cargo.toml` file like this:
//!
//! ```no_compile
//! [dependencies]
//! smartstring = { version = "*", features = ["proptest", "serde"] }
//! ```
//!
//! | Feature | Description |
//! | ------- | ----------- |
//! | [`arbitrary`](https://crates.io/crates/arbitrary) | [`Arbitrary`][Arbitrary] implementation for [`SmartString`]. |
//! | [`proptest`](https://crates.io/crates/proptest) | A strategy for generating [`SmartString`]s from a regular expression. |
//! | [`serde`](https://crates.io/crates/serde) | [`Serialize`][Serialize] and [`Deserialize`][Deserialize] implementations for [`SmartString`]. |
//!
//! [IntoString]: struct.SmartString.html#impl-Into%3CString%3E
//! [serde]: https://crates.io/crates/serde
//! [Serialize]: https://docs.rs/serde/latest/serde/trait.Serialize.html
//! [Deserialize]: https://docs.rs/serde/latest/serde/trait.Deserialize.html
//! [Arbitrary]: https://docs.rs/arbitrary/latest/arbitrary/trait.Arbitrary.html

#![forbid(rust_2018_idioms)]
#![deny(nonstandard_style)]
#![warn(unreachable_pub, missing_debug_implementations, missing_docs)]
#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

use alloc::{
    boxed::Box,
    string::{String, ToString},
};
use core::{
    borrow::{Borrow, BorrowMut},
    cmp::Ordering,
    convert::Infallible,
    fmt::{Debug, Display, Error, Formatter, Write},
    hash::{Hash, Hasher},
    iter::FromIterator,
    marker::PhantomData,
    mem::{forget, MaybeUninit},
    ops::{
        Add, Bound, Deref, DerefMut, Index, IndexMut, Range, RangeBounds, RangeFrom, RangeFull,
        RangeInclusive, RangeTo, RangeToInclusive,
    },
    ptr::drop_in_place,
    str::FromStr,
};

#[cfg(feature = "std")]
use std::borrow::Cow;

mod config;
pub use config::{Compact, LazyCompact, SmartStringMode, MAX_INLINE};

mod marker_byte;
use marker_byte::{Discriminant, Marker};

mod inline;
use inline::InlineString;

mod boxed;
use boxed::BoxedString;

mod casts;
use casts::{StringCast, StringCastInto, StringCastMut};

mod iter;
pub use iter::Drain;

#[cfg(feature = "serde")]
mod serde;

#[cfg(feature = "arbitrary")]
mod arbitrary;

#[cfg(feature = "proptest")]
pub mod proptest;

/// Validate the crate's assumptions about [`String`] memory layout.
///
/// Because [`SmartString`] makes some assumptions about how [`String`] is
/// laid out in memory that rustc does not actually guarantee, you should
/// always run this function in your target environment before using
/// [`SmartString`], ideally at the start of your application's `main()`
/// function, so your application will crash as early as possible without
/// any security risks. You should also run it as part of your application's
/// test suite, to catch problems before runtime.
///
/// If the assumptions don't hold, this function will panic.
///
/// # Example
///
/// ```
/// fn main() {
///     smartstring::validate();
/// }
/// ```
pub fn validate() {
    let mut s = String::with_capacity(5);
    s.push_str("lol");
    assert_eq!(3, s.len(), "SmartString memory layout check failed");
    assert_eq!(5, s.capacity(), "SmartString memory layout check failed");
    let ptr: *const String = &s;
    let ptr: *const usize = ptr.cast();
    let first_bytes = unsafe { *ptr };
    assert_ne!(3, first_bytes, "SmartString memory layout check failed");
    assert_ne!(5, first_bytes, "SmartString memory layout check failed");
    let first_byte = unsafe { *(ptr as *const u8) };
    #[cfg(target_endian = "little")]
    assert_eq!(
        0,
        first_byte & 0x01,
        "SmartString memory layout check failed"
    );
    #[cfg(target_endian = "big")]
    assert_eq!(
        0,
        first_byte & 0x80,
        "SmartString memory layout check failed"
    );
}

/// Convenient type aliases.
pub mod alias {
    use super::*;

    /// A convenience alias for a [`LazyCompact`] layout [`SmartString`].
    ///
    /// Just pretend it's a [`String`][String]!
    pub type String = SmartString<LazyCompact>;

    /// A convenience alias for a [`Compact`] layout [`SmartString`].
    pub type CompactString = SmartString<Compact>;
}

/// A smart string.
///
/// This wraps one of two string types: an inline string or a boxed string.
/// Conversion between the two happens opportunistically and transparently.
///
/// It takes a layout as its type argument: one of [`Compact`] or [`LazyCompact`].
///
/// It mimics the interface of [`String`] except where behaviour cannot
/// be guaranteed to stay consistent between its boxed and inline states. This means
/// you still have `capacity()` and `shrink_to_fit()`, relating to state that only
/// really exists in the boxed variant, because the inline variant can still give
/// sensible behaviour for these operations, but `with_capacity()`, `reserve()` etc are
/// absent, because they would have no effect on inline strings and the requested
/// state changes wouldn't carry over if the inline string is promoted to a boxed
/// one - not without also storing that state in the inline representation, which
/// would waste precious bytes for inline string data.
#[repr(C)]
#[cfg_attr(target_pointer_width = "64", repr(align(8)))]
#[cfg_attr(target_pointer_width = "32", repr(align(4)))]
pub struct SmartString<Mode: SmartStringMode> {
    data: MaybeUninit<InlineString>,
    mode: PhantomData<Mode>,
}

impl<Mode: SmartStringMode> Drop for SmartString<Mode> {
    fn drop(&mut self) {
        if let StringCastMut::Boxed(string) = self.cast_mut() {
            unsafe { drop_in_place(string) };
        }
    }
}

impl<Mode: SmartStringMode> Clone for SmartString<Mode> {
    /// Clone a [`SmartString`].
    ///
    /// If the string is inlined, this is a [`Copy`] operation. Otherwise,
    /// [`String::clone()`][String::clone] is invoked.
    fn clone(&self) -> Self {
        match self.cast() {
            StringCast::Boxed(string) => Self::from_boxed(string.string().clone().into()),
            StringCast::Inline(string) => Self::from_inline(*string),
        }
    }
}

impl<Mode: SmartStringMode> Deref for SmartString<Mode> {
    type Target = str;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        match self.cast() {
            StringCast::Boxed(string) => string.string().as_str(),
            StringCast::Inline(string) => string.as_str(),
        }
    }
}

impl<Mode: SmartStringMode> DerefMut for SmartString<Mode> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self.cast_mut() {
            StringCastMut::Boxed(string) => string.string_mut().as_mut_str(),
            StringCastMut::Inline(string) => string.as_mut_str(),
        }
    }
}

impl SmartString<LazyCompact> {
    /// Construct an empty string.
    ///
    /// This is a `const fn` version of [`SmartString::new`].
    /// It's a temporary measure while we wait for trait bounds on
    /// type arguments to `const fn`s to stabilise, and will be deprecated
    /// once this happens.
    pub const fn new_const() -> Self {
        Self {
            data: MaybeUninit::new(InlineString::new()),
            mode: PhantomData,
        }
    }
}

impl SmartString<Compact> {
    /// Construct an empty string.
    ///
    /// This is a `const fn` version of [`SmartString::new`].
    /// It's a temporary measure while we wait for trait bounds on
    /// type arguments to `const fn`s to stabilise, and will be deprecated
    /// once this happens.
    pub const fn new_const() -> Self {
        Self {
            data: MaybeUninit::new(InlineString::new()),
            mode: PhantomData,
        }
    }
}

impl<Mode: SmartStringMode> SmartString<Mode> {
    /// Construct an empty string.
    #[inline(always)]
    pub fn new() -> Self {
        Self::from_inline(InlineString::new())
    }

    fn from_boxed(boxed: Mode::BoxedString) -> Self {
        let mut out = Self {
            data: MaybeUninit::uninit(),
            mode: PhantomData,
        };
        let data_ptr: *mut Mode::BoxedString = out.data.as_mut_ptr().cast();
        unsafe { data_ptr.write(boxed) };
        out
    }

    fn from_inline(inline: InlineString) -> Self {
        Self {
            data: MaybeUninit::new(inline),
            mode: PhantomData,
        }
    }

    fn discriminant(&self) -> Discriminant {
        let ptr: *const Marker = self.data.as_ptr().cast();
        unsafe { *ptr }.discriminant()
    }

    fn cast(&self) -> StringCast<'_, Mode> {
        match self.discriminant() {
            Discriminant::Inline => StringCast::Inline(unsafe { &*self.data.as_ptr() }),
            Discriminant::Boxed => StringCast::Boxed(unsafe { &*self.data.as_ptr().cast() }),
        }
    }

    fn cast_mut(&mut self) -> StringCastMut<'_, Mode> {
        match self.discriminant() {
            Discriminant::Inline => StringCastMut::Inline(unsafe { &mut *self.data.as_mut_ptr() }),
            Discriminant::Boxed => {
                StringCastMut::Boxed(unsafe { &mut *self.data.as_mut_ptr().cast() })
            }
        }
    }

    fn cast_into(mut self) -> StringCastInto<Mode> {
        match self.discriminant() {
            Discriminant::Inline => StringCastInto::Inline(unsafe { self.data.assume_init() }),
            Discriminant::Boxed => StringCastInto::Boxed(unsafe {
                let boxed_ptr: *mut Mode::BoxedString = self.data.as_mut_ptr().cast();
                let string = boxed_ptr.read();
                forget(self);
                string
            }),
        }
    }

    fn promote_from(&mut self, string: String) {
        debug_assert!(self.discriminant() == Discriminant::Inline);
        let string: Mode::BoxedString = string.into();
        let data: *mut Mode::BoxedString = self.data.as_mut_ptr().cast();
        unsafe { data.write(string) };
    }

    /// Attempt to inline the string if it's currently heap allocated.
    ///
    /// Returns the resulting state: `true` if it's inlined, `false` if it's not.
    fn try_demote(&mut self) -> bool {
        if Mode::DEALLOC {
            self.really_try_demote()
        } else {
            false
        }
    }

    /// Attempt to inline the string regardless of whether `Mode::DEALLOC` is set.
    fn really_try_demote(&mut self) -> bool {
        if let StringCastMut::Boxed(string) = self.cast_mut() {
            if string.len() > MAX_INLINE {
                false
            } else {
                let inlined = string.string().as_bytes().into();
                unsafe {
                    drop_in_place(string);
                    let data = &mut self.data.as_mut_ptr();
                    data.write(inlined);
                }
                true
            }
        } else {
            true
        }
    }

    /// Return the length in bytes of the string.
    ///
    /// Note that this may differ from the length in `char`s.
    pub fn len(&self) -> usize {
        match self.cast() {
            StringCast::Boxed(string) => string.len(),
            StringCast::Inline(string) => string.len(),
        }
    }

    /// Test whether the string is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Test whether the string is currently inlined.
    pub fn is_inline(&self) -> bool {
        self.discriminant() == Discriminant::Inline
    }

    /// Get a reference to the string as a string slice.
    pub fn as_str(&self) -> &str {
        self.deref()
    }

    /// Get a reference to the string as a mutable string slice.
    pub fn as_mut_str(&mut self) -> &mut str {
        self.deref_mut()
    }

    /// Push a character to the end of the string.
    pub fn push(&mut self, ch: char) {
        match self.cast_mut() {
            StringCastMut::Boxed(string) => string.string_mut().push(ch),
            StringCastMut::Inline(string) => {
                let len = string.len();
                let new_len = len + ch.len_utf8();
                if new_len > MAX_INLINE {
                    let mut new_str = String::with_capacity(new_len);
                    new_str.push_str(string.as_str());
                    new_str.push(ch);
                    self.promote_from(new_str);
                } else {
                    let written = ch.encode_utf8(&mut string.as_mut_slice()[len..]).len();
                    string.set_size(len + written);
                }
            }
        }
    }

    /// Copy a string slice onto the end of the string.
    pub fn push_str(&mut self, string: &str) {
        let len = self.len();
        match self.cast_mut() {
            StringCastMut::Boxed(this) => this.string_mut().push_str(string),
            StringCastMut::Inline(this) => {
                let new_len = len + string.len();
                if new_len > MAX_INLINE {
                    let mut new_str = String::with_capacity(new_len);
                    new_str.push_str(this.as_str());
                    new_str.push_str(string);
                    self.promote_from(new_str);
                } else {
                    this.as_mut_slice()[len..len + string.len()].copy_from_slice(string.as_bytes());
                    this.set_size(len + string.len());
                }
            }
        }
    }

    /// Return the currently allocated capacity of the string.
    ///
    /// Note that if this is a boxed string, it returns [`String::capacity()`][String::capacity],
    /// but an inline string always returns [`MAX_INLINE`].
    ///
    /// Note also that if a boxed string is converted into an inline string, its capacity is
    /// deallocated, and if the inline string is promoted to a boxed string in the future,
    /// it will be reallocated with a default capacity.
    pub fn capacity(&self) -> usize {
        if let StringCast::Boxed(string) = self.cast() {
            string.string().capacity()
        } else {
            MAX_INLINE
        }
    }

    /// Shrink the capacity of the string to fit its contents exactly.
    ///
    /// This has no effect on inline strings, which always have a fixed capacity.
    /// Thus, it's not safe to assume that [`capacity()`][SmartString::capacity] will
    /// equal [`len()`][SmartString::len] after calling this.
    ///
    /// Calling this on a [`LazyCompact`] string that is currently
    /// heap allocated but is short enough to be inlined will deallocate the
    /// heap allocation and convert it to an inline string.
    pub fn shrink_to_fit(&mut self) {
        if let StringCastMut::Boxed(string) = self.cast_mut() {
            if string.len() > MAX_INLINE {
                string.string_mut().shrink_to_fit();
            }
        }
        self.really_try_demote();
    }

    /// Truncate the string to `new_len` bytes.
    ///
    /// If `new_len` is larger than the string's current length, this does nothing.
    /// If `new_len` isn't on a UTF-8 character boundary, this method panics.
    pub fn truncate(&mut self, new_len: usize) {
        match self.cast_mut() {
            StringCastMut::Boxed(string) => string.string_mut().truncate(new_len),
            StringCastMut::Inline(string) => {
                if new_len < string.len() {
                    assert!(string.as_str().is_char_boundary(new_len));
                    string.set_size(new_len);
                }
                return;
            }
        }
        self.try_demote();
    }

    /// Pop a `char` off the end of the string.
    pub fn pop(&mut self) -> Option<char> {
        let result = match self.cast_mut() {
            StringCastMut::Boxed(string) => string.string_mut().pop()?,
            StringCastMut::Inline(string) => {
                let ch = string.as_str().chars().rev().next()?;
                string.set_size(string.len() - ch.len_utf8());
                return Some(ch);
            }
        };
        self.try_demote();
        Some(result)
    }

    /// Remove a `char` from the string at the given index.
    ///
    /// If the index doesn't fall on a UTF-8 character boundary, this method panics.
    pub fn remove(&mut self, index: usize) -> char {
        let result = match self.cast_mut() {
            StringCastMut::Boxed(string) => string.string_mut().remove(index),
            StringCastMut::Inline(string) => {
                let ch = match string.as_str()[index..].chars().next() {
                    Some(ch) => ch,
                    None => panic!("cannot remove a char from the end of a string"),
                };
                let next = index + ch.len_utf8();
                let len = string.len();
                let tail_len = len - next;
                if tail_len > 0 {
                    unsafe {
                        (&mut string.as_mut_slice()[index] as *mut u8)
                            .copy_from(&string.as_slice()[next], tail_len);
                    }
                }
                string.set_size(len - (next - index));
                return ch;
            }
        };
        self.try_demote();
        result
    }

    /// Insert a `char` into the string at the given index.
    ///
    /// If the index doesn't fall on a UTF-8 character boundary, this method panics.
    pub fn insert(&mut self, index: usize, ch: char) {
        match self.cast_mut() {
            StringCastMut::Boxed(string) => {
                string.string_mut().insert(index, ch);
            }
            StringCastMut::Inline(string) if string.len() + ch.len_utf8() <= MAX_INLINE => {
                let mut buffer = [0; 4];
                let buffer = ch.encode_utf8(&mut buffer).as_bytes();
                string.insert_bytes(index, buffer);
            }
            _ => {
                let mut string = self.to_string();
                string.insert(index, ch);
                self.promote_from(string);
            }
        }
    }

    /// Insert a string slice into the string at the given index.
    ///
    /// If the index doesn't fall on a UTF-8 character boundary, this method panics.
    pub fn insert_str(&mut self, index: usize, string: &str) {
        match self.cast_mut() {
            StringCastMut::Boxed(this) => {
                this.string_mut().insert_str(index, string);
            }
            StringCastMut::Inline(this) if this.len() + string.len() <= MAX_INLINE => {
                this.insert_bytes(index, string.as_bytes());
            }
            _ => {
                let mut this = self.to_string();
                this.insert_str(index, string);
                self.promote_from(this);
            }
        }
    }

    /// Split the string into two at the given index.
    ///
    /// Returns the content to the right of the index as a new string, and removes
    /// it from the original.
    ///
    /// If the index doesn't fall on a UTF-8 character boundary, this method panics.
    pub fn split_off(&mut self, index: usize) -> Self {
        let result = match self.cast_mut() {
            StringCastMut::Boxed(string) => string.string_mut().split_off(index),
            StringCastMut::Inline(string) => {
                let s = string.as_str();
                assert!(s.is_char_boundary(index));
                let result = s[index..].into();
                string.set_size(index);
                return result;
            }
        };
        self.try_demote();
        result.into()
    }

    /// Clear the string.
    ///
    /// This causes any memory reserved by the string to be immediately deallocated.
    pub fn clear(&mut self) {
        *self = Self::new();
    }

    /// Filter out `char`s not matching a predicate.
    pub fn retain<F>(&mut self, mut f: F)
    where
        F: FnMut(char) -> bool,
    {
        match self.cast_mut() {
            StringCastMut::Boxed(string) => {
                string.string_mut().retain(f);
            }
            StringCastMut::Inline(string) => {
                let len = string.len();
                let mut del_bytes = 0;
                let mut index = 0;

                while index < len {
                    let ch = unsafe {
                        string
                            .as_mut_str()
                            .get_unchecked(index..len)
                            .chars()
                            .next()
                            .unwrap()
                    };
                    let ch_len = ch.len_utf8();

                    if !f(ch) {
                        del_bytes += ch_len;
                    } else if del_bytes > 0 {
                        let ptr = string.as_mut_slice().as_mut_ptr();
                        unsafe { ptr.add(index - del_bytes).copy_from(ptr.add(index), ch_len) };
                    }
                    index += ch_len;
                }
                if del_bytes > 0 {
                    string.set_size(len - del_bytes);
                }
                return;
            }
        }
        self.try_demote();
    }

    /// Construct a draining iterator over a given range.
    ///
    /// This removes the given range from the string, and returns an iterator over the
    /// removed `char`s.
    pub fn drain<R>(&mut self, range: R) -> Drain<'_, Mode>
    where
        R: RangeBounds<usize>,
    {
        Drain::new(self, range)
    }

    /// Replaces a range with the contents of a string slice.
    pub fn replace_range<R>(&mut self, range: R, replace_with: &str)
    where
        R: RangeBounds<usize>,
    {
        match self.cast_mut() {
            StringCastMut::Boxed(string) => {
                string.string_mut().replace_range(range, replace_with);
            }
            StringCastMut::Inline(string) => {
                let len = string.len();
                let (start, end) = bounds_for(&range, len);
                assert!(end >= start);
                assert!(end <= len);
                assert!(string.as_str().is_char_boundary(start));
                assert!(string.as_str().is_char_boundary(end));
                let replaced_len = end - start;
                let replace_len = replace_with.len();
                if (len - replaced_len) + replace_len > MAX_INLINE {
                    let mut string = string.as_str().to_string();
                    string.replace_range(range, replace_with);
                    self.promote_from(string);
                } else {
                    let new_end = start + replace_len;
                    let end_size = len - end;
                    let ptr = string.as_mut_slice().as_mut_ptr();
                    unsafe {
                        ptr.add(end).copy_to(ptr.add(new_end), end_size);
                        ptr.add(start)
                            .copy_from(replace_with.as_bytes().as_ptr(), replace_len);
                    }
                    string.set_size(start + replace_len + end_size);
                }
                return;
            }
        }
        self.try_demote();
    }
}

fn bounds_for<R>(range: &R, max_len: usize) -> (usize, usize)
where
    R: RangeBounds<usize>,
{
    let start = match range.start_bound() {
        Bound::Included(&n) => n,
        Bound::Excluded(&n) => n.checked_add(1).unwrap(),
        Bound::Unbounded => 0,
    };
    let end = match range.end_bound() {
        Bound::Included(&n) => n.checked_add(1).unwrap(),
        Bound::Excluded(&n) => n,
        Bound::Unbounded => max_len,
    };
    (start, end)
}

impl<Mode: SmartStringMode> Default for SmartString<Mode> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Mode: SmartStringMode> AsRef<str> for SmartString<Mode> {
    fn as_ref(&self) -> &str {
        self.deref()
    }
}

impl<Mode: SmartStringMode> AsMut<str> for SmartString<Mode> {
    fn as_mut(&mut self) -> &mut str {
        self.deref_mut()
    }
}

impl<Mode: SmartStringMode> AsRef<[u8]> for SmartString<Mode> {
    fn as_ref(&self) -> &[u8] {
        self.deref().as_bytes()
    }
}

impl<Mode: SmartStringMode> Borrow<str> for SmartString<Mode> {
    fn borrow(&self) -> &str {
        self.deref()
    }
}

impl<Mode: SmartStringMode> BorrowMut<str> for SmartString<Mode> {
    fn borrow_mut(&mut self) -> &mut str {
        self.deref_mut()
    }
}

impl<Mode: SmartStringMode> Index<Range<usize>> for SmartString<Mode> {
    type Output = str;
    fn index(&self, index: Range<usize>) -> &Self::Output {
        &self.deref()[index]
    }
}

impl<Mode: SmartStringMode> Index<RangeTo<usize>> for SmartString<Mode> {
    type Output = str;
    fn index(&self, index: RangeTo<usize>) -> &Self::Output {
        &self.deref()[index]
    }
}

impl<Mode: SmartStringMode> Index<RangeFrom<usize>> for SmartString<Mode> {
    type Output = str;
    fn index(&self, index: RangeFrom<usize>) -> &Self::Output {
        &self.deref()[index]
    }
}

impl<Mode: SmartStringMode> Index<RangeFull> for SmartString<Mode> {
    type Output = str;
    fn index(&self, _index: RangeFull) -> &Self::Output {
        self.deref()
    }
}

impl<Mode: SmartStringMode> Index<RangeInclusive<usize>> for SmartString<Mode> {
    type Output = str;
    fn index(&self, index: RangeInclusive<usize>) -> &Self::Output {
        &self.deref()[index]
    }
}

impl<Mode: SmartStringMode> Index<RangeToInclusive<usize>> for SmartString<Mode> {
    type Output = str;
    fn index(&self, index: RangeToInclusive<usize>) -> &Self::Output {
        &self.deref()[index]
    }
}

impl<Mode: SmartStringMode> IndexMut<Range<usize>> for SmartString<Mode> {
    fn index_mut(&mut self, index: Range<usize>) -> &mut Self::Output {
        &mut self.deref_mut()[index]
    }
}

impl<Mode: SmartStringMode> IndexMut<RangeTo<usize>> for SmartString<Mode> {
    fn index_mut(&mut self, index: RangeTo<usize>) -> &mut Self::Output {
        &mut self.deref_mut()[index]
    }
}

impl<Mode: SmartStringMode> IndexMut<RangeFrom<usize>> for SmartString<Mode> {
    fn index_mut(&mut self, index: RangeFrom<usize>) -> &mut Self::Output {
        &mut self.deref_mut()[index]
    }
}

impl<Mode: SmartStringMode> IndexMut<RangeFull> for SmartString<Mode> {
    fn index_mut(&mut self, _index: RangeFull) -> &mut Self::Output {
        self.deref_mut()
    }
}

impl<Mode: SmartStringMode> IndexMut<RangeInclusive<usize>> for SmartString<Mode> {
    fn index_mut(&mut self, index: RangeInclusive<usize>) -> &mut Self::Output {
        &mut self.deref_mut()[index]
    }
}

impl<Mode: SmartStringMode> IndexMut<RangeToInclusive<usize>> for SmartString<Mode> {
    fn index_mut(&mut self, index: RangeToInclusive<usize>) -> &mut Self::Output {
        &mut self.deref_mut()[index]
    }
}

impl<Mode: SmartStringMode> From<&'_ str> for SmartString<Mode> {
    fn from(string: &'_ str) -> Self {
        if string.len() > MAX_INLINE {
            Self::from_boxed(string.to_string().into())
        } else {
            Self::from_inline(string.as_bytes().into())
        }
    }
}

impl<Mode: SmartStringMode> From<&'_ mut str> for SmartString<Mode> {
    fn from(string: &'_ mut str) -> Self {
        if string.len() > MAX_INLINE {
            Self::from_boxed(string.to_string().into())
        } else {
            Self::from_inline(string.as_bytes().into())
        }
    }
}

impl<Mode: SmartStringMode> From<&'_ String> for SmartString<Mode> {
    fn from(string: &'_ String) -> Self {
        if string.len() > MAX_INLINE {
            Self::from_boxed(string.clone().into())
        } else {
            Self::from_inline(string.as_bytes().into())
        }
    }
}

impl<Mode: SmartStringMode> From<String> for SmartString<Mode> {
    fn from(string: String) -> Self {
        if string.len() > MAX_INLINE {
            Self::from_boxed(string.into())
        } else {
            Self::from_inline(string.as_bytes().into())
        }
    }
}

impl<Mode: SmartStringMode> From<Box<str>> for SmartString<Mode> {
    fn from(string: Box<str>) -> Self {
        if string.len() > MAX_INLINE {
            String::from(string).into()
        } else {
            Self::from(&*string)
        }
    }
}

#[cfg(feature = "std")]
impl<Mode: SmartStringMode> From<Cow<'_, str>> for SmartString<Mode> {
    fn from(string: Cow<'_, str>) -> Self {
        if string.len() > MAX_INLINE {
            String::from(string).into()
        } else {
            Self::from(&*string)
        }
    }
}

impl<'a, Mode: SmartStringMode> Extend<&'a str> for SmartString<Mode> {
    fn extend<I: IntoIterator<Item = &'a str>>(&mut self, iter: I) {
        for item in iter {
            self.push_str(item);
        }
    }
}

impl<'a, Mode: SmartStringMode> Extend<&'a char> for SmartString<Mode> {
    fn extend<I: IntoIterator<Item = &'a char>>(&mut self, iter: I) {
        for item in iter {
            self.push(*item);
        }
    }
}

impl<Mode: SmartStringMode> Extend<char> for SmartString<Mode> {
    fn extend<I: IntoIterator<Item = char>>(&mut self, iter: I) {
        for item in iter {
            self.push(item);
        }
    }
}

impl<Mode: SmartStringMode> Extend<SmartString<Mode>> for SmartString<Mode> {
    fn extend<I: IntoIterator<Item = SmartString<Mode>>>(&mut self, iter: I) {
        for item in iter {
            self.push_str(&item);
        }
    }
}

impl<Mode: SmartStringMode> Extend<String> for SmartString<Mode> {
    fn extend<I: IntoIterator<Item = String>>(&mut self, iter: I) {
        for item in iter {
            self.push_str(&item);
        }
    }
}

impl<'a, Mode: SmartStringMode + 'a> Extend<&'a SmartString<Mode>> for SmartString<Mode> {
    fn extend<I: IntoIterator<Item = &'a SmartString<Mode>>>(&mut self, iter: I) {
        for item in iter {
            self.push_str(item);
        }
    }
}

impl<'a, Mode: SmartStringMode> Extend<&'a String> for SmartString<Mode> {
    fn extend<I: IntoIterator<Item = &'a String>>(&mut self, iter: I) {
        for item in iter {
            self.push_str(item);
        }
    }
}

impl<Mode: SmartStringMode> Add<Self> for SmartString<Mode> {
    type Output = Self;
    fn add(mut self, rhs: Self) -> Self::Output {
        self.push_str(&rhs);
        self
    }
}

impl<Mode: SmartStringMode> Add<&'_ Self> for SmartString<Mode> {
    type Output = Self;
    fn add(mut self, rhs: &'_ Self) -> Self::Output {
        self.push_str(rhs);
        self
    }
}

impl<Mode: SmartStringMode> Add<&'_ str> for SmartString<Mode> {
    type Output = Self;
    fn add(mut self, rhs: &'_ str) -> Self::Output {
        self.push_str(rhs);
        self
    }
}

impl<Mode: SmartStringMode> Add<&'_ String> for SmartString<Mode> {
    type Output = Self;
    fn add(mut self, rhs: &'_ String) -> Self::Output {
        self.push_str(rhs);
        self
    }
}

impl<Mode: SmartStringMode> Add<String> for SmartString<Mode> {
    type Output = Self;
    fn add(mut self, rhs: String) -> Self::Output {
        self.push_str(&rhs);
        self
    }
}

impl<Mode: SmartStringMode> Add<SmartString<Mode>> for String {
    type Output = Self;
    fn add(mut self, rhs: SmartString<Mode>) -> Self::Output {
        self.push_str(&rhs);
        self
    }
}

impl<Mode: SmartStringMode> FromIterator<Self> for SmartString<Mode> {
    fn from_iter<I: IntoIterator<Item = Self>>(iter: I) -> Self {
        let mut out = Self::new();
        out.extend(iter.into_iter());
        out
    }
}

impl<Mode: SmartStringMode> FromIterator<String> for SmartString<Mode> {
    fn from_iter<I: IntoIterator<Item = String>>(iter: I) -> Self {
        let mut out = Self::new();
        out.extend(iter.into_iter());
        out
    }
}

impl<'a, Mode: SmartStringMode + 'a> FromIterator<&'a Self> for SmartString<Mode> {
    fn from_iter<I: IntoIterator<Item = &'a Self>>(iter: I) -> Self {
        let mut out = Self::new();
        out.extend(iter.into_iter());
        out
    }
}

impl<'a, Mode: SmartStringMode> FromIterator<&'a str> for SmartString<Mode> {
    fn from_iter<I: IntoIterator<Item = &'a str>>(iter: I) -> Self {
        let mut out = Self::new();
        out.extend(iter.into_iter());
        out
    }
}

impl<'a, Mode: SmartStringMode> FromIterator<&'a String> for SmartString<Mode> {
    fn from_iter<I: IntoIterator<Item = &'a String>>(iter: I) -> Self {
        let mut out = Self::new();
        out.extend(iter.into_iter());
        out
    }
}

impl<Mode: SmartStringMode> FromIterator<char> for SmartString<Mode> {
    fn from_iter<I: IntoIterator<Item = char>>(iter: I) -> Self {
        let mut out = Self::new();
        for ch in iter {
            out.push(ch);
        }
        out
    }
}

impl<Mode: SmartStringMode> FromStr for SmartString<Mode> {
    type Err = Infallible;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self::from(s))
    }
}

impl<Mode: SmartStringMode> Into<String> for SmartString<Mode> {
    /// Unwrap a boxed [`String`][String], or copy an inline string into a new [`String`][String].
    ///
    /// [String]: https://doc.rust-lang.org/std/string/struct.String.html
    fn into(self) -> String {
        match self.cast_into() {
            StringCastInto::Boxed(string) => string.into_string(),
            StringCastInto::Inline(string) => string.as_str().to_string(),
        }
    }
}

impl<Mode: SmartStringMode> PartialEq<str> for SmartString<Mode> {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl<Mode: SmartStringMode> PartialEq<&'_ str> for SmartString<Mode> {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl<Mode: SmartStringMode> PartialEq<SmartString<Mode>> for &'_ str {
    fn eq(&self, other: &SmartString<Mode>) -> bool {
        other.eq(*self)
    }
}

impl<Mode: SmartStringMode> PartialEq<SmartString<Mode>> for str {
    fn eq(&self, other: &SmartString<Mode>) -> bool {
        other.eq(self)
    }
}

impl<Mode: SmartStringMode> PartialEq<String> for SmartString<Mode> {
    fn eq(&self, other: &String) -> bool {
        self.eq(other.as_str())
    }
}

impl<Mode: SmartStringMode> PartialEq<SmartString<Mode>> for String {
    fn eq(&self, other: &SmartString<Mode>) -> bool {
        other.eq(self.as_str())
    }
}

impl<Mode: SmartStringMode> PartialEq for SmartString<Mode> {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl<Mode: SmartStringMode> Eq for SmartString<Mode> {}

impl<Mode: SmartStringMode> PartialOrd<str> for SmartString<Mode> {
    fn partial_cmp(&self, other: &str) -> Option<Ordering> {
        self.as_str().partial_cmp(other)
    }
}

impl<Mode: SmartStringMode> PartialOrd for SmartString<Mode> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.partial_cmp(other.as_str())
    }
}

impl<Mode: SmartStringMode> Ord for SmartString<Mode> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl<Mode: SmartStringMode> Hash for SmartString<Mode> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl<Mode: SmartStringMode> Debug for SmartString<Mode> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        Debug::fmt(self.as_str(), f)
    }
}

impl<Mode: SmartStringMode> Display for SmartString<Mode> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        Display::fmt(self.as_str(), f)
    }
}

impl<Mode: SmartStringMode> Write for SmartString<Mode> {
    fn write_str(&mut self, string: &str) -> Result<(), Error> {
        self.push_str(string);
        Ok(())
    }
}

#[cfg(any(test, feature = "test"))]
#[allow(missing_docs)]
pub mod test;
