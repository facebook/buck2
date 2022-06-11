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

// Possible optimisations:
// Avoid the Box duplication
// Encode Int in the pointer too

// We use pointer tagging on the bottom two bits:
// 00 => this Value pointer is actually a FrozenValue pointer
// 01 => this is a real Value pointer
// 11 => this is a bool (next bit: 1 => true, 0 => false)
// 10 => this is a None
//
// We don't use pointer tagging for Int (although we'd like to), because
// our val_ref requires a pointer to the value. We need to put that pointer
// somewhere. The solution is to have a separate value storage vs vtable.

use std::{
    borrow::Cow,
    cmp::Ordering,
    fmt,
    fmt::{Debug, Display},
};

use either::Either;
use gazebo::{
    any::{AnyLifetime, ProvidesStaticType},
    cast,
    coerce::{coerce, Coerce, CoerceKey},
    prelude::*,
};
use indexmap::Equivalent;
use num_bigint::BigInt;
use serde::{Serialize, Serializer};

use crate::{
    collections::{Hashed, StarlarkHashValue, StarlarkHasher},
    eval::{
        compiler::def::{Def, FrozenDef},
        runtime::{arguments::ArgumentsFull, call_stack::FrozenFileSpan},
        Arguments, Evaluator, ParametersSpec,
    },
    sealed::Sealed,
    values::{
        dict::FrozenDict,
        docs::DocItem,
        enumeration::{EnumType, FrozenEnumValue},
        float::StarlarkFloat,
        function::{FrozenBoundMethod, NativeFunction, FUNCTION_TYPE},
        int::PointerI32,
        layout::{
            arena::{AValueHeader, AValueRepr},
            avalue::{basic_ref, AValue, StarlarkStrAValue, VALUE_FALSE, VALUE_NONE, VALUE_TRUE},
            pointer::{FrozenPointer, Pointer},
            static_string::VALUE_EMPTY_STRING,
            typed::string::StringValueLike,
            vtable::AValueDyn,
        },
        list::FrozenList,
        num::Num,
        range::Range,
        record::{FrozenRecord, RecordType},
        recursive_repr_or_json_guard::{json_stack_push, repr_stack_push},
        stack_guard,
        string::StarlarkStr,
        structs::FrozenStruct,
        tuple::{FrozenTuple, Tuple},
        types::unbound::MaybeUnboundValue,
        Freeze, Freezer, FrozenRef, FrozenStringValue, FrozenValueTyped, Heap, StarlarkValue,
        StringValue, UnpackValue, ValueError, ValueIdentity,
    },
};

/// A Starlark value. The lifetime argument `'v` corresponds to the [`Heap`](crate::values::Heap) it is stored on.
///
/// Many of the methods simply forward to the underlying [`StarlarkValue`](crate::values::StarlarkValue).
/// The [`Display`](std::fmt::Display) trait is equivalent to the `repr()` function in Starlark.
#[derive(Clone_, Copy_, Dupe_, ProvidesStaticType)]
// One possible change: moving to Forward during GC.
pub struct Value<'v>(pub(crate) Pointer<'v, AValueHeader>);

unsafe impl<'v> Coerce<Value<'v>> for Value<'v> {}
unsafe impl<'v> CoerceKey<Value<'v>> for Value<'v> {}
unsafe impl<'v> Coerce<Value<'v>> for FrozenValue {}
unsafe impl<'v> CoerceKey<Value<'v>> for FrozenValue {}

impl Default for Value<'_> {
    fn default() -> Self {
        Self::new_none()
    }
}

impl Default for FrozenValue {
    fn default() -> Self {
        Self::new_none()
    }
}

impl Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match repr_stack_push(*self) {
            Ok(_guard) => {
                // We want to reuse Display for `repr`, so that means that
                // strings must display "with quotes", so we get everything consistent.
                Display::fmt(self.get_ref().as_display(), f)
            }
            Err(..) => {
                let mut recursive = String::new();
                self.get_ref().collect_repr_cycle(&mut recursive);
                write!(f, "{}", recursive)
            }
        }
    }
}

impl Display for FrozenValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.to_value(), f)
    }
}

fn debug_value(typ: &str, v: Value, f: &mut fmt::Formatter) -> fmt::Result {
    f.debug_tuple(typ).field(v.get_ref().as_debug()).finish()
}

impl Debug for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        debug_value("Value", *self, f)
    }
}

impl Debug for FrozenValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        debug_value("FrozenValue", Value::new_frozen(*self), f)
    }
}

impl<'v> PartialEq for Value<'v> {
    fn eq(&self, other: &Value<'v>) -> bool {
        self.equals(*other).ok() == Some(true)
    }
}

impl PartialEq for FrozenValue {
    fn eq(&self, other: &FrozenValue) -> bool {
        self.to_value().eq(&other.to_value())
    }
}

impl Eq for Value<'_> {}

impl Eq for FrozenValue {}

impl Equivalent<FrozenValue> for Value<'_> {
    fn equivalent(&self, key: &FrozenValue) -> bool {
        key.equals(*self).unwrap()
    }
}

impl Equivalent<Value<'_>> for FrozenValue {
    fn equivalent(&self, key: &Value) -> bool {
        self.equals(*key).unwrap()
    }
}

/// A [`Value`] that can never be changed. Can be converted back to a [`Value`] with [`to_value`](FrozenValue::to_value).
///
/// A [`FrozenValue`] exists on a [`FrozenHeap`](crate::values::FrozenHeap), which in turn can be kept
/// alive by a [`FrozenHeapRef`](crate::values::FrozenHeapRef). If the frozen heap gets dropped
/// while a [`FrozenValue`] from it still exists, the program will probably segfault, so be careful
/// when working directly with [`FrozenValue`]s. See the type [`OwnedFrozenValue`](crate::values::OwnedFrozenValue)
/// for a little bit more safety.
#[derive(Clone, Copy, Dupe, ProvidesStaticType)]
// One possible change: moving from Blackhole during GC
pub struct FrozenValue(pub(crate) FrozenPointer<'static, AValueHeader>);

// These can both be shared, but not obviously, because we hide a fake RefCell in Pointer to stop
// it having variance.
unsafe impl Send for FrozenValue {}
unsafe impl Sync for FrozenValue {}

impl<'v> Value<'v> {
    #[inline]
    pub(crate) fn new_ptr(x: &'v AValueHeader, is_str: bool) -> Self {
        Self(Pointer::new_unfrozen(x, is_str))
    }

    #[inline]
    pub(crate) fn new_ptr_query_is_str(x: &'v AValueHeader) -> Self {
        let is_string = x.unpack().is_str();
        Self::new_ptr(x, is_string)
    }

    #[inline]
    pub(crate) fn new_repr<T: AValue<'v>>(x: &'v AValueRepr<T>) -> Self {
        Self::new_ptr(&x.header, T::IS_STR)
    }

    #[inline]
    pub(crate) fn new_ptr_usize_with_str_tag(x: usize) -> Self {
        Self(Pointer::new_unfrozen_usize_with_str_tag(x))
    }

    /// Create a new `None` value.
    #[inline]
    pub fn new_none() -> Self {
        FrozenValue::new_none().to_value()
    }

    /// Create a new boolean.
    #[inline]
    pub fn new_bool(x: bool) -> Self {
        FrozenValue::new_bool(x).to_value()
    }

    /// Create a new integer.
    #[inline]
    pub fn new_int(x: i32) -> Self {
        FrozenValue::new_int(x).to_value()
    }

    /// Create a new blank string.
    #[inline]
    pub(crate) fn new_empty_string() -> Self {
        FrozenValue::new_empty_string().to_value()
    }

    /// Turn a [`FrozenValue`] into a [`Value`]. See the safety warnings on
    /// [`OwnedFrozenValue`](crate::values::OwnedFrozenValue).
    #[inline]
    pub fn new_frozen(x: FrozenValue) -> Self {
        // Safe if every FrozenValue must have had a reference added to its heap first.
        // That property is NOT statically checked.
        Self(x.0.to_pointer())
    }

    /// Obtain the underlying [`FrozenValue`] from inside the [`Value`], if it is one.
    #[inline]
    pub fn unpack_frozen(self) -> Option<FrozenValue> {
        if self.0.is_unfrozen() {
            None
        } else {
            // SAFETY: We've just checked the value is frozen.
            unsafe { Some(self.unpack_frozen_unchecked()) }
        }
    }

    #[inline]
    unsafe fn unpack_frozen_unchecked(self) -> FrozenValue {
        debug_assert!(!self.0.is_unfrozen());
        FrozenValue(self.0.cast_lifetime().to_frozen_pointer())
    }

    /// Is this value `None`.
    #[inline]
    pub fn is_none(self) -> bool {
        // Safe because frozen values never have a tag
        self.0.ptr_value() == cast::ptr_to_usize(&VALUE_NONE)
    }

    /// Obtain the underlying numerical value, if it is one.
    pub fn unpack_num(self) -> Option<Num<'v>> {
        Num::unpack_value(self)
    }

    /// This operation allocates `BigInt` for small int, use carefully.
    pub(crate) fn unpack_int_or_big(self) -> Option<Cow<'v, BigInt>> {
        match self.unpack_num()? {
            Num::Float(_) => None,
            Num::Int(x) => Some(Cow::Owned(BigInt::from(x))),
            Num::BigInt(x) => Some(Cow::Borrowed(x.get())),
        }
    }

    /// Obtain the underlying `bool` if it is a boolean.
    pub fn unpack_bool(self) -> Option<bool> {
        let p = self.0.ptr_value();
        if p == cast::ptr_to_usize(&VALUE_TRUE) {
            Some(true)
        } else if p == cast::ptr_to_usize(&VALUE_FALSE) {
            Some(false)
        } else {
            None
        }
    }

    /// Obtain the underlying `int` if it is an integer.
    #[inline]
    pub fn unpack_int(self) -> Option<i32> {
        self.0.unpack_int()
    }

    #[inline]
    pub(crate) fn unpack_int_value(self) -> Option<FrozenValueTyped<'static, PointerI32>> {
        if self.unpack_int().is_some() {
            // SAFETY: We've just checked the value is an int.
            unsafe {
                Some(FrozenValueTyped::new_unchecked(
                    self.unpack_frozen_unchecked(),
                ))
            }
        } else {
            None
        }
    }

    #[inline]
    pub(crate) fn is_str(self) -> bool {
        self.0.is_str()
    }

    /// Like [`unpack_str`](Value::unpack_str), but gives a pointer to a boxed string.
    /// Mostly useful for when you want to convert the string to a `dyn` trait, but can't
    /// form a `dyn` of an unsized type.
    ///
    /// Unstable and likely to be removed in future, as the presence of the `Box` is
    /// not a guaranteed part of the API.
    #[inline]
    pub fn unpack_starlark_str(self) -> Option<&'v StarlarkStr> {
        if self.is_str() {
            unsafe {
                Some(
                    &self
                        .0
                        .unpack_ptr_no_int_unchecked()
                        .as_repr::<StarlarkStrAValue>()
                        .payload
                        .1,
                )
            }
        } else {
            None
        }
    }

    /// Obtain the underlying `str` if it is a string.
    #[inline]
    pub fn unpack_str(self) -> Option<&'v str> {
        self.unpack_starlark_str().map(|s| s.as_str())
    }

    /// Get a pointer to a [`AValue`].
    pub(crate) fn get_ref(self) -> AValueDyn<'v> {
        match self.0.unpack() {
            Either::Left(x) => x.unpack(),
            Either::Right(x) => basic_ref(x),
        }
    }

    /// Downcast without checking the value type.
    #[inline]
    pub(crate) unsafe fn downcast_ref_unchecked<T: StarlarkValue<'v>>(self) -> &'v T {
        debug_assert!(self.get_ref().downcast_ref::<T>().is_some());
        if PointerI32::type_is_pointer_i32::<T>() {
            transmute!(
                &PointerI32,
                &T,
                PointerI32::new(self.0.unpack_int_unchecked())
            )
        } else {
            self.0.unpack_ptr_no_int_unchecked().payload()
        }
    }

    pub(crate) fn get_hash(self) -> anyhow::Result<StarlarkHashValue> {
        self.get_ref().get_hash()
    }

    /// Are two [`Value`]s equal, looking at only their underlying pointer. This function is
    /// low-level and provides two guarantees.
    ///
    /// 1. It is _reflexive_, the same [`Value`] passed as both arguments will result in [`true`].
    /// 2. If this function is [`true`], then [`Value::equals`] will also consider them equal.
    ///
    /// Note that other properties are not guaranteed, and the result is not considered part of the API.
    /// The result can be impacted by optimisations such as hash-consing, copy-on-write, partial
    /// evaluation etc.
    #[inline]
    pub fn ptr_eq(self, other: Value) -> bool {
        self.0.ptr_eq(other.0)
    }

    /// Returns an identity for this [`Value`], derived from its pointer. This function is
    /// low-level and provides two guarantees. Those are valid until the next GC:
    ///
    /// 1. Calling it mulitple times on the same [`Value`]  will return [`ValueIdentity`] that
    ///    compare equal.
    /// 2. If two [`Value]` have [`ValueIdentity`]  that compare equal, then [`Value::ptr_eq`] and
    ///    [`Value::equals`]  will also consider them to be equal.
    #[inline]
    pub fn identity(self) -> ValueIdentity<'v> {
        ValueIdentity::new(self)
    }

    /// Get the underlying pointer.
    /// Should be done sparingly as it slightly breaks the abstraction.
    /// Most useful as a hash key based on pointer.
    /// For external users, `Value::identity` returns an opaque `ValueIdentity` that makes fewer
    /// guarantees.
    #[inline]
    pub(crate) fn ptr_value(self) -> usize {
        self.0.ptr_value()
    }

    /// `type(x)`.
    pub fn get_type(self) -> &'static str {
        self.get_ref().get_type()
    }

    /// `bool(x)`.
    pub fn to_bool(self) -> bool {
        // Fast path for the common case
        if let Some(x) = self.unpack_bool() {
            x
        } else {
            self.get_ref().to_bool()
        }
    }

    /// `int(x)`.
    pub fn to_int(self) -> anyhow::Result<i32> {
        // Fast path for the common case
        if let Some(x) = self.unpack_int() {
            Ok(x)
        } else {
            self.get_ref().to_int()
        }
    }

    /// `x[index]`.
    pub fn at(self, index: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.get_ref().at(index, heap)
    }

    /// `x[start:stop:stride]`.
    pub fn slice(
        self,
        start: Option<Value<'v>>,
        stop: Option<Value<'v>>,
        stride: Option<Value<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        self.get_ref().slice(start, stop, stride, heap)
    }

    /// `len(x)`.
    pub fn length(self) -> anyhow::Result<i32> {
        self.get_ref().length()
    }

    /// `other in x`.
    pub fn is_in(self, other: Value<'v>) -> anyhow::Result<bool> {
        self.get_ref().is_in(other)
    }

    /// `+x`.
    pub fn plus(self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.get_ref().plus(heap)
    }

    /// `-x`.
    pub fn minus(self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.get_ref().minus(heap)
    }

    /// `x - other`.
    pub fn sub(self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.get_ref().sub(other, heap)
    }

    /// `x * other`.
    pub fn mul(self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.get_ref().mul(other, heap)
    }

    /// `x % other`.
    pub fn percent(self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.get_ref().percent(other, heap)
    }

    /// `x / other`.
    pub fn div(self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.get_ref().div(other, heap)
    }

    /// `x // other`.
    pub fn floor_div(self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.get_ref().floor_div(other, heap)
    }

    /// `x & other`.
    pub fn bit_and(self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.get_ref().bit_and(other, heap)
    }

    /// `x | other`.
    pub fn bit_or(self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.get_ref().bit_or(other, heap)
    }

    /// `x ^ other`.
    pub fn bit_xor(self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.get_ref().bit_xor(other, heap)
    }

    /// `~x`.
    pub fn bit_not(self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.get_ref().bit_not(heap)
    }

    /// `x << other`.
    pub fn left_shift(self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.get_ref().left_shift(other, heap)
    }

    /// `x >> other`.
    pub fn right_shift(self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        self.get_ref().right_shift(other, heap)
    }

    pub(crate) fn invoke_with_loc(
        self,
        location: Option<FrozenRef<'static, FrozenFileSpan>>,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        eval.with_call_stack(self, location, |eval| {
            self.get_ref().invoke(self, args, eval)
        })
    }

    /// Callable parameters if known.
    ///
    /// For now it only returns parameter spec for `def` and `lambda`.
    pub fn parameters_spec(self) -> Option<&'v ParametersSpec<Value<'v>>> {
        if let Some(def) = self.downcast_ref::<Def>() {
            Some(&def.parameters)
        } else if let Some(def) = self.downcast_ref::<FrozenDef>() {
            Some(coerce(&def.parameters))
        } else {
            None
        }
    }

    /// Invoke self with given arguments.
    pub(crate) fn invoke(
        self,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        self.invoke_with_loc(None, args, eval)
    }

    /// Invoke a function with only positional arguments.
    pub(crate) fn invoke_pos(
        self,
        pos: &[Value<'v>],
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let params = Arguments(ArgumentsFull {
            pos,
            ..ArgumentsFull::default()
        });
        self.invoke(&params, eval)
    }

    /// `type(x)`.
    pub fn get_type_value(self) -> FrozenStringValue {
        self.get_ref().get_type_value()
    }

    /// Add two [`Value`]s together. Will first try using [`radd`](StarlarkValue::radd),
    /// before falling back to [`add`](StarlarkValue::add).
    pub fn add(self, other: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        // Fast special case for ints.
        if let Some(ls) = self.unpack_int() {
            if let Some(rs) = other.unpack_int() {
                // On overflow take the slow path below.
                if let Some(sum) = ls.checked_add(rs) {
                    return Ok(Value::new_int(sum));
                }
            }
        }

        // Addition of string is super common and pretty cheap, so have a special case for it.
        if let Some(ls) = self.unpack_str() {
            if let Some(rs) = other.unpack_str() {
                if ls.is_empty() {
                    return Ok(other);
                } else if rs.is_empty() {
                    return Ok(self);
                } else {
                    return Ok(heap.alloc_str_concat(ls, rs).to_value());
                }
            }
        }

        if let Some(v) = self.get_ref().add(other, heap) {
            v
        } else if let Some(v) = other.get_ref().radd(self, heap) {
            v
        } else {
            ValueError::unsupported_owned(self.get_type(), "+", Some(other.get_type()))
        }
    }

    /// Convert a value to a [`FrozenValue`] using a supplied [`Freezer`].
    pub fn freeze(self, freezer: &Freezer) -> anyhow::Result<FrozenValue> {
        freezer.freeze(self)
    }

    /// Implement the `str()` function - converts a string value to itself,
    /// otherwise uses `repr()`.
    pub fn to_str(self) -> String {
        match self.unpack_str() {
            None => self.to_repr(),
            Some(s) => s.to_owned(),
        }
    }

    /// Implement the `repr()` function.
    pub fn to_repr(self) -> String {
        let mut s = String::new();
        self.collect_repr(&mut s);
        s
    }

    pub(crate) fn name_for_call_stack(self) -> String {
        self.get_ref().name_for_call_stack(self)
    }

    /// Convert the value to JSON.
    ///
    /// Return an error if the value or any contained value does not support conversion to JSON.
    pub fn to_json(self) -> anyhow::Result<String> {
        serde_json::to_string(&self).map_err(|e| anyhow::anyhow!(e))
    }

    /// Forwards to [`StarlarkValue::set_attr`].
    pub fn set_attr(self, attribute: &str, alloc_value: Value<'v>) -> anyhow::Result<()> {
        self.get_ref().set_attr(attribute, alloc_value)
    }

    /// Forwards to [`StarlarkValue::set_at`].
    pub fn set_at(self, index: Value<'v>, alloc_value: Value<'v>) -> anyhow::Result<()> {
        self.get_ref().set_at(index, alloc_value)
    }

    /// Forwards to [`StarlarkValue::documentation`].
    pub fn documentation(self) -> Option<DocItem> {
        self.get_ref().documentation()
    }

    /// Return the contents of an iterable collection, as an owned vector.
    pub fn iterate_collect(self, heap: &'v Heap) -> anyhow::Result<Vec<Value<'v>>> {
        // You might reasonably think this is mostly called on lists (I think it is),
        // and thus that a fast-path here would speed things up. But in my experiments
        // it's completely irrelevant (you pay a bit for the check, you save a bit on each step).
        self.with_iterator(heap, |it| it.collect())
    }

    /// Operate over an iterable for a value.
    pub fn with_iterator<T>(
        self,
        heap: &'v Heap,
        mut f: impl FnMut(&mut dyn Iterator<Item = Value<'v>>) -> T,
    ) -> anyhow::Result<T> {
        let mut res = None;
        self.get_ref().with_iterator(heap, &mut |it| {
            res = Some(f(it));
            Ok(())
        })?;
        // Safe because if we ran the iterator, we should have called it and set `res`
        Ok(res.take().expect("with_iterator to call the callback"))
    }

    /// Produce an iterable from a value.
    pub fn iterate(
        self,
        heap: &'v Heap,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'v>> {
        self.get_ref().iterate(heap)
    }

    /// Get the [`Hashed`] version of this [`Value`].
    #[inline]
    pub fn get_hashed(self) -> anyhow::Result<Hashed<Self>> {
        ValueLike::get_hashed(self)
    }

    /// Are two values equal. If the values are of different types it will
    /// return [`false`]. It will only error if there is excessive recursion.
    #[inline]
    pub fn equals(self, other: Value<'v>) -> anyhow::Result<bool> {
        ValueLike::equals(self, other)
    }

    /// How are two values comparable. For values of different types will return [`Err`].
    #[inline]
    pub fn compare(self, other: Value<'v>) -> anyhow::Result<Ordering> {
        ValueLike::compare(self, other)
    }

    /// Describe the value, in order to get its metadata in a way that could be used
    /// to generate prototypes, help information or whatever other descriptive text
    /// is required.
    /// Plan is to make this return a data type at some point in the future, possibly
    /// move on to `StarlarkValue` and include data from members.
    pub fn describe(self, name: &str) -> String {
        if self.get_type() == FUNCTION_TYPE {
            format!("def {}: pass", self.to_repr().replace(" = ...", " = None"))
        } else {
            format!("# {} = {}", name, self.to_repr())
        }
    }

    /// Call `export_as` on the underlying value, but only if the type is mutable.
    /// Otherwise, does nothing.
    pub fn export_as(self, variable_name: &str, eval: &mut Evaluator<'v, '_>) {
        self.get_ref().export_as(variable_name, eval)
    }

    /// Return the attribute with the given name.
    pub fn get_attr(self, attribute: &str, heap: &'v Heap) -> anyhow::Result<Option<Value<'v>>> {
        let aref = self.get_ref();
        if let Some(methods) = aref.get_methods() {
            let attribute = Hashed::new(attribute);
            if let Some(v) = methods.get_hashed(attribute) {
                return Ok(Some(MaybeUnboundValue::new(v).bind(self, heap)?));
            }
            Ok(aref.get_attr_hashed(attribute, heap))
        } else {
            Ok(aref.get_attr(attribute, heap))
        }
    }

    /// Like `get_attr` but return an error if the attribute is not available.
    pub fn get_attr_error(self, attribute: &str, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        match self.get_attr(attribute, heap)? {
            None => {
                ValueError::unsupported_owned(self.get_type(), &format!(".{}", attribute), None)
            }
            Some(x) => Ok(x),
        }
    }

    /// Query whether an attribute exists on a type. Should be equivalent to whether
    /// [`get_attr`](Value::get_attr) succeeds, but potentially more efficient.
    pub fn has_attr(self, attribute: &str) -> bool {
        let aref = self.get_ref();
        if let Some(methods) = aref.get_methods() {
            if methods.get(attribute).is_some() {
                return true;
            }
        }
        aref.has_attr(attribute)
    }

    /// Get a list of all the attributes this function supports, used to implement the
    /// `dir()` function.
    pub fn dir_attr(self) -> Vec<String> {
        let aref = self.get_ref();
        let mut result = if let Some(methods) = aref.get_methods() {
            let mut res = methods.names();
            res.extend(aref.dir_attr());
            res
        } else {
            aref.dir_attr()
        };
        result.sort();
        result
    }
}

impl FrozenValue {
    #[inline]
    pub(crate) fn new_ptr(x: &'static AValueHeader, is_str: bool) -> Self {
        Self(FrozenPointer::new_frozen(x, is_str))
    }

    #[inline]
    pub(crate) fn new_repr<'a, T: AValue<'a>>(x: &'static AValueRepr<T>) -> Self {
        Self::new_ptr(&x.header, T::IS_STR)
    }

    #[inline]
    pub(crate) fn new_ptr_usize_with_str_tag(x: usize) -> Self {
        Self(FrozenPointer::new_frozen_usize_with_str_tag(x))
    }

    #[inline]
    pub(crate) fn new_ptr_value(x: usize) -> Self {
        unsafe { Self(FrozenPointer::new(x)) }
    }

    /// Create a new value representing `None` in Starlark.
    #[inline]
    pub fn new_none() -> Self {
        Self::new_repr(&VALUE_NONE)
    }

    /// Create a new boolean in Starlark.
    #[inline]
    pub fn new_bool(x: bool) -> Self {
        if x {
            Self::new_repr(&VALUE_TRUE)
        } else {
            Self::new_repr(&VALUE_FALSE)
        }
    }

    /// Create a new int in Starlark.
    #[inline]
    pub fn new_int(x: i32) -> Self {
        Self(FrozenPointer::new_int(x))
    }

    /// Create a new empty string.
    #[inline]
    pub(crate) fn new_empty_string() -> Self {
        VALUE_EMPTY_STRING.unpack()
    }

    #[inline]
    pub(crate) fn ptr_value(self) -> usize {
        self.0.ptr_value()
    }

    /// Is a value a Starlark `None`.
    #[inline]
    pub fn is_none(self) -> bool {
        // Safe because frozen values never have a tag
        self.0.ptr_value() == cast::ptr_to_usize(&VALUE_NONE)
    }

    /// Return the [`bool`] if the value is a boolean, otherwise [`None`].
    pub fn unpack_bool(self) -> Option<bool> {
        let p = self.0.ptr_value();
        if p == cast::ptr_to_usize(&VALUE_TRUE) {
            Some(true)
        } else if p == cast::ptr_to_usize(&VALUE_FALSE) {
            Some(false)
        } else {
            None
        }
    }

    /// Return the int if the value is an integer, otherwise [`None`].
    #[inline]
    pub fn unpack_int(self) -> Option<i32> {
        self.0.unpack_int()
    }

    #[inline]
    pub(crate) unsafe fn unpack_int_unchecked(self) -> i32 {
        self.0.unpack_int_unchecked()
    }

    #[inline]
    pub(crate) fn is_str(self) -> bool {
        self.to_value().is_str()
    }

    // The resulting `str` is alive as long as the `FrozenHeap` is,
    // but we don't have that lifetime available to us. Therefore,
    // we cheat a little, and use the lifetime of the `FrozenValue`.
    // Because of this cheating, we don't expose it outside Starlark.
    #[allow(clippy::trivially_copy_pass_by_ref)]
    #[inline]
    pub(crate) fn unpack_str<'v>(&'v self) -> Option<&'v str> {
        self.to_value().unpack_str()
    }

    /// Get a pointer to the [`AValue`] object this value represents.
    pub(crate) fn get_ref<'v>(self) -> AValueDyn<'v> {
        match self.0.unpack() {
            Either::Left(x) => x.unpack(),
            Either::Right(x) => basic_ref(x),
        }
    }

    /// Convert a [`FrozenValue`] back to a [`Value`].
    #[inline]
    pub fn to_value<'v>(self) -> Value<'v> {
        Value::new_frozen(self)
    }

    /// Is this type builtin? We perform certain optimizations only on builtin types
    /// because we know they have well defined semantics.
    pub(crate) fn is_builtin(self) -> bool {
        // The list is not comprehensive, this is fine.
        // If some type is not listed here, some optimizations won't work for this type.
        self.is_none()
            || self.is_str()
            || self.unpack_bool().is_some()
            || self.unpack_int().is_some()
            || FrozenValueTyped::<StarlarkFloat>::new(self).is_some()
            || FrozenList::from_frozen_value(&self).is_some()
            || FrozenDict::from_frozen_value(&self).is_some()
            || FrozenValueTyped::<FrozenTuple>::new(self).is_some()
            || FrozenValueTyped::<Range>::new(self).is_some()
            || FrozenValueTyped::<FrozenDef>::new(self).is_some()
            || FrozenValueTyped::<NativeFunction>::new(self).is_some()
            || FrozenValueTyped::<FrozenStruct>::new(self).is_some()
            || FrozenValueTyped::<RecordType>::new(self).is_some()
            || FrozenValueTyped::<FrozenRecord>::new(self).is_some()
            || FrozenValueTyped::<EnumType>::new(self).is_some()
            || FrozenValueTyped::<FrozenEnumValue>::new(self).is_some()
    }

    /// Can `invoke` be called on this object speculatively?
    /// (E. g. at compiled time when all the arguments are known.)
    pub(crate) fn speculative_exec_safe(self) -> bool {
        if let Some(v) = FrozenValueTyped::<NativeFunction>::new(self) {
            v.speculative_exec_safe
        } else if let Some(v) = FrozenValueTyped::<FrozenBoundMethod>::new(self) {
            v.method.speculative_exec_safe
        } else {
            false
        }
    }

    /// `self == b` is `ptr_eq`.
    pub(crate) fn eq_is_ptr_eq(self) -> bool {
        // Note `int` is not `ptr_eq` because `int` can be equal to `float`.
        self.is_none()
            || self.unpack_bool().is_some()
            // Strings of length <= 1 are statically allocated.
            || matches!(self.unpack_str(), Some(s) if s.len() <= 1)
            // Empty tuple is statically allocated.
            || matches!(Tuple::from_value(self.to_value()), Some(t) if t.len() == 0)
    }

    /// Downcast to given type.
    #[inline]
    pub fn downcast_frozen_ref<T: StarlarkValue<'static>>(self) -> Option<FrozenRef<'static, T>> {
        self.downcast_ref::<T>().map(|value| FrozenRef { value })
    }

    /// Downcast to string.
    #[inline]
    pub fn downcast_frozen_str(self) -> Option<FrozenRef<'static, str>> {
        self.to_value()
            .unpack_str()
            .map(|value| FrozenRef { value })
    }

    /// Note: see docs about ['Value::unpack_box_str'] about instability
    #[inline]
    pub fn downcast_frozen_starlark_str(self) -> Option<FrozenRef<'static, StarlarkStr>> {
        self.to_value()
            .unpack_starlark_str()
            .map(|value| FrozenRef { value })
    }
}

impl<'v> Serialize for Value<'v> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match json_stack_push(*self) {
            Ok(_guard) => erased_serde::serialize(self.get_ref().as_serialize(), s),
            Err(..) => Err(serde::ser::Error::custom(ToJsonCycleError(self.get_type()))),
        }
    }
}

impl Serialize for FrozenValue {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.to_value().serialize(s)
    }
}

/// Abstract over [`Value`] and [`FrozenValue`].
///
/// The methods on this trait are those required to implement containers,
/// allowing implementations of [`ComplexValue`](crate::values::ComplexValue)
/// to be agnostic of their contained type.
/// For details about each function, see the documentation for [`Value`],
/// which provides the same functions (and more).
pub trait ValueLike<'v>:
    Eq
    + Copy
    + Debug
    + Default
    + Display
    + Serialize
    + CoerceKey<Value<'v>>
    + Freeze<Frozen = FrozenValue>
    + Sealed
{
    /// `StringValue` or `FrozenStringValue`.
    type String: StringValueLike<'v>;

    /// Produce a [`Value`] regardless of the type you are starting with.
    fn to_value(self) -> Value<'v>;

    /// Get referenced [`StarlarkValue`] a value as [`AnyLifetime`].
    fn as_dyn_any(self) -> &'v dyn AnyLifetime<'v>;

    /// Call this value as a function with given arguments.
    fn invoke(
        self,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        self.to_value().invoke(args, eval)
    }

    /// Hash the value.
    fn write_hash(self, hasher: &mut StarlarkHasher) -> anyhow::Result<()>;

    /// Get hash value.
    fn get_hashed(self) -> anyhow::Result<Hashed<Self>> {
        let hash = if let Some(s) = self.to_value().unpack_starlark_str() {
            s.get_hash()
        } else {
            self.to_value().get_hash()?
        };
        Ok(Hashed::new_unchecked(hash, self))
    }

    /// `repr(x)`.
    fn collect_repr(self, collector: &mut String);

    /// `str(x)`.
    fn collect_str(self, collector: &mut String) {
        if let Some(s) = self.to_value().unpack_str() {
            collector.push_str(s);
        } else {
            self.collect_repr(collector);
        }
    }

    /// `x == other`.
    ///
    /// This operation can only return error on stack overflow.
    fn equals(self, other: Value<'v>) -> anyhow::Result<bool>;

    /// `x <=> other`.
    fn compare(self, other: Value<'v>) -> anyhow::Result<Ordering>;

    /// Get a reference to underlying data or [`None`]
    /// if contained object has different type than requested.
    fn downcast_ref<T: StarlarkValue<'v>>(self) -> Option<&'v T>;
}

#[derive(Debug, thiserror::Error)]
#[error("Cycle detected when serializing value of type `{0}` to JSON")]
struct ToJsonCycleError(&'static str);

impl<'v> Sealed for Value<'v> {}

impl<'v> ValueLike<'v> for Value<'v> {
    type String = StringValue<'v>;

    #[inline]
    fn to_value(self) -> Value<'v> {
        self
    }

    fn downcast_ref<T: StarlarkValue<'v>>(self) -> Option<&'v T> {
        if T::static_type_id() == StarlarkStr::static_type_id() {
            if self.is_str() {
                // SAFETY: we just checked this is string, and requested type is string.
                Some(unsafe { self.downcast_ref_unchecked() })
            } else {
                None
            }
        } else if PointerI32::type_is_pointer_i32::<T>() {
            if self.unpack_int().is_some() {
                // SAFETY: we just checked this is int, and requested type is int.
                Some(unsafe { self.downcast_ref_unchecked() })
            } else {
                None
            }
        } else {
            self.get_ref().downcast_ref::<T>()
        }
    }

    fn collect_repr(self, collector: &mut String) {
        match repr_stack_push(self) {
            Ok(_guard) => {
                self.get_ref().collect_repr(collector);
            }
            Err(..) => {
                self.get_ref().collect_repr_cycle(collector);
            }
        }
    }

    fn write_hash(self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.get_ref().write_hash(hasher)
    }

    fn equals(self, other: Value<'v>) -> anyhow::Result<bool> {
        if self.ptr_eq(other) {
            Ok(true)
        } else {
            let _guard = stack_guard::stack_guard()?;
            self.get_ref().equals(other)
        }
    }

    fn compare(self, other: Value<'v>) -> anyhow::Result<Ordering> {
        let _guard = stack_guard::stack_guard()?;
        self.get_ref().compare(other)
    }

    fn as_dyn_any(self) -> &'v dyn AnyLifetime<'v> {
        self.get_ref().value_as_dyn_any()
    }
}

impl Sealed for FrozenValue {}

impl<'v> ValueLike<'v> for FrozenValue {
    type String = FrozenStringValue;

    #[inline]
    fn to_value(self) -> Value<'v> {
        Value::new_frozen(self)
    }

    #[inline]
    fn downcast_ref<T: StarlarkValue<'v>>(self) -> Option<&'v T> {
        self.to_value().downcast_ref()
    }

    #[inline]
    fn collect_repr(self, collector: &mut String) {
        self.to_value().collect_repr(collector)
    }

    #[inline]
    fn write_hash(self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        self.to_value().write_hash(hasher)
    }

    #[inline]
    fn equals(self, other: Value<'v>) -> anyhow::Result<bool> {
        self.to_value().equals(other)
    }

    #[inline]
    fn compare(self, other: Value<'v>) -> anyhow::Result<Ordering> {
        self.to_value().compare(other)
    }

    fn as_dyn_any(self) -> &'v dyn AnyLifetime<'v> {
        self.get_ref().value_as_dyn_any()
    }
}

fn _test_send_sync()
where
    FrozenValue: Send + Sync,
{
}

#[cfg(test)]
mod tests {
    use crate::values::{
        none::NoneType, string::StarlarkStr, types::int::PointerI32, Heap, Value, ValueLike,
    };

    #[test]
    fn test_downcast_ref() {
        let heap = Heap::new();
        let string = heap.alloc_str("asd").to_value();
        let none = Value::new_none();
        let integer = Value::new_int(17);

        assert!(string.downcast_ref::<NoneType>().is_none());
        assert!(integer.downcast_ref::<NoneType>().is_none());
        assert!(none.downcast_ref::<NoneType>().is_some());

        assert_eq!(
            "asd",
            string.downcast_ref::<StarlarkStr>().unwrap().as_str()
        );
        assert!(integer.downcast_ref::<StarlarkStr>().is_none());
        assert!(none.downcast_ref::<StarlarkStr>().is_none());

        assert!(string.downcast_ref::<PointerI32>().is_none());
        assert_eq!(17, integer.downcast_ref::<PointerI32>().unwrap().get());
        assert!(none.downcast_ref::<PointerI32>().is_none());
    }
}
