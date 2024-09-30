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

use std::cell::Ref;
use std::cell::RefCell;
use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::mem;
use std::ops::Deref;

use allocative::Allocative;
use display_container::fmt_container;
use serde::Serialize;
use starlark_map::small_set::SmallSet;
use starlark_map::Hashed;

use super::refs::SetRef;
use crate as starlark;
use crate::coerce::coerce;
use crate::coerce::Coerce;
use crate::environment::Methods;
use crate::environment::MethodsStatic;
use crate::typing::Ty;
use crate::util::refcell::unleak_borrow;
use crate::values::comparison::equals_small_set;
use crate::values::set::methods;
use crate::values::starlark_value;
use crate::values::type_repr::SetType;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::AllocValue;
use crate::values::Freeze;
use crate::values::Freezer;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::ProvidesStaticType;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Value;
use crate::StarlarkDocs;

#[derive(
    Clone,
    Default,
    Trace,
    Debug,
    ProvidesStaticType,
    StarlarkDocs,
    Allocative
)]
#[starlark_docs(builtin = "standard")]
#[repr(transparent)]
pub struct SetGen<T>(pub(crate) T);

/// Define the mutable set type.
#[derive(Default, Trace, Debug, ProvidesStaticType, Allocative, Clone)]
pub struct SetData<'v> {
    /// The data stored by the list.
    pub(crate) content: SmallSet<Value<'v>>,
}

impl<'v> SetData<'v> {
    pub fn clear(&mut self) {
        self.content.clear();
    }

    /// Iterate through the values in the set.
    pub fn iter<'a>(&'a self) -> impl ExactSizeIterator<Item = Value<'v>> + 'a {
        self.content.iter().copied()
    }

    /// Iterate through the values in the set, but retaining the hash of the values.
    pub fn iter_hashed<'a>(&'a self) -> impl Iterator<Item = Hashed<Value<'v>>> + 'a
    where
        'v: 'a,
    {
        self.content.iter_hashed().map(|h| h.copied())
    }
}

#[derive(Clone, Default, Debug, ProvidesStaticType, Allocative)]
#[repr(transparent)]
pub struct FrozenSetData {
    /// The data stored by the set. The values must all be hashable values.
    content: SmallSet<FrozenValue>,
}

/// Define the set type.
pub type Set<'v> = SetGen<SetData<'v>>;

pub type MutableSet<'v> = SetGen<RefCell<SetData<'v>>>;

pub type FrozenSet = SetGen<FrozenSetData>;

impl<'v> AllocValue<'v> for SetData<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(SetGen(RefCell::new(self)))
    }
}

impl<'v> StarlarkTypeRepr for SetData<'v> {
    type Canonical = <SetType<FrozenValue> as StarlarkTypeRepr>::Canonical;

    fn starlark_type_repr() -> Ty {
        SetType::<Value<'v>>::starlark_type_repr()
    }
}

unsafe impl<'v> Coerce<SetData<'v>> for FrozenSetData {}

// TODO Add optimizations not to allocate empty set.
impl<'v> Freeze for MutableSet<'v> {
    type Frozen = SetGen<FrozenSetData>;
    fn freeze(self, freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        let content = self.0.into_inner().content.freeze(freezer)?;
        Ok(SetGen(FrozenSetData { content }))
    }
}

pub(crate) fn set_methods() -> Option<&'static Methods> {
    static RES: MethodsStatic = MethodsStatic::new();
    RES.methods(methods::set_methods)
}

trait SetLike<'v>: Debug + Allocative {
    type ContentRef<'a>: Deref<Target = SmallSet<Value<'v>>>
    where
        Self: 'a,
        'v: 'a;
    fn content<'a>(&'a self) -> Self::ContentRef<'a>;

    // These functions are unsafe for the same reason
    // `StarlarkValue` iterator functions are unsafe.
    unsafe fn iter_start(&self);
    unsafe fn content_unchecked(&self) -> &SmallSet<Value<'v>>;
    unsafe fn iter_stop(&self);
    // fn set_at(&self, index: Hashed<Value<'v>>, value: Value<'v>) -> crate::Result<()>;
}

impl<'v> SetLike<'v> for RefCell<SetData<'v>> {
    type ContentRef<'a> = Ref<'a, SmallSet<Value<'v>>> where Self: 'a, 'v: 'a;

    fn content<'a>(&'a self) -> Ref<'a, SmallSet<Value<'v>>> {
        Ref::map(self.borrow(), |x| &x.content)
    }

    #[inline]
    unsafe fn iter_start(&self) {
        mem::forget(self.borrow());
    }

    #[inline]
    unsafe fn iter_stop(&self) {
        unleak_borrow(self);
    }

    #[inline]
    unsafe fn content_unchecked(&self) -> &SmallSet<Value<'v>> {
        &self.try_borrow_unguarded().ok().unwrap_unchecked().content
    }
}

impl<'v> SetLike<'v> for FrozenSetData {
    type ContentRef<'a> = &'a SmallSet<Value<'v>> where Self: 'a, 'v: 'a;

    fn content(&self) -> &SmallSet<Value<'v>> {
        coerce(&self.content)
    }

    unsafe fn iter_start(&self) {}

    unsafe fn iter_stop(&self) {}

    unsafe fn content_unchecked(&self) -> &SmallSet<Value<'v>> {
        coerce(&self.content)
    }
}

#[starlark_value(type = "set")]
impl<'v, T: SetLike<'v> + 'v> StarlarkValue<'v> for SetGen<T>
where
    Self: ProvidesStaticType<'v>,
{
    type Canonical = FrozenSet;

    /// Returns the length of the value, if this value is a sequence.
    fn length(&self) -> crate::Result<i32> {
        Ok(self.0.content().len() as i32)
    }

    fn is_in(&self, other: Value<'v>) -> crate::Result<bool> {
        Ok(self
            .0
            .content()
            .contains_hashed(other.get_hashed()?.as_ref()))
    }

    fn equals(&self, other: Value<'v>) -> crate::Result<bool> {
        match SetRef::from_value(other) {
            None => Ok(false),
            Some(other) => Ok(equals_small_set(&self.0.content(), &other.content)),
        }
    }

    fn get_methods() -> Option<&'static Methods> {
        set_methods()
    }

    unsafe fn iterate(&self, me: Value<'v>, _heap: &'v Heap) -> crate::Result<Value<'v>> {
        self.0.iter_start();
        Ok(me)
    }

    unsafe fn iter_size_hint(&self, index: usize) -> (usize, Option<usize>) {
        debug_assert!(index <= self.0.content().len());
        let rem = self.0.content().len() - index;
        (rem, Some(rem))
    }

    unsafe fn iter_next(&self, index: usize, _heap: &'v Heap) -> Option<Value<'v>> {
        self.0.content_unchecked().iter().nth(index).copied()
    }

    unsafe fn iter_stop(&self) {
        self.0.iter_stop();
    }

    fn to_bool(&self) -> bool {
        !self.0.content().is_empty()
    }
}

impl<'v, T: SetLike<'v>> Serialize for SetGen<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_seq(self.0.content().iter())
    }
}

impl<'v, T: SetLike<'v>> Display for SetGen<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_container(f, "set([", "])", self.0.content().iter())
    }
}
