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

use std::marker::PhantomData;

use dupe::Dupe;

use crate::any::IsStaticType;
use crate::any::ProvidesStaticType;
use crate::cast::transmute;

/// Witness that the heap identified by `'v` depends on the heap identified by `'dep`.
///
/// As described in the `branding` module, a `Value<'v>` may live either in the heap identified
/// by `'v` or in any frozen heap that that heap depends on. This type is a certificate of such a
/// dependency: it proves that the heap of `'dep` is kept alive by the heap of `'v`, so that
/// anything kept alive by the former is usable in the context of the latter. That conversion is
/// what [`rebrand`](HeapEdge::rebrand) provides.
#[derive(Copy, Clone, Dupe)]
pub struct HeapEdge<'v, 'dep> {
    _invariant: PhantomData<(fn(&'v ()) -> &'v (), fn(&'dep ()) -> &'dep ())>,
}

impl<'v, 'dep> HeapEdge<'v, 'dep> {
    /// Assert the existence of this heap dependency.
    ///
    /// # SAFETY
    ///
    /// The heap identified by `'v` must keep the heap identified by `'dep` alive. Additionally,
    /// `'dep` must be a brand: a lifetime at which only values kept alive by that heap (or
    /// `'static` data) can exist. An ordinary borrow lifetime is not a brand — stack data can be
    /// borrowed at it, and [`rebrand`](HeapEdge::rebrand) would extend such a borrow past its
    /// referent.
    pub unsafe fn unchecked_new() -> Self {
        Self {
            _invariant: PhantomData,
        }
    }

    /// Convert a value kept alive by the `'dep` heap for use in the context of the `'v` heap.
    pub fn rebrand<U>(self, v: U) -> <U::StaticType as IsStaticType>::Reinfect<'v>
    where
        U: ProvidesStaticType<'dep>,
        U::StaticType: IsStaticType + Sized,
        <U::StaticType as IsStaticType>::Reinfect<'v>: Sized,
    {
        // SAFETY: The input and output are the same type up to the brand (guaranteed by
        // `ProvidesStaticType`/`IsStaticType`), and everything that exists at the `'dep` brand
        // lives as long as the `'dep` heap, which lives at least as long as the `'v` heap (both
        // guaranteed by the construction contract of `self`)
        unsafe { transmute!(U, <U::StaticType as IsStaticType>::Reinfect<'v>, v) }
    }

    /// [`rebrand`](HeapEdge::rebrand), behind a reference.
    pub fn rebrand_ref<'a, U>(self, v: &'a U) -> &'a <U::StaticType as IsStaticType>::Reinfect<'v>
    where
        U: ProvidesStaticType<'dep>,
        U::StaticType: IsStaticType + Sized,
        <U::StaticType as IsStaticType>::Reinfect<'v>: Sized,
    {
        // SAFETY: As for `rebrand`; references to two types that differ only in lifetimes have
        // the same layout, and the borrow is kept.
        unsafe { transmute!(&'a U, &'a <U::StaticType as IsStaticType>::Reinfect<'v>, v) }
    }
}

impl<'v> HeapEdge<'v, 'static> {
    /// The dependency of every heap on the `'static` brand.
    ///
    /// The only data at the `'static` brand is immortal: statics ([`AllocStaticSimple`]) and the
    /// [`Methods`] tables reached through `&'static Methods`. Nothing keeps it alive because
    /// nothing needs to, and the frozen bit already makes the garbage collector skip it, so every
    /// heap trivially depends on it and the edge can be minted anywhere.
    ///
    /// Like the other minters, this is sound in the end state. `FrozenValue::to_value` and the
    /// typed `'static` handles can today produce `'static`-branded values that are not immortal;
    /// see "The `FrozenValue` hole" in the `branding` module.
    ///
    /// [`AllocStaticSimple`]: crate::values::AllocStaticSimple
    /// [`Methods`]: crate::environment::Methods
    pub fn immortal() -> Self {
        // SAFETY: `'static` is a brand: no stack data can be borrowed at it, so a `'static`-branded
        // value can only be immortal data, which every heap keeps alive by virtue of it never
        // being freed. (The holes listed in the doc comment are the ones the `branding` module
        // tracks for all minters.)
        unsafe { Self::unchecked_new() }
    }
}

#[cfg(test)]
mod tests {
    use crate::values::AllocStaticSimple;
    use crate::values::Heap;
    use crate::values::Value;
    use crate::values::ValueTyped;
    use crate::values::layout::heap::edge::HeapEdge;
    use crate::values::list::AllocList;
    use crate::values::list::ListRef;
    use crate::values::none::NoneType;
    use crate::values::types::none::none_type::VALUE_NONE;

    /// Rebrands a static for the given heap; the heap only serves to name the brand.
    fn at_brand<'v>(_heap: Heap<'v>, v: ValueTyped<'static, NoneType>) -> Value<'v> {
        HeapEdge::immortal().rebrand(v).to_value()
    }

    #[test]
    fn test_immortal_edge_at_nested_brands() {
        let none: &'static AllocStaticSimple<NoneType> = &VALUE_NONE;
        let none: ValueTyped<'static, NoneType> = none.at();
        Heap::temp(|outer| {
            let v1 = at_brand(outer, none);
            let list1 = outer.alloc(AllocList([v1]));
            Heap::temp(|inner| {
                let v2 = at_brand(inner, none);
                let list2 = inner.alloc(AllocList([v2]));
                assert!(v1.ptr_eq(none.to_value()));
                assert!(v2.ptr_eq(none.to_value()));
                assert!(ListRef::from_value(list1).unwrap().content()[0].is_none());
                assert!(ListRef::from_value(list2).unwrap().content()[0].is_none());
            });
        });
    }
}
