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

use std::cell::Cell;
use std::cell::RefCell;
use std::cell::UnsafeCell;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

use crate::any::ProvidesStaticType;

macro_rules! any_lifetime {
    ( $t:ty ) => {
        unsafe impl<'a> $crate::any::ProvidesStaticType<'a> for $t {
            type StaticType = $t;
        }
    };
}

// One of the disadvantages of AnyLifetime is there is no finite covering set of
// types so we predeclare instances for things that seem useful, but the list is
// pretty adhoc
any_lifetime!(());
any_lifetime!(bool);
any_lifetime!(u8);
any_lifetime!(u16);
any_lifetime!(u32);
any_lifetime!(u64);
any_lifetime!(u128);
any_lifetime!(usize);
any_lifetime!(i8);
any_lifetime!(i16);
any_lifetime!(i32);
any_lifetime!(i64);
any_lifetime!(i128);
any_lifetime!(isize);
any_lifetime!(f32);
any_lifetime!(f64);
any_lifetime!(String);
any_lifetime!(str);

unsafe impl<'a, T: ProvidesStaticType<'a> + ?Sized> ProvidesStaticType<'a> for &'a T {
    type StaticType = &'static T::StaticType;
}
unsafe impl<'a, T: ProvidesStaticType<'a> + ?Sized> ProvidesStaticType<'a> for &'a mut T {
    type StaticType = &'static mut T::StaticType;
}
unsafe impl<'a, T: ProvidesStaticType<'a> + ?Sized> ProvidesStaticType<'a> for *const T {
    type StaticType = *const T::StaticType;
}
unsafe impl<'a, T: ProvidesStaticType<'a> + ?Sized> ProvidesStaticType<'a> for *mut T {
    type StaticType = *mut T::StaticType;
}
unsafe impl<'a, T> ProvidesStaticType<'a> for [T]
where
    T: ProvidesStaticType<'a>,
    T::StaticType: Sized,
{
    type StaticType = [T::StaticType];
}
unsafe impl<'a, T: ProvidesStaticType<'a> + ?Sized> ProvidesStaticType<'a> for Box<T> {
    type StaticType = Box<T::StaticType>;
}
unsafe impl<'a, T: ProvidesStaticType<'a> + ?Sized> ProvidesStaticType<'a> for Rc<T> {
    type StaticType = Rc<T::StaticType>;
}
unsafe impl<'a, T: ProvidesStaticType<'a> + ?Sized> ProvidesStaticType<'a> for Arc<T> {
    type StaticType = Arc<T::StaticType>;
}
unsafe impl<'a, T: ProvidesStaticType<'a>> ProvidesStaticType<'a> for Cell<T> {
    type StaticType = Cell<T::StaticType>;
}
unsafe impl<'a, T: ProvidesStaticType<'a>> ProvidesStaticType<'a> for UnsafeCell<T> {
    type StaticType = UnsafeCell<T::StaticType>;
}
unsafe impl<'a, T: ProvidesStaticType<'a>> ProvidesStaticType<'a> for RefCell<T> {
    type StaticType = RefCell<T::StaticType>;
}
unsafe impl<'a, T> ProvidesStaticType<'a> for Option<T>
where
    T: ProvidesStaticType<'a>,
    T::StaticType: Sized,
{
    type StaticType = Option<T::StaticType>;
}
unsafe impl<'a, T, E> ProvidesStaticType<'a> for Result<T, E>
where
    T: ProvidesStaticType<'a>,
    T::StaticType: Sized,
    E: ProvidesStaticType<'a>,
    E::StaticType: Sized,
{
    type StaticType = Result<T::StaticType, E::StaticType>;
}
unsafe impl<'a, T> ProvidesStaticType<'a> for Vec<T>
where
    T: ProvidesStaticType<'a>,
    T::StaticType: Sized,
{
    type StaticType = Vec<T::StaticType>;
}
unsafe impl<'a, K, V> ProvidesStaticType<'a> for HashMap<K, V>
where
    K: ProvidesStaticType<'a>,
    K::StaticType: Sized,
    V: ProvidesStaticType<'a>,
    V::StaticType: Sized,
{
    type StaticType = HashMap<K::StaticType, V::StaticType>;
}
unsafe impl<'a, K, V> ProvidesStaticType<'a> for BTreeMap<K, V>
where
    K: ProvidesStaticType<'a>,
    K::StaticType: Sized,
    V: ProvidesStaticType<'a>,
    V::StaticType: Sized,
{
    type StaticType = BTreeMap<K::StaticType, V::StaticType>;
}
