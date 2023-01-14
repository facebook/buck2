/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Methods that build upon the [`Any` trait](std::any::Any).

use std::any::TypeId;
use std::cell::Cell;
use std::cell::RefCell;
use std::cell::UnsafeCell;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

pub use gazebo_derive::ProvidesStaticType;

/// Provides access to the same type as `Self` but with all lifetimes dropped to `'static`
/// (including lifetimes of parameters).
///
/// This type is usually implemented with `#[derive(ProvidesStaticType)]`.
pub unsafe trait ProvidesStaticType {
    /// Same type as `Self` but with lifetimes dropped to `'static`.
    ///
    /// The trait is unsafe because if this is implemented incorrectly,
    /// the program might not work correctly.
    type StaticType: 'static + ?Sized;
}

/// Any `ProvidesStaticType` can implement `AnyLifetime`.
///
/// Note `ProvidesStaticType` and `AnyLifetime` cannot be the same type,
/// because `AnyLifetime` need to be object safe,
/// and `ProvidesStaticType` has type member.
unsafe impl<'a, T: ProvidesStaticType + 'a + ?Sized> AnyLifetime<'a> for T {
    fn static_type_id() -> TypeId
    where
        Self: Sized,
    {
        TypeId::of::<T::StaticType>()
    }

    fn static_type_of(&self) -> TypeId {
        TypeId::of::<T::StaticType>()
    }
}

/// Like [`Any`](std::any::Any), but while [`Any`](std::any::Any) requires `'static`,
/// this version allows a lifetime parameter.
///
/// Code using this trait is _unsafe_ if your implementation of the inner
/// methods do not meet the invariants listed. Therefore, it is recommended you
/// use one of the helper macros.
///
/// If your data type is of the form `Foo` or `Foo<'v>` you can derive
/// `AnyLifetime`:
///
/// ```
/// use gazebo::any::ProvidesStaticType;
/// #[derive(ProvidesStaticType)]
/// struct Foo1();
/// #[derive(ProvidesStaticType)]
/// struct Foo2<'a>(&'a ());
/// ```
///
/// For more complicated context or constraints, you can implement `ProvidesStaticType`
/// directly.
///
/// ```
/// use gazebo::any::ProvidesStaticType;
/// # fn main() {
/// # use std::fmt::Display;
/// struct Baz<T: Display>(T);
/// # // TODO: `#[derive(ProvidesStaticType)]` should learn to handle this case too.
/// unsafe impl<T> ProvidesStaticType for Baz<T>
///     where
///         T: ProvidesStaticType + Display,
///         T::StaticType: Display + Sized,
/// {
///     type StaticType = Baz<T::StaticType>;
/// }
/// # }
/// ```
pub unsafe trait AnyLifetime<'a>: 'a {
    /// Must return the `TypeId` of `Self` but where the lifetimes are changed
    /// to `'static`. Must be consistent with `static_type_of`.
    fn static_type_id() -> TypeId
    where
        Self: Sized;

    /// Must return the `TypeId` of `Self` but where the lifetimes are changed
    /// to `'static`. Must be consistent with `static_type_id`. Must not
    /// consult the `self` parameter in any way.
    fn static_type_of(&self) -> TypeId;
    // Required so we can have a `dyn AnyLifetime`.
}

impl<'a> dyn AnyLifetime<'a> {
    /// Is the value of type `T`.
    pub fn is<T: AnyLifetime<'a>>(&self) -> bool {
        self.static_type_of() == T::static_type_id()
    }

    /// Downcast a reference to type `T`, or return [`None`](None) if it is not the
    /// right type.
    pub fn downcast_ref<T: AnyLifetime<'a>>(&self) -> Option<&T> {
        if self.is::<T>() {
            // SAFETY: just checked whether we are pointing to the correct type.
            unsafe { Some(&*(self as *const Self as *const T)) }
        } else {
            None
        }
    }

    /// Downcast a mutable reference to type `T`, or return [`None`](None) if it is not
    /// the right type.
    pub fn downcast_mut<T: AnyLifetime<'a>>(&mut self) -> Option<&mut T> {
        if self.is::<T>() {
            // SAFETY: just checked whether we are pointing to the correct type.
            unsafe { Some(&mut *(self as *mut Self as *mut T)) }
        } else {
            None
        }
    }
}

macro_rules! any_lifetime {
    ( $t:ty ) => {
        unsafe impl $crate::any::ProvidesStaticType for $t {
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

unsafe impl<'a, T: ProvidesStaticType + ?Sized> ProvidesStaticType for &'a T {
    type StaticType = &'static T::StaticType;
}
unsafe impl<'a, T: ProvidesStaticType + ?Sized> ProvidesStaticType for &'a mut T {
    type StaticType = &'static mut T::StaticType;
}
unsafe impl<T: ProvidesStaticType + ?Sized> ProvidesStaticType for *const T {
    type StaticType = *const T::StaticType;
}
unsafe impl<T: ProvidesStaticType + ?Sized> ProvidesStaticType for *mut T {
    type StaticType = *mut T::StaticType;
}
unsafe impl<T> ProvidesStaticType for [T]
where
    T: ProvidesStaticType,
    T::StaticType: Sized,
{
    type StaticType = [T::StaticType];
}
unsafe impl<T: ProvidesStaticType + ?Sized> ProvidesStaticType for Box<T> {
    type StaticType = Box<T::StaticType>;
}
unsafe impl<T: ProvidesStaticType + ?Sized> ProvidesStaticType for Rc<T> {
    type StaticType = Rc<T::StaticType>;
}
unsafe impl<T: ProvidesStaticType + ?Sized> ProvidesStaticType for Arc<T> {
    type StaticType = Arc<T::StaticType>;
}
unsafe impl<T: ProvidesStaticType> ProvidesStaticType for Cell<T> {
    type StaticType = Cell<T::StaticType>;
}
unsafe impl<T: ProvidesStaticType> ProvidesStaticType for UnsafeCell<T> {
    type StaticType = UnsafeCell<T::StaticType>;
}
unsafe impl<T: ProvidesStaticType> ProvidesStaticType for RefCell<T> {
    type StaticType = RefCell<T::StaticType>;
}
unsafe impl<T> ProvidesStaticType for Option<T>
where
    T: ProvidesStaticType,
    T::StaticType: Sized,
{
    type StaticType = Option<T::StaticType>;
}
unsafe impl<T, E> ProvidesStaticType for Result<T, E>
where
    T: ProvidesStaticType,
    T::StaticType: Sized,
    E: ProvidesStaticType,
    E::StaticType: Sized,
{
    type StaticType = Result<T::StaticType, E::StaticType>;
}
unsafe impl<T> ProvidesStaticType for Vec<T>
where
    T: ProvidesStaticType,
    T::StaticType: Sized,
{
    type StaticType = Vec<T::StaticType>;
}
unsafe impl<K, V> ProvidesStaticType for HashMap<K, V>
where
    K: ProvidesStaticType,
    K::StaticType: Sized,
    V: ProvidesStaticType,
    V::StaticType: Sized,
{
    type StaticType = HashMap<K::StaticType, V::StaticType>;
}
unsafe impl<K, V> ProvidesStaticType for BTreeMap<K, V>
where
    K: ProvidesStaticType,
    K::StaticType: Sized,
    V: ProvidesStaticType,
    V::StaticType: Sized,
{
    type StaticType = BTreeMap<K::StaticType, V::StaticType>;
}

#[cfg(test)]
mod tests {
    use std::fmt::Display;

    use super::*;
    #[allow(unused_imports)] // Not actually unused, this makes testing the derive macro work
    use crate as gazebo;

    #[test]
    fn test_can_convert() {
        #[derive(Debug, PartialEq, ProvidesStaticType)]
        struct Value<'a>(&'a str);

        #[derive(ProvidesStaticType)]
        struct Value2<'a>(&'a str);

        // Changing the return type too `Value<'static>` causes a compile error.
        fn convert_value<'a>(x: &'a Value<'a>) -> Option<&'a Value<'a>> {
            <dyn AnyLifetime>::downcast_ref(x)
        }

        fn convert_any<'p, 'a>(x: &'p dyn AnyLifetime<'a>) -> Option<&'p Value<'a>> {
            x.downcast_ref()
        }

        let v = Value("test");
        let v2 = Value2("test");
        assert_eq!(convert_value(&v), Some(&v));
        assert_eq!(convert_any(&v), Some(&v));
        assert_eq!(convert_any(&v2), None);
    }

    #[test]
    fn test_any_lifetime() {
        fn test<'a, A: AnyLifetime<'a>>(expected: TypeId) {
            assert_eq!(expected, A::static_type_id());
        }

        test::<&str>(TypeId::of::<&str>());
        test::<&String>(TypeId::of::<&String>());
        test::<Box<str>>(TypeId::of::<Box<str>>());
    }

    #[test]
    fn test_provides_static_type_id() {
        fn test<'a, A: AnyLifetime<'a>>(expected: TypeId) {
            assert_eq!(expected, A::static_type_id());
        }

        #[derive(ProvidesStaticType)]
        struct Aaa;
        test::<Aaa>(TypeId::of::<Aaa>());

        #[derive(ProvidesStaticType)]
        struct Bbb<'a>(&'a str);
        test::<Bbb>(TypeId::of::<Bbb<'static>>());

        #[derive(ProvidesStaticType)]
        struct Bbb2<'a, 'b>(&'a str, &'b str);
        test::<Bbb2>(TypeId::of::<Bbb2<'static, 'static>>());

        #[derive(ProvidesStaticType)]
        struct Ccc<X>(X);
        test::<Ccc<String>>(TypeId::of::<Ccc<String>>());

        #[derive(ProvidesStaticType)]
        struct LifetimeTypeConst<'a, T, const N: usize>([&'a T; N]);
        test::<LifetimeTypeConst<i32, 3>>(TypeId::of::<LifetimeTypeConst<'static, i32, 3>>());

        #[derive(ProvidesStaticType)]
        struct TypeWithConstraint<T: Display>(T);
        test::<TypeWithConstraint<String>>(TypeId::of::<TypeWithConstraint<String>>());

        struct TypeWhichDoesNotImplementAnyLifetime;

        #[derive(ProvidesStaticType)]
        struct TypeWithStaticLifetime<T: 'static>(T);
        test::<TypeWithStaticLifetime<TypeWhichDoesNotImplementAnyLifetime>>(TypeId::of::<
            TypeWithStaticLifetime<TypeWhichDoesNotImplementAnyLifetime>,
        >());
    }

    #[test]
    fn test_provides_static_type_when_type_parameter_has_bound_with_lifetime() {
        trait My<'a> {}

        #[derive(ProvidesStaticType)]
        struct FooBar<'x, P: My<'x>>(&'x P);
    }
}
