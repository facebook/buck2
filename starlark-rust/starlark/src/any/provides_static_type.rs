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

/// Provides access to the same type as `Self` but with all lifetimes dropped to `'static`
/// (including lifetimes of parameters).
///
/// This type is usually implemented with `#[derive(ProvidesStaticType)]`.
///
/// # Safety
///
/// `StaticType` must be `Self` with all lifetimes replaced by `'static`. Additionally, if
/// `Self::StaticType` implements [`IsStaticType`], the two impls must agree, i.e.
/// `<Self::StaticType as IsStaticType>::Reinfect<'a>` is `Self` and
/// `<Self::StaticType as IsStaticType>::Reinfect<'static>` is `Self::StaticType`.
pub unsafe trait ProvidesStaticType<'a> {
    /// Same type as `Self` but with lifetimes dropped to `'static`.
    type StaticType: 'static + ?Sized;
    // FIXME(JakobDegen): Ideally we'd like to "connect" this trait to `IsStaticType` bia the
    // following two additional bounds:
    //
    // ```rs
    // + IsStaticType<Reinfect<'a> = Self>;
    // + IsStaticType<Reinfect<'static> = Self::StaticType>;
    // ```
    //
    // That would ensure that both operations agree with each other. Unfortunately, the compiler
    // struggles with the bounds when this is being implemented for some `X<T>` with further bounds
    // on `T` (including `Sized`). So we leave this bound out and rely on the `unsafe` to guarantee
    // this agreement.
}

/// A `'static` type that can be "reinfected" with a lifetime to produce a related type.
///
/// This trait is the inverse of [`ProvidesStaticType`]. Given a static type `T`,
/// `T::Reinfect<'a>` produces a type that is `T` with lifetime `'a` injected.
///
/// This type is usually implemented with `#[derive(ProvidesStaticType)]`.
pub trait IsStaticType: 'static {
    /// The type with lifetime `'lt` injected.
    type Reinfect<'lt>: ?Sized + ProvidesStaticType<'lt, StaticType = Self>;
}

#[cfg(test)]
mod tests {
    use crate as starlark;
    use crate::any::IsStaticType;
    use crate::any::ProvidesStaticType;

    #[test]
    fn test_provides_static_type_id() {
        // This test is just about checking that things compile. We wrap the test in this function
        // so that there's a non-'static lifetime in scope
        #[allow(dead_code)]
        // extra_unused_lifetime suppression doesn't work?
        fn some_lifetime<'a>(_r: &'a ()) {
            fn check_static_type_is<'a, T: ProvidesStaticType<'a, StaticType = U>, U>() {}
            fn check_reinfected_type_is<'a, T: IsStaticType<Reinfect<'a> = U>, U>() {}

            #[derive(ProvidesStaticType)]
            struct Aaa;
            check_static_type_is::<Aaa, Aaa>();
            check_reinfected_type_is::<Aaa, Aaa>();

            #[derive(ProvidesStaticType)]
            #[allow(dead_code)] // field `0` is never read
            struct Bbb<'a>(&'a str);
            check_static_type_is::<Bbb<'a>, Bbb<'static>>();
            check_reinfected_type_is::<Bbb<'static>, Bbb<'a>>();

            #[derive(ProvidesStaticType)]
            struct Ccc<X>(X);
            check_static_type_is::<Ccc<String>, Ccc<String>>();
            check_reinfected_type_is::<Ccc<String>, Ccc<String>>();

            #[derive(ProvidesStaticType)]
            struct LifetimeTypeConst<'a, T, const N: usize>([&'a T; N]);
            check_static_type_is::<LifetimeTypeConst<'a, i32, 3>, LifetimeTypeConst<'static, i32, 3>>(
            );
            check_reinfected_type_is::<
                LifetimeTypeConst<'static, i32, 3>,
                LifetimeTypeConst<'a, i32, 3>,
            >();

            #[derive(ProvidesStaticType)]
            struct TypeWithConstraint<T: std::fmt::Display>(T);
            check_static_type_is::<TypeWithConstraint<String>, TypeWithConstraint<String>>();
            check_reinfected_type_is::<TypeWithConstraint<String>, TypeWithConstraint<String>>();

            struct TypeWhichDoesNotImplementAnyLifetime;

            #[derive(ProvidesStaticType)]
            struct TypeWithStaticLifetime<T: 'static>(T);
            check_static_type_is::<
                TypeWithStaticLifetime<TypeWhichDoesNotImplementAnyLifetime>,
                TypeWithStaticLifetime<TypeWhichDoesNotImplementAnyLifetime>,
            >();
            check_reinfected_type_is::<
                TypeWithStaticLifetime<TypeWhichDoesNotImplementAnyLifetime>,
                TypeWithStaticLifetime<TypeWhichDoesNotImplementAnyLifetime>,
            >();
        }
    }

    #[test]
    fn test_provides_static_type_when_type_parameter_has_bound_with_lifetime() {
        trait My<'a> {}

        #[derive(ProvidesStaticType)]
        #[allow(dead_code)] // field `0` is never read
        struct FooBar<'x, P: My<'x>>(&'x P);
    }
}
