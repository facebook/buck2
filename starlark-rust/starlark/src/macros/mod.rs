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

/// Define the [`get_type`](crate::values::StarlarkValue::get_type) and
/// [`get_type_value`](crate::values::StarlarkValue::get_type_value_static) fields of
/// [`StarlarkValue`](crate::values::StarlarkValue).
#[macro_export]
macro_rules! starlark_type {
    ($typ:expr) => {
        fn get_type(&self) -> &'static str {
            $typ
        }
        fn get_type_value_static() -> $crate::values::FrozenStringValue {
            $crate::const_frozen_string!($typ)
        }
    };
}

/// Reduce boilerplate when making types instances of [`ComplexValue`](crate::values::ComplexValue)
/// - see the [`ComplexValue`](crate::values::ComplexValue) docs for an example.
#[macro_export]
macro_rules! starlark_complex_value {
    // Common part of macro variants.
    (impl $x:ident) => {
        $crate::__macro_refs::item! {
            impl<'v> $crate::values::AllocValue<'v> for $x<'v> {
                fn alloc_value(self, heap: &'v $crate::values::Heap) -> $crate::values::Value<'v> {
                    heap.alloc_complex(self)
                }
            }

            impl $crate::values::AllocFrozenValue for [< Frozen $x >] {
                fn alloc_frozen_value(self, heap: &$crate::values::FrozenHeap) -> $crate::values::FrozenValue {
                    heap.alloc_simple(self)
                }
            }

            impl<'v> $x<'v> {
                /// Downcast the value.
                pub fn from_value(x: $crate::values::Value<'v>) -> Option<&'v Self> {
                    if let Some(x) = x.unpack_frozen() {
                        $crate::values::ValueLike::downcast_ref::< [< Frozen $x >] >(x).map($crate::__macro_refs::coerce_ref)
                    } else {
                        $crate::values::ValueLike::downcast_ref::< $x<'v> >(x)
                    }
                }
            }

            impl<'v> $crate::values::UnpackValue<'v> for &'v $x<'v> {
                fn expected() -> String {
                    <$x as $crate::values::StarlarkValue>::get_type_value_static().as_str().to_owned()
                }

                fn unpack_value(x: $crate::values::Value<'v>) -> Option<&'v $x<'v>> {
                    $x::from_value(x)
                }
            }
        }
    };
    ($v:vis $x:ident) => {
        $crate::__macro_refs::item! {
            /// Type of value.
            $v type $x<'v> = [< $x Gen >]<$crate::values::Value<'v>>;
            /// Type of frozen value.
            $v type [< Frozen $x >] = [< $x Gen >]<$crate::values::FrozenValue>;

            starlark_complex_value!(impl $x);
        }
    };
    ($v:vis $x:ident <'v>) => {
        $crate::__macro_refs::item! {
            /// Type of unfrozen value.
            $v type $x<'v> = [< $x Gen >]<'v, $crate::values::Value<'v>>;
            /// Type of frozen value.
            $v type [< Frozen $x >] = [< $x Gen >]<'static, $crate::values::FrozenValue>;

            starlark_complex_value!(impl $x);
        }
    };
}

/// Reduce boilerplate when making types instances of [`ComplexValue`](crate::values::ComplexValue)
/// - see the [`ComplexValue`](crate::values::ComplexValue) docs for an example.
#[macro_export]
macro_rules! starlark_complex_values {
    ($x:ident) => {
        $crate::__macro_refs::item! {
            impl<'v> $crate::values::AllocValue<'v> for $x<'v> {
                fn alloc_value(self, heap: &'v $crate::values::Heap) -> $crate::values::Value<'v> {
                    heap.alloc_complex(self)
                }
            }

            impl $crate::values::AllocFrozenValue for [< Frozen $x >] {
                fn alloc_frozen_value(self, heap: &$crate::values::FrozenHeap) -> $crate::values::FrozenValue {
                    heap.alloc_simple(self)
                }
            }

            impl<'v> $x<'v> {
                #[allow(dead_code)]
                fn from_value(
                    x: $crate::values::Value<'v>,
                ) -> Option<$crate::__macro_refs::Either<&'v Self, &'v [< Frozen $x >]>> {
                    if let Some(x) = x.unpack_frozen() {
                        $crate::values::ValueLike::downcast_ref(x).map($crate::__macro_refs::Either::Right)
                    } else {
                        $crate::values::ValueLike::downcast_ref(x).map($crate::__macro_refs::Either::Left)
                    }
                }
            }
        }
    };
}

/// A macro reducing boilerplace defining Starlark values which are simple - they
/// aren't mutable and can't contain references to other Starlark values.
///
/// Let's define a simple object, where `+x` makes the string uppercase:
///
/// ```
/// use starlark::values::{Heap, StarlarkValue, Value, AnyLifetime, NoSerialize};
/// use starlark::{starlark_simple_value, starlark_type};
/// use derive_more::Display;
///
/// #[derive(Debug, Display, AnyLifetime, NoSerialize)]
/// struct MyObject(String);
/// starlark_simple_value!(MyObject);
/// impl<'v> StarlarkValue<'v> for MyObject {
///     starlark_type!("my_object");
///
///     // We can choose to implement whichever methods we want.
///     // All other operations will result in runtime errors.
///     fn plus(&self, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
///         Ok(heap.alloc(MyObject(self.0.to_uppercase())))
///     }
/// }
/// ```
///
/// The [`starlark_simple_value!`] macro defines instances of
/// [`AnyLifetime`](crate::values::AnyLifetime),
/// [`AllocValue`](crate::values::AllocValue),
/// [`AllocFrozenValue`](crate::values::AllocFrozenValue) and
/// [`UnpackValue`](crate::values::UnpackValue). It also defines a method:
///
/// ```
/// # use crate::starlark::values::*;
/// # struct MyObject;
/// impl MyObject {
///     pub fn from_value<'v>(x: Value<'v>) -> Option<&'v MyObject> {
/// # unimplemented!(
/// # r#"
///         ...
/// # "#);
///     }
/// }
/// ```
#[macro_export]
macro_rules! starlark_simple_value {
    ($x:ident) => {
        $crate::__macro_refs::item! {
            impl<'v> $crate::values::AllocValue<'v> for $x {
                fn alloc_value(self, heap: &'v $crate::values::Heap) -> $crate::values::Value<'v> {
                    heap.alloc_simple(self)
                }
            }

            impl $crate::values::AllocFrozenValue for $x {
                fn alloc_frozen_value(self, heap: &$crate::values::FrozenHeap) -> $crate::values::FrozenValue {
                    heap.alloc_simple(self)
                }
            }

            impl $x {
                /// Downcast a value to self type.
                pub fn from_value<'v>(x: $crate::values::Value<'v>) -> Option<&'v Self> {
                    $crate::values::ValueLike::downcast_ref::< $x >(x)
                }
            }

            impl<'v> $crate::values::UnpackValue<'v> for &'v $x {
                fn expected() -> String {
                    <$x as $crate::values::StarlarkValue>::get_type_value_static().as_str().to_owned()
                }

                fn unpack_value(x: $crate::values::Value<'v>) -> Option<&'v $x> {
                    $x::from_value(x)
                }
            }
        }
    };
}
