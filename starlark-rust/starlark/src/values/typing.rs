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

use std::fmt::Debug;

use allocative::Allocative;
use dupe::Dupe;
use thiserror::Error;

use crate as starlark;
use crate::any::ProvidesStaticType;
use crate::coerce::Coerce;
use crate::private::Private;
use crate::slice_vec_ext::SliceExt;
use crate::starlark_type;
use crate::values::dict::Dict;
use crate::values::dict::DictRef;
use crate::values::layout::avalue::alloc_static;
use crate::values::layout::avalue::AValueImpl;
use crate::values::layout::avalue::Basic;
use crate::values::layout::heap::repr::AValueRepr;
use crate::values::list::ListRef;
use crate::values::types::tuple::value::Tuple;
use crate::values::types::tuple::value::TupleGen;
use crate::values::Freeze;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::NoSerialize;
use crate::values::StarlarkValue;
use crate::values::Trace;
use crate::values::Value;
use crate::values::ValueLike;

#[derive(Debug, Error)]
enum TypingError {
    /// The value does not have the specified type
    #[error("Value `{0}` of type `{1}` does not match the type annotation `{2}` for {3}")]
    TypeAnnotationMismatch(String, String, String, String),
    /// The given type annotation does not represent a type
    #[error("Type `{0}` is not a valid type annotation")]
    InvalidTypeAnnotation(String),
    /// The given type annotation does not exist, but the user might have forgotten quotes around
    /// it
    #[error(r#"Found `{0}` instead of a valid type annotation. Perhaps you meant `"{1}"`?"#)]
    PerhapsYouMeant(String, String),
}

trait TypeCompiledImpl<'v>: Allocative + Debug + ProvidesStaticType + Sized + 'v {
    fn matches(&self, value: Value<'v>) -> bool;
}

#[derive(
    Debug,
    Trace,
    Freeze,
    Allocative,
    derive_more::Display,
    ProvidesStaticType,
    NoSerialize
)]
#[display(fmt = "type")]
struct TypeCompiledImplAsStarlarkValue<T>(T);

impl<T> TypeCompiledImplAsStarlarkValue<T>
where
    TypeCompiledImplAsStarlarkValue<T>: StarlarkValue<'static>,
{
    const fn alloc_static(imp: T) -> AValueRepr<AValueImpl<Basic, Self>> {
        alloc_static(Basic, TypeCompiledImplAsStarlarkValue(imp))
    }
}

impl<'v, T> StarlarkValue<'v> for TypeCompiledImplAsStarlarkValue<T>
where
    T: TypeCompiledImpl<'v>,
    Self: ProvidesStaticType,
{
    starlark_type!("eval_type");

    fn type_matches_value(&self, value: Value<'v>, _private: Private) -> bool {
        self.0.matches(value)
    }
}

#[derive(
    Debug,
    derive_more::Display,
    Allocative,
    Freeze,
    Trace,
    Clone,
    Copy,
    Dupe,
    Coerce,
    ProvidesStaticType
)]
#[repr(transparent)]
pub(crate) struct TypeCompiled<V>(
    /// `V` is a starlark value which implements `type_matches_value`.
    /// Such values are not visible to the user.
    V,
);

impl<'v, V: ValueLike<'v>> TypeCompiled<V> {
    fn matches(&self, value: Value<'v>) -> bool {
        self.0.to_value().get_ref().type_matches_value(value)
    }

    pub(crate) fn to_value(self) -> TypeCompiled<Value<'v>> {
        TypeCompiled(self.0.to_value())
    }
}

// These functions are small, but are deliberately out-of-line so we get better
// information in profiling about the origin of these closures
impl<'v> TypeCompiled<Value<'v>> {
    fn type_anything() -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, ProvidesStaticType)]
        struct Anything;

        impl<'v> TypeCompiledImpl<'v> for Anything {
            fn matches(&self, _value: Value<'v>) -> bool {
                true
            }
        }

        static ANYTHING: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<Anything>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(Anything);

        TypeCompiled(FrozenValue::new_repr(&ANYTHING).to_value())
    }

    fn type_none() -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, ProvidesStaticType)]
        struct IsNone;

        impl<'v> TypeCompiledImpl<'v> for IsNone {
            fn matches(&self, value: Value<'v>) -> bool {
                value.is_none()
            }
        }

        static IS_NONE: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsNone>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsNone);

        TypeCompiled(FrozenValue::new_repr(&IS_NONE).to_value())
    }

    fn type_string() -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, ProvidesStaticType)]
        struct IsString;

        impl<'v> TypeCompiledImpl<'v> for IsString {
            fn matches(&self, value: Value<'v>) -> bool {
                value.unpack_str().is_some() || value.get_ref().matches_type("string")
            }
        }

        static IS_STRING: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsString>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsString);

        TypeCompiled(FrozenValue::new_repr(&IS_STRING).to_value())
    }

    fn type_int() -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, ProvidesStaticType)]
        struct IsInt;

        impl<'v> TypeCompiledImpl<'v> for IsInt {
            fn matches(&self, value: Value<'v>) -> bool {
                value.unpack_inline_int().is_some() || value.get_ref().matches_type("int")
            }
        }

        static IS_INT: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsInt>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsInt);

        TypeCompiled(FrozenValue::new_repr(&IS_INT).to_value())
    }

    fn type_bool() -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, ProvidesStaticType)]
        struct IsBool;

        impl<'v> TypeCompiledImpl<'v> for IsBool {
            fn matches(&self, value: Value<'v>) -> bool {
                value.unpack_bool().is_some() || value.get_ref().matches_type("bool")
            }
        }

        static IS_BOOL: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsBool>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsBool);

        TypeCompiled(FrozenValue::new_repr(&IS_BOOL).to_value())
    }

    fn type_concrete(t: &str, heap: &'v Heap) -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, Trace, Freeze, ProvidesStaticType)]
        struct IsConcrete(String);

        impl<'v> TypeCompiledImpl<'v> for IsConcrete {
            fn matches(&self, value: Value<'v>) -> bool {
                value.get_ref().matches_type(&self.0)
            }
        }

        TypeCompiled(heap.alloc_simple(TypeCompiledImplAsStarlarkValue(IsConcrete(t.to_owned()))))
    }

    fn type_list() -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, ProvidesStaticType)]
        struct IsList;

        impl<'v> TypeCompiledImpl<'v> for IsList {
            fn matches(&self, value: Value<'v>) -> bool {
                ListRef::from_value(value).is_some()
            }
        }

        static IS_LIST: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsList>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsList);

        TypeCompiled(FrozenValue::new_repr(&IS_LIST).to_value())
    }

    fn type_list_of(t: TypeCompiled<Value<'v>>, heap: &'v Heap) -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, Trace, Freeze, ProvidesStaticType)]
        struct IsListOf<V>(TypeCompiled<V>);

        impl<'v, V: ValueLike<'v>> TypeCompiledImpl<'v> for IsListOf<V>
        where
            Self: ProvidesStaticType,
        {
            fn matches(&self, value: Value<'v>) -> bool {
                match ListRef::from_value(value) {
                    None => false,
                    Some(list) => list.iter().all(|v| self.0.matches(v)),
                }
            }
        }

        TypeCompiled(heap.alloc_complex(TypeCompiledImplAsStarlarkValue(IsListOf(t))))
    }

    fn type_any_of_two(
        t1: TypeCompiled<Value<'v>>,
        t2: TypeCompiled<Value<'v>>,
        heap: &'v Heap,
    ) -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, Trace, Freeze, ProvidesStaticType)]
        struct IsAnyOfTwo<V>(TypeCompiled<V>, TypeCompiled<V>);

        impl<'v, V: ValueLike<'v>> TypeCompiledImpl<'v> for IsAnyOfTwo<V>
        where
            Self: ProvidesStaticType,
        {
            fn matches(&self, value: Value<'v>) -> bool {
                self.0.matches(value) || self.1.matches(value)
            }
        }

        TypeCompiled(heap.alloc_complex(TypeCompiledImplAsStarlarkValue(IsAnyOfTwo(t1, t2))))
    }

    fn type_any_of(ts: Vec<TypeCompiled<Value<'v>>>, heap: &'v Heap) -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, Trace, Freeze, ProvidesStaticType)]
        struct IsAnyOf<V>(Vec<TypeCompiled<V>>);

        impl<'v, V: ValueLike<'v>> TypeCompiledImpl<'v> for IsAnyOf<V>
        where
            Self: ProvidesStaticType,
        {
            fn matches(&self, value: Value<'v>) -> bool {
                self.0.iter().any(|t| t.matches(value))
            }
        }

        TypeCompiled(heap.alloc_complex(TypeCompiledImplAsStarlarkValue(IsAnyOf(ts))))
    }

    fn type_dict() -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, ProvidesStaticType)]
        struct IsDict;

        impl<'v> TypeCompiledImpl<'v> for IsDict {
            fn matches(&self, value: Value<'v>) -> bool {
                DictRef::from_value(value).is_some()
            }
        }

        static IS_DICT: AValueRepr<AValueImpl<Basic, TypeCompiledImplAsStarlarkValue<IsDict>>> =
            TypeCompiledImplAsStarlarkValue::alloc_static(IsDict);

        TypeCompiled(FrozenValue::new_repr(&IS_DICT).to_value())
    }

    fn type_dict_of(
        kt: TypeCompiled<Value<'v>>,
        vt: TypeCompiled<Value<'v>>,
        heap: &'v Heap,
    ) -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, Trace, Freeze, ProvidesStaticType)]
        struct IsDictOf<V>(TypeCompiled<V>, TypeCompiled<V>);

        impl<'v, V: ValueLike<'v>> TypeCompiledImpl<'v> for IsDictOf<V>
        where
            Self: ProvidesStaticType,
        {
            fn matches(&self, value: Value<'v>) -> bool {
                match DictRef::from_value(value) {
                    None => false,
                    Some(dict) => dict
                        .iter()
                        .all(|(k, v)| self.0.matches(k) && self.1.matches(v)),
                }
            }
        }

        TypeCompiled(heap.alloc_complex(TypeCompiledImplAsStarlarkValue(IsDictOf(kt, vt))))
    }

    fn type_tuple_of(ts: Vec<TypeCompiled<Value<'v>>>, heap: &'v Heap) -> TypeCompiled<Value<'v>> {
        #[derive(Allocative, Debug, Trace, Freeze, ProvidesStaticType)]
        struct IsTupleOf<V>(Vec<TypeCompiled<V>>);

        impl<'v, V: ValueLike<'v>> TypeCompiledImpl<'v> for IsTupleOf<V>
        where
            Self: ProvidesStaticType,
        {
            fn matches(&self, value: Value<'v>) -> bool {
                match Tuple::from_value(value) {
                    Some(v) if v.len() == self.0.len() => {
                        v.iter().zip(self.0.iter()).all(|(v, t)| t.matches(v))
                    }
                    _ => false,
                }
            }
        }

        TypeCompiled(heap.alloc_complex(TypeCompiledImplAsStarlarkValue(IsTupleOf(ts))))
    }

    /// Types that are `""` or start with `"_"` are wildcard - they match everything.
    fn is_wildcard(x: &str) -> bool {
        x == "" || x.starts_with('_')
    }

    pub(crate) fn is_wildcard_value(x: Value) -> bool {
        x.unpack_str().map(TypeCompiled::is_wildcard) == Some(true)
    }

    /// For `p: "xxx"`, parse that `"xxx"` as type.
    fn from_str(t: &str, heap: &'v Heap) -> TypeCompiled<Value<'v>> {
        if TypeCompiled::is_wildcard(t) {
            TypeCompiled::type_anything()
        } else {
            match t {
                "string" => TypeCompiled::type_string(),
                "int" => TypeCompiled::type_int(),
                "bool" => TypeCompiled::type_bool(),
                t => TypeCompiled::type_concrete(t, heap),
            }
        }
    }

    fn from_tuple(
        t: &TupleGen<Value<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<TypeCompiled<Value<'v>>> {
        let ts = t.content().try_map(|t| TypeCompiled::new(*t, heap))?;
        Ok(TypeCompiled::type_tuple_of(ts, heap))
    }

    /// Parse `[t1, t2, ...]` as type.
    fn from_list(t: &ListRef<'v>, heap: &'v Heap) -> anyhow::Result<TypeCompiled<Value<'v>>> {
        match t.len() {
            0 => Err(TypingError::InvalidTypeAnnotation(t.to_string()).into()),
            1 => {
                // Must be a list with all elements of this type
                let t = *t.first().unwrap();
                if TypeCompiled::is_wildcard_value(t) {
                    // Any type - so avoid the inner iteration
                    Ok(TypeCompiled::type_list())
                } else {
                    let t = TypeCompiled::new(t, heap)?;
                    Ok(TypeCompiled::type_list_of(t, heap))
                }
            }
            2 => {
                // A union type, can match either - special case of the arbitrary choice to go slightly faster
                let t1 = TypeCompiled::new(t[0], heap)?;
                let t2 = TypeCompiled::new(t[1], heap)?;
                Ok(TypeCompiled::type_any_of_two(t1, t2, heap))
            }
            _ => {
                // A union type, can match any
                let ts = t[..].try_map(|t| TypeCompiled::new(*t, heap))?;
                Ok(TypeCompiled::type_any_of(ts, heap))
            }
        }
    }

    fn from_dict(t: DictRef<'v>, heap: &'v Heap) -> anyhow::Result<TypeCompiled<Value<'v>>> {
        // Dictionary with a single element
        fn unpack_singleton_dictionary<'v>(x: &Dict<'v>) -> Option<(Value<'v>, Value<'v>)> {
            if x.len() == 1 { x.iter().next() } else { None }
        }

        if let Some((tk, tv)) = unpack_singleton_dictionary(&t) {
            if TypeCompiled::is_wildcard_value(tk) && TypeCompiled::is_wildcard_value(tv) {
                Ok(TypeCompiled::type_dict())
            } else {
                // Dict of the form {k: v} must all match the k/v types
                let tk = TypeCompiled::new(tk, heap)?;
                let tv = TypeCompiled::new(tv, heap)?;
                Ok(TypeCompiled::type_dict_of(tk, tv, heap))
            }
        } else {
            // Dict type with zero or multiple fields is not allowed
            Err(TypingError::InvalidTypeAnnotation(t.to_string()).into())
        }
    }

    pub(crate) fn new(ty: Value<'v>, heap: &'v Heap) -> anyhow::Result<Self> {
        if let Some(s) = ty.unpack_str() {
            Ok(TypeCompiled::from_str(s, heap))
        } else if ty.is_none() {
            Ok(TypeCompiled::type_none())
        } else if let Some(t) = Tuple::from_value(ty) {
            TypeCompiled::from_tuple(t, heap)
        } else if let Some(t) = ListRef::from_value(ty) {
            TypeCompiled::from_list(t, heap)
        } else if let Some(t) = DictRef::from_value(ty) {
            TypeCompiled::from_dict(t, heap)
        } else {
            Err(invalid_type_annotation(ty, heap).into())
        }
    }
}

fn invalid_type_annotation<'v>(ty: Value<'v>, heap: &'v Heap) -> TypingError {
    if let Some(name) = ty
        .get_attr("type", heap)
        .ok()
        .flatten()
        .and_then(|v| v.unpack_str())
    {
        TypingError::PerhapsYouMeant(ty.to_str(), name.into())
    } else {
        TypingError::InvalidTypeAnnotation(ty.to_str())
    }
}

impl<'v> Value<'v> {
    pub(crate) fn is_type(self, ty: Value<'v>, heap: &'v Heap) -> anyhow::Result<bool> {
        Ok(TypeCompiled::new(ty, heap)?.matches(self))
    }

    #[cold]
    #[inline(never)]
    fn check_type_error(value: Value, ty: Value, arg_name: Option<&str>) -> anyhow::Result<()> {
        Err(TypingError::TypeAnnotationMismatch(
            value.to_str(),
            value.get_type().to_owned(),
            ty.to_str(),
            match arg_name {
                None => "return type".to_owned(),
                Some(x) => format!("argument `{}`", x),
            },
        )
        .into())
    }

    pub(crate) fn check_type(
        self,
        ty: Value<'v>,
        arg_name: Option<&str>,
        heap: &'v Heap,
    ) -> anyhow::Result<()> {
        if self.is_type(ty, heap)? {
            Ok(())
        } else {
            Self::check_type_error(self, ty, arg_name)
        }
    }

    pub(crate) fn check_type_compiled(
        self,
        ty: Value<'v>,
        ty_compiled: TypeCompiled<Value<'v>>,
        arg_name: Option<&str>,
    ) -> anyhow::Result<()> {
        if ty_compiled.matches(self) {
            Ok(())
        } else {
            Self::check_type_error(self, ty, arg_name)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::assert;

    #[test]
    fn test_types() {
        let a = assert::Assert::new();
        a.is_true(
            r#"
def f(i: int.type) -> bool.type:
    return i == 3
f(8) == False"#,
        );

        // If the types are either malformed or runtime errors, it should fail
        a.fail("def f(i: made_up):\n pass", "Variable");
        a.fail(
            "def f(i: fail('bad')):\n pass",
            "call expression is not allowed in type expression",
        );

        // Type errors should be caught in arguments
        a.fails(
            "def f(i: bool.type):\n pass\nf(1)",
            &["type annotation", "`1`", "`int`", "`bool`", "`i`"],
        );
        // Type errors should be caught when the user forgets quotes around a valid type
        a.fail("def f(v: bool):\n pass\n", r#"Perhaps you meant `"bool"`"#);
        a.fails(
            r#"Foo = record(value=int.type)
def f(v: bool.type) -> Foo:
    return Foo(value=1)"#,
            &[r#"record(value=field("int"))"#, "Foo"],
        );
        a.fails(
            r#"Bar = enum("bar")
def f(v: Bar):
  pass"#,
            &[r#"enum("bar")"#, "Bar"],
        );
        // Type errors should be caught in return positions
        a.fails(
            "def f() -> bool.type:\n return 1\nf()",
            &["type annotation", "`1`", "`bool`", "`int`", "return"],
        );
        // And for functions without return
        a.fails(
            "def f() -> bool.type:\n pass\nf()",
            &["type annotation", "`None`", "`bool`", "return"],
        );
        // And for functions that return None implicitly or explicitly
        a.fails(
            "def f() -> None:\n return True\nf()",
            &["type annotation", "`None`", "`bool`", "return"],
        );
        a.pass("def f() -> None:\n pass\nf()");

        // The following are all valid types
        a.all_true(
            r#"
is_type(1, int.type)
is_type(True, bool.type)
is_type(True, "")
is_type(None, None)
is_type(assert_type, "function")
is_type([], [int.type])
is_type([], [""])
is_type([1, 2, 3], [int.type])
is_type(None, [None, int.type])
is_type('test', [int.type, str.type])
is_type(('test', None), (str.type, None))
is_type({"test": 1, "more": 2}, {str.type: int.type})
is_type({1: 1, 2: 2}, {int.type: int.type})

not is_type(1, None)
not is_type((1, 1), str.type)
not is_type('test', [int.type, bool.type])
not is_type([1,2,None], [int.type])
not is_type({"test": 1, 8: 2}, {str.type: int.type})
not is_type({"test": 1, "more": None}, {str.type: int.type})

is_type(1, "")
is_type([1,2,"test"], ["_a"])
"#,
        );

        // Checking types fails for invalid types
        a.fail("is_type(None, is_type)", "not a valid type");
        a.fail("is_type(None, [])", "not a valid type");
        a.fail("is_type(None, {'1': '', '2': ''})", "not a valid type");
        a.fail("is_type({}, {1: 'string', 2: 'bool'})", "not a valid type");

        // Should check the type of default parameters that aren't used
        a.fail(
            r#"
def foo(f: int.type = None):
    pass
"#,
            "`None` of type `NoneType` does not match the type annotation `int`",
        );
    }
}
