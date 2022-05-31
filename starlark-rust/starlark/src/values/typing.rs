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

use std::fmt::{self, Debug};

use gazebo::{coerce::Coerce, prelude::*};
use thiserror::Error;

use crate::values::{
    dict::{Dict, DictRef},
    list::{List, ListRef},
    tuple::{Tuple, TupleGen},
    Heap, Trace, Tracer, Value,
};

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

pub(crate) struct TypeCompiled(Box<dyn for<'v> Fn(Value<'v>) -> bool + Send + Sync>);

unsafe impl Coerce<TypeCompiled> for TypeCompiled {}

unsafe impl<'v> Trace<'v> for TypeCompiled {
    fn trace(&mut self, _tracer: &Tracer<'v>) {
        // Nothing stored here
    }
}

impl Debug for TypeCompiled {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("TypeCompiled")
    }
}

// These functions are small, but are deliberately out-of-line so we get better
// information in profiling about the origin of these closures
impl TypeCompiled {
    fn type_anything() -> TypeCompiled {
        TypeCompiled(box |_| true)
    }

    fn type_none() -> TypeCompiled {
        TypeCompiled(box |v| v.is_none())
    }

    fn type_string() -> TypeCompiled {
        TypeCompiled(box |v| v.unpack_str().is_some() || v.get_ref().matches_type("string"))
    }

    fn type_int() -> TypeCompiled {
        TypeCompiled(box |v| v.unpack_int().is_some() || v.get_ref().matches_type("int"))
    }

    fn type_bool() -> TypeCompiled {
        TypeCompiled(box |v| v.unpack_bool().is_some() || v.get_ref().matches_type("bool"))
    }

    fn type_concrete(t: &str) -> TypeCompiled {
        let t = t.to_owned();
        TypeCompiled(box move |v| v.get_ref().matches_type(&t))
    }

    fn type_list() -> TypeCompiled {
        TypeCompiled(box |v| List::from_value(v).is_some())
    }

    fn type_list_of(t: TypeCompiled) -> TypeCompiled {
        TypeCompiled(box move |v| match List::from_value(v) {
            None => false,
            Some(v) => v.iter().all(|v| (t.0)(v)),
        })
    }

    fn type_any_of_two(t1: TypeCompiled, t2: TypeCompiled) -> TypeCompiled {
        TypeCompiled(box move |v| (t1.0)(v) || (t2.0)(v))
    }

    fn type_any_of(ts: Vec<TypeCompiled>) -> TypeCompiled {
        TypeCompiled(box move |v| ts.iter().any(|t| (t.0)(v)))
    }

    fn type_dict() -> TypeCompiled {
        TypeCompiled(box |v| Dict::from_value(v).is_some())
    }

    fn type_dict_of(kt: TypeCompiled, vt: TypeCompiled) -> TypeCompiled {
        TypeCompiled(box move |v| match Dict::from_value(v) {
            None => false,
            Some(v) => v.iter().all(|(k, v)| (kt.0)(k) && (vt.0)(v)),
        })
    }

    fn type_tuple_of(ts: Vec<TypeCompiled>) -> TypeCompiled {
        TypeCompiled(box move |v| match Tuple::from_value(v) {
            Some(v) if v.len() == ts.len() => v.iter().zip(ts.iter()).all(|(v, t)| (t.0)(v)),
            _ => false,
        })
    }
}

impl TypeCompiled {
    /// Types that are `""` or start with `"_"` are wildcard - they match everything.
    fn is_wildcard(x: &str) -> bool {
        x == "" || x.starts_with('_')
    }

    fn is_wildcard_value(x: Value) -> bool {
        x.unpack_str().map(TypeCompiled::is_wildcard) == Some(true)
    }

    /// For `p: "xxx"`, parse that `"xxx"` as type.
    fn from_str(t: &str) -> TypeCompiled {
        if TypeCompiled::is_wildcard(t) {
            TypeCompiled::type_anything()
        } else {
            match t {
                "string" => TypeCompiled::type_string(),
                "int" => TypeCompiled::type_int(),
                "bool" => TypeCompiled::type_bool(),
                t => TypeCompiled::type_concrete(t),
            }
        }
    }

    fn from_tuple<'v>(t: &TupleGen<Value<'v>>, heap: &'v Heap) -> anyhow::Result<TypeCompiled> {
        let ts = t.content().try_map(|t| TypeCompiled::new(*t, heap))?;
        Ok(TypeCompiled::type_tuple_of(ts))
    }

    /// Parse `[t1, t2, ...]` as type.
    fn from_list<'v>(t: &ListRef<'v>, heap: &'v Heap) -> anyhow::Result<TypeCompiled> {
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
                    Ok(TypeCompiled::type_list_of(t))
                }
            }
            2 => {
                // A union type, can match either - special case of the arbitrary choice to go slightly faster
                let t1 = TypeCompiled::new(t[0], heap)?;
                let t2 = TypeCompiled::new(t[1], heap)?;
                Ok(TypeCompiled::type_any_of_two(t1, t2))
            }
            _ => {
                // A union type, can match any
                let ts = t[..].try_map(|t| TypeCompiled::new(*t, heap))?;
                Ok(TypeCompiled::type_any_of(ts))
            }
        }
    }

    fn from_dict<'v>(t: DictRef<'v>, heap: &'v Heap) -> anyhow::Result<TypeCompiled> {
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
                Ok(TypeCompiled::type_dict_of(tk, tv))
            }
        } else {
            // Dict type with zero or multiple fields is not allowed
            Err(TypingError::InvalidTypeAnnotation(t.to_string()).into())
        }
    }

    pub(crate) fn new<'v>(ty: Value<'v>, heap: &'v Heap) -> anyhow::Result<Self> {
        if let Some(s) = ty.unpack_str() {
            Ok(TypeCompiled::from_str(s))
        } else if ty.is_none() {
            Ok(TypeCompiled::type_none())
        } else if let Some(t) = Tuple::from_value(ty) {
            TypeCompiled::from_tuple(t, heap)
        } else if let Some(t) = List::from_value(ty) {
            TypeCompiled::from_list(t, heap)
        } else if let Some(t) = Dict::from_value(ty) {
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
        Ok(TypeCompiled::new(ty, heap)?.0(self))
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
        ty_compiled: &TypeCompiled,
        arg_name: Option<&str>,
    ) -> anyhow::Result<()> {
        if ty_compiled.0(self) {
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
        a.fail("def f(i: fail('bad')):\n pass", "bad");

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
