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

//! The struct type, an associative-map created with `struct()`.
//!
//! This struct type is related to both the [dictionary](crate::values::dict) and the
//! [record](crate::values::record) types, all being associative maps.
//!
//! * Like a record, a struct is immutable, fields can be referred to with `struct.field`, and
//!   it uses strings for keys.
//! * Like a dictionary, the struct is untyped, and manipulating structs from Rust is ergonomic.
//!
//! The `struct()` function creates a struct. It accepts keyword arguments, keys become
//! struct field names, and values become field values.
//!
//! ```
//! # starlark::assert::is_true(r#"
//! ip_address = struct(host='localhost', port=80)
//! ip_address.port == 80
//! # "#);
//! ```

use std::{
    cmp::Ordering,
    fmt::{self, Display},
    hash::Hash,
    marker,
    marker::PhantomData,
};

use gazebo::{
    any::{AnyLifetime, ProvidesStaticType},
    coerce::{coerce_ref, Coerce},
};
use serde::Serialize;

use crate::{
    self as starlark,
    collections::{Hashed, SmallMap, StarlarkHasher},
    environment::{Methods, MethodsStatic},
    values::{
        comparison::{compare_small_map, equals_small_map},
        display::display_keyed_container,
        docs,
        docs::DocItem,
        error::ValueError,
        layout::typed::string::StringValueLike,
        AllocValue, Freeze, FrozenValue, Heap, StarlarkValue, StringValue, Trace, UnpackValue,
        Value, ValueLike, ValueOf,
    },
};

impl<'v, V: ValueLike<'v>> StructGen<'v, V> {
    /// The result of calling `type()` on a struct.
    pub const TYPE: &'static str = "struct";

    /// Create a new [`Struct`].
    pub fn new(fields: SmallMap<V::String, V>) -> Self {
        Self {
            fields,
            _marker: marker::PhantomData,
        }
    }

    /// Iterate over the elements in the struct.
    pub fn iter<'a>(&'a self) -> impl ExactSizeIterator<Item = (StringValue<'v>, V)> + 'a
    where
        'v: 'a,
    {
        self.fields
            .iter()
            .map(|(name, value)| (name.to_string_value(), *value))
    }
}

starlark_complex_value!(pub Struct<'v>);

/// The result of calling `struct()`.
#[derive(Clone, Default, Debug, Trace, Freeze, AnyLifetime)]
#[repr(C)]
pub struct StructGen<'v, V: ValueLike<'v>> {
    /// The fields in a struct.
    pub fields: SmallMap<V::String, V>,
    _marker: marker::PhantomData<&'v String>,
}

unsafe impl<'v> Coerce<StructGen<'v, Value<'v>>> for StructGen<'static, FrozenValue> {}

impl<'v, V: ValueLike<'v>> Display for StructGen<'v, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        display_keyed_container(
            f,
            "struct(",
            ")",
            "=",
            self.iter().map(|(k, v)| (k.as_str(), v)),
        )
    }
}

/// A builder to create a `Struct` easily.
pub struct StructBuilder<'v>(&'v Heap, SmallMap<StringValue<'v>, Value<'v>>);

impl<'v> StructBuilder<'v> {
    /// Create a new [`StructBuilder`] with a given capacity.
    pub fn with_capacity(heap: &'v Heap, capacity: usize) -> Self {
        Self(heap, SmallMap::with_capacity(capacity))
    }

    /// Create a new [`StructBuilder`].
    pub fn new(heap: &'v Heap) -> Self {
        Self(heap, SmallMap::new())
    }

    /// Add an element to the underlying [`Struct`].
    pub fn add(&mut self, key: &str, val: impl AllocValue<'v>) {
        self.1.insert(self.0.alloc_str(key), self.0.alloc(val));
    }

    /// Finish building and produce a [`Struct`].
    pub fn build(self) -> Struct<'v> {
        Struct {
            fields: self.1,
            _marker: marker::PhantomData,
        }
    }
}

impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for StructGen<'v, V>
where
    Self: AnyLifetime<'v> + ProvidesStaticType,
{
    starlark_type!(Struct::TYPE);

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(crate::stdlib::structs::struct_methods)
    }

    fn extra_memory(&self) -> usize {
        self.fields.extra_memory()
    }

    fn collect_repr_cycle(&self, collector: &mut String) {
        collector.push_str("struct(...)");
    }

    fn equals(&self, other: Value<'v>) -> anyhow::Result<bool> {
        match Struct::from_value(other) {
            None => Ok(false),
            Some(other) => {
                equals_small_map(coerce_ref(&self.fields), &other.fields, |x, y| x.equals(*y))
            }
        }
    }

    fn compare(&self, other: Value<'v>) -> anyhow::Result<Ordering> {
        match Struct::from_value(other) {
            None => ValueError::unsupported_with(self, "cmp()", other),
            Some(other) => compare_small_map(
                coerce_ref(&self.fields),
                &other.fields,
                |k| k.as_str(),
                |x, y| x.compare(*y),
            ),
        }
    }

    fn get_attr(&self, attribute: &str, heap: &'v Heap) -> Option<Value<'v>> {
        self.get_attr_hashed(Hashed::new(attribute), heap)
    }

    fn get_attr_hashed(&self, attribute: Hashed<&str>, _heap: &'v Heap) -> Option<Value<'v>> {
        coerce_ref(&self.fields).get_hashed(attribute).copied()
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        for (k, v) in self.fields.iter_hashed() {
            Hash::hash(&k, hasher);
            v.write_hash(hasher)?;
        }
        Ok(())
    }

    fn has_attr(&self, attribute: &str) -> bool {
        coerce_ref(&self.fields).contains_key(attribute)
    }

    fn dir_attr(&self) -> Vec<String> {
        self.fields.keys().map(|x| x.as_str().to_owned()).collect()
    }

    fn documentation(&self) -> Option<DocItem> {
        let members = self
            .fields
            .iter()
            .map(|(k, v)| {
                let name = k.as_str().to_owned();
                match v.to_value().documentation() {
                    Some(DocItem::Function(f)) => (name, docs::Member::Function(f)),
                    _ => (
                        name,
                        docs::Member::Property(docs::Property {
                            docs: None,
                            typ: None,
                        }),
                    ),
                }
            })
            .collect();
        Some(DocItem::Object(docs::Object {
            docs: None,
            members,
        }))
    }
}

impl<'v, V: ValueLike<'v>> Serialize for StructGen<'v, V>
where
    Self: AnyLifetime<'v>,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.collect_map(self.iter())
    }
}

/// Like [`ValueOf`](crate::values::ValueOf), but only validates value types; does not construct
/// or store a map.
pub struct StructOf<'v, V: UnpackValue<'v>> {
    value: ValueOf<'v, &'v Struct<'v>>,
    _marker: PhantomData<V>,
}

impl<'v, V: UnpackValue<'v>> UnpackValue<'v> for StructOf<'v, V> {
    fn expected() -> String {
        format!("struct with fields of type {}", V::expected())
    }

    fn unpack_value(value: Value<'v>) -> Option<StructOf<'v, V>> {
        let value = ValueOf::<&Struct>::unpack_value(value)?;
        for (_k, &v) in &value.typed.fields {
            // Validate field types
            V::unpack_value(v)?;
        }
        Some(StructOf {
            value,
            _marker: marker::PhantomData,
        })
    }
}

impl<'v, V: UnpackValue<'v>> StructOf<'v, V> {
    /// Get the actual value this `StructOf` wraps.
    pub fn to_value(&self) -> Value<'v> {
        self.value.value
    }

    /// Get untyped struct reference.
    pub fn as_struct(&self) -> &Struct<'v> {
        self.value.typed
    }

    /// Collect field structs.
    pub fn to_map(&self) -> SmallMap<StringValue<'v>, V> {
        self.as_struct()
            .fields
            .iter()
            .map(|(&k, &v)| (k, V::unpack_value(v).expect("validated at construction")))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        assert,
        values::{
            docs,
            docs::{DocItem, DocString, DocStringKind},
        },
    };

    #[test]
    fn test_repr() {
        assert::eq("repr(struct(a=1, b=[]))", "'struct(a=1, b=[])'");
        assert::eq("str(struct(a=1, b=[]))", "'struct(a=1, b=[])'");
    }

    #[test]
    fn test_repr_cycle() {
        assert::eq(
            "l = []; s = struct(f=l); l.append(s); repr(s)",
            "'struct(f=[struct(...)])'",
        );
        assert::eq(
            "l = []; s = struct(f=l); l.append(s); str(s)",
            "'struct(f=[struct(...)])'",
        );
    }

    #[test]
    fn test_to_json_cycle() {
        assert::fail(
            "l = []; s = struct(f=l); l.append(s); s.to_json()",
            "Cycle detected when serializing value of type `struct` to JSON",
        );
    }

    #[test]
    fn test_to_json() {
        assert::all_true(
            r#"
struct(key = None).to_json() == '{"key":null}'
struct(key = True).to_json() == '{"key":true}'
struct(key = False).to_json() == '{"key":false}'
struct(key = 42).to_json() == '{"key":42}'
struct(key = 'value').to_json() == '{"key":"value"}'
struct(key = 'value"').to_json() == '{"key":"value\\\""}'
struct(key = 'value\\').to_json() == '{"key":"value\\\\"}'
struct(key = 'value/').to_json() == '{"key":"value/"}'
struct(key = 'value\u0008').to_json() == '{"key":"value\\b"}'
struct(key = 'value\u000C').to_json() == '{"key":"value\\f"}'
struct(key = 'value\n').to_json() == '{"key":"value\\n"}'
struct(key = 'value\r').to_json() == '{"key":"value\\r"}'
struct(key = 'value\t').to_json() == '{"key":"value\\t"}'
struct(foo = 42, bar = "some").to_json() == '{"foo":42,"bar":"some"}'
struct(foo = struct(bar = "some")).to_json() == '{"foo":{"bar":"some"}}'
struct(foo = ["bar/", "some"]).to_json() == '{"foo":["bar/","some"]}'
struct(foo = [struct(bar = "some")]).to_json() == '{"foo":[{"bar":"some"}]}'
"#,
        );
    }

    #[test]
    fn test_docs() {
        let expected = DocItem::Object(docs::Object {
            docs: None,
            members: vec![
                (
                    "member".to_owned(),
                    docs::Member::Property(docs::Property {
                        docs: None,
                        typ: None,
                    }),
                ),
                (
                    "some_func".to_owned(),
                    docs::Member::Function(docs::Function {
                        docs: DocString::from_docstring(DocStringKind::Starlark, "some_func docs"),
                        params: vec![docs::Param::Arg {
                            name: "v".to_owned(),
                            docs: None,
                            typ: Some(docs::Type {
                                raw_type: "\"\"".to_owned(),
                            }),
                            default_value: None,
                        }],
                        ret: docs::Return {
                            docs: None,
                            typ: Some(docs::Type {
                                raw_type: "\"\"".to_owned(),
                            }),
                        },
                    }),
                ),
            ],
        });

        let s = assert::pass(
            r#"
def some_func(v: "") -> "":
    """ some_func docs """
    return v

struct(
    member = "some string",
    some_func = some_func,
)"#,
        );
        let docs = s.value().documentation().expect("some docs");

        assert_eq!(expected, docs);
    }
}
