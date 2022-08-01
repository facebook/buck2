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

use std::cmp::Ordering;
use std::fmt;
use std::fmt::Display;
use std::hash::Hash;
use std::marker;
use std::marker::PhantomData;

use gazebo::any::ProvidesStaticType;
use gazebo::coerce::coerce;
use gazebo::coerce::Coerce;
use gazebo::display::display_keyed_container;
use serde::Serialize;

use crate as starlark;
use crate::collections::Hashed;
use crate::collections::SmallMap;
use crate::collections::StarlarkHasher;
use crate::values::comparison::compare_small_map;
use crate::values::comparison::equals_small_map;
use crate::values::docs;
use crate::values::docs::DocItem;
use crate::values::error::ValueError;
use crate::values::layout::typed::string::StringValueLike;
use crate::values::type_repr::StarlarkTypeRepr;
use crate::values::AllocValue;
use crate::values::Freeze;
use crate::values::FrozenValue;
use crate::values::Heap;
use crate::values::StarlarkValue;
use crate::values::StringValue;
use crate::values::Trace;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueLike;
use crate::values::ValueOf;

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
#[derive(Clone, Default, Debug, Trace, Freeze, ProvidesStaticType, StarlarkDocs)]
#[starlark_docs_attrs(builtin = "extension")]
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

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for StructGen<'v, V>
where
    Self: ProvidesStaticType,
{
    starlark_type!(Struct::TYPE);

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
                equals_small_map(coerce(&self.fields), &other.fields, |x, y| x.equals(*y))
            }
        }
    }

    fn compare(&self, other: Value<'v>) -> anyhow::Result<Ordering> {
        match Struct::from_value(other) {
            None => ValueError::unsupported_with(self, "cmp()", other),
            Some(other) => compare_small_map(
                coerce(&self.fields),
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
        coerce(&self.fields).get_hashed(attribute).copied()
    }

    fn write_hash(&self, hasher: &mut StarlarkHasher) -> anyhow::Result<()> {
        for (k, v) in self.fields.iter_hashed() {
            Hash::hash(&k, hasher);
            v.write_hash(hasher)?;
        }
        Ok(())
    }

    fn has_attr(&self, attribute: &str) -> bool {
        coerce(&self.fields).contains_key(attribute)
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

impl<'v, V: ValueLike<'v>> Serialize for StructGen<'v, V> {
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

impl<'v, V: UnpackValue<'v>> StarlarkTypeRepr for StructOf<'v, V> {
    fn starlark_type_repr() -> String {
        Struct::TYPE.to_owned()
    }
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
    use crate::assert;
    use crate::values::docs;
    use crate::values::docs::DocItem;
    use crate::values::docs::DocString;
    use crate::values::docs::DocStringKind;

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
            "l = []; s = struct(f=l); l.append(s); json.encode(s)",
            "Cycle detected when serializing value of type `struct` to JSON",
        );
    }

    #[test]
    fn test_to_json() {
        assert::all_true(
            r#"
json.encode(struct(key = None)) == '{"key":null}'
json.encode(struct(key = True)) == '{"key":true}'
json.encode(struct(key = False)) == '{"key":false}'
json.encode(struct(key = 42)) == '{"key":42}'
json.encode(struct(key = 'value')) == '{"key":"value"}'
json.encode(struct(key = 'value"')) == '{"key":"value\\\""}'
json.encode(struct(key = 'value\\')) == '{"key":"value\\\\"}'
json.encode(struct(key = 'value/')) == '{"key":"value/"}'
json.encode(struct(key = 'value\u0008')) == '{"key":"value\\b"}'
json.encode(struct(key = 'value\u000C')) == '{"key":"value\\f"}'
json.encode(struct(key = 'value\n')) == '{"key":"value\\n"}'
json.encode(struct(key = 'value\r')) == '{"key":"value\\r"}'
json.encode(struct(key = 'value\t')) == '{"key":"value\\t"}'
json.encode(struct(foo = 42, bar = "some")) == '{"foo":42,"bar":"some"}'
json.encode(struct(foo = struct(bar = "some"))) == '{"foo":{"bar":"some"}}'
json.encode(struct(foo = ["bar/", "some"])) == '{"foo":["bar/","some"]}'
json.encode(struct(foo = [struct(bar = "some")])) == '{"foo":[{"bar":"some"}]}'
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
                                raw_type: "\"x\"".to_owned(),
                            }),
                            default_value: None,
                        }],
                        ret: docs::Return {
                            docs: None,
                            typ: Some(docs::Type {
                                raw_type: "\"y\"".to_owned(),
                            }),
                        },
                    }),
                ),
            ],
        });

        let s = assert::pass(
            r#"
def some_func(v: "x") -> "y":
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
