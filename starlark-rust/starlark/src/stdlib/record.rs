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

//! Implementation of `record` function.

use dupe::Dupe;
use starlark_derive::starlark_module;

use crate as starlark;
use crate::collections::SmallMap;
use crate::environment::GlobalsBuilder;
use crate::values::record::Field;
use crate::values::record::RecordType;
use crate::values::typing::TypeCompiled;
use crate::values::Heap;
use crate::values::Value;

#[starlark_module]
pub fn global(builder: &mut GlobalsBuilder) {
    /// A `record` type represents a set of named values, each with their own type.
    ///
    /// For example:
    ///
    /// ```python
    /// MyRecord = record(host=str.type, port=int.type)
    /// ```
    ///
    /// This above statement defines a record `MyRecord` with 2 fields, the first named `host` that must be of type `str.type`, and the second named `port` that must be of type `int.type`.
    ///
    /// Now `MyRecord` is defined, it's possible to do the following:
    ///
    /// * Create values of this type with `MyRecord(host="localhost", port=80)`. It is a runtime error if any arguments are missed, of the wrong type, or if any unexpected arguments are given.
    /// * Get the type of the record suitable for a type annotation with `MyRecord.type`.
    /// * Get the fields of the record. For example, `v = MyRecord(host="localhost", port=80)` will provide `v.host == "localhost"` and `v.port == 80`. Similarly, `dir(v) == ["host", "port"]`.
    ///
    /// It is also possible to specify default values for parameters using the `field` function.
    ///
    /// For example:
    ///
    /// ```python
    /// MyRecord = record(host=str.type, port=field(int.type, 80))
    /// ```
    ///
    /// Now the `port` field can be omitted, defaulting to `80` is not present (for example, `MyRecord(host="localhost").port == 80`).
    ///
    /// Records are stored deduplicating their field names, making them more memory efficient than dictionaries.
    fn record<'v>(
        #[starlark(kwargs)] kwargs: SmallMap<String, Value<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<RecordType<'v>> {
        // Every Value must either be a field or a value (the type)
        let mut mp = SmallMap::with_capacity(kwargs.len());
        for (k, v) in kwargs.into_iter_hashed() {
            let field = match Field::from_value(v) {
                None => Field::new(TypeCompiled::new(v, heap)?, None),
                Some(v) => v.dupe(),
            };
            mp.insert_hashed(k, field);
        }
        Ok(RecordType::new(mp))
    }

    /// Creates a field record. Used as an argument to the `record` function.
    ///
    /// ```
    /// # starlark::assert::is_true(r#"
    /// rec_type = record(host=field(str.type), port=field(int.type), mask=field(int.type, default=255))
    /// rec = rec_type(host="localhost", port=80)
    /// rec.port == 80
    /// rec.mask == 255
    /// # "#);
    /// ```
    fn field<'v>(
        #[starlark(require = pos)] typ: Value<'v>,
        default: Option<Value<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<Field<'v>> {
        // We compile the type even if we don't have a default to raise the error sooner
        let compiled = TypeCompiled::new(typ, heap)?;
        if let Some(d) = default {
            compiled.check_type(d, Some("default"))?;
        }
        Ok(Field::new(compiled, default))
    }
}

#[cfg(test)]
mod tests {
    use crate::assert;
    use crate::assert::Assert;

    #[test]
    fn test_record() {
        assert::pass(
            r#"
rec_type = record(host=str.type, port=int.type)
rec1 = rec_type(host = "test", port=80)
rec2 = rec_type(host = "test", port=90)
assert_eq(rec1, rec1)
assert_eq(rec1 == rec2, False)
assert_eq(rec1.host, "test")
assert_eq(rec1.port, 80)
assert_eq(dir(rec1), ["host", "port"])
"#,
        );
        assert::fails(
            r#"
rec_type = record(host=str.type, port=int.type)
rec_type(host=1, port=80)
"#,
            &["`1`", "`str.type`", "`host`"],
        );
        assert::fails(
            r#"
rec_type = record(host=str.type, port=int.type)
rec_type(port=80)
"#,
            &["Missing parameter", "`host`"],
        );
        assert::fails(
            r#"
rec_type = record(host=str.type, port=int.type)
rec_type(host="localhost", port=80, mask=255)
"#,
            &["extra named", "mask"],
        );
        assert::pass(
            r#"
rec_type = record(host=str.type, port=int.type)
def foo(x: rec_type.type) -> "rec_type":
    return x
foo(rec_type(host="localhost", port=80))"#,
        );
        assert::pass(
            r#"
v = [record(host=str.type, port=int.type)]
v_0 = v[0]
def foo(x: v_0.type) -> "record":
    return x
foo(v[0](host="localhost", port=80))"#,
        );
        assert::pass(
            r#"
rec_type = record(host=str.type, port=field(int.type, 80), mask=int.type)
assert_eq(rec_type(host="localhost", mask=255), rec_type(host="localhost", port=80, mask=255))"#,
        );
        // Make sure the default value is heap allocated (used to fail with a GC issue)
        assert::pass(
            r#"
heap_string = "test{}".format(42)
rec_type = record(test_gc=field(str.type, heap_string))
assert_eq(rec_type().test_gc, "test42")"#,
        );
    }

    #[test]
    fn test_record_equality() {
        assert::pass(
            r#"
rec_type = record(host=str.type, port=field(int.type, 80))
assert_eq(rec_type(host="s"), rec_type(host="s"))
assert_eq(rec_type(host="s"), rec_type(host="s", port=80))
assert_ne(rec_type(host="s"), rec_type(host="t"))
"#,
        );

        let mut a = Assert::new();
        a.module(
            "m",
            r#"
rec_type = record(host=str.type, port=field(int.type, 80))
rec_val = rec_type(host="s")
"#,
        );
        a.pass(
            r#"
load('m', 'rec_type', 'rec_val')
assert_eq(rec_val, rec_type(host="s"))
assert_ne(rec_val, rec_type(host="t"))
"#,
        );

        a = Assert::new();
        a.module(
            "m",
            r#"
rt = record(host=str.type)
"#,
        );
        a.pass(
            r#"
load('m', r1='rt')
rt = record(host=str.type)
diff = record(host=str.type)
assert_eq(r1(host="test"), rt(host="test"))
assert_ne(r1(host="test"), diff(host="test"))
"#,
        );
    }

    #[test]
    fn test_field_invalid() {
        assert::fails(
            "field(str.type, None)",
            &["does not match the type", "`default`"],
        );
        assert::fails("field(True)", &["`True`", "not a valid type"]);
    }
}
