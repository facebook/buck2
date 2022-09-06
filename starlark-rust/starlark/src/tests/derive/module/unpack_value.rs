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

use either::Either;
use itertools::Itertools;

use crate as starlark;
use crate::assert::Assert;
use crate::environment::GlobalsBuilder;
use crate::values::dict::DictOf;
use crate::values::list::ListOf;
use crate::values::structs::StructOf;
use crate::values::Value;
use crate::values::ValueOf;

// TODO(nmj): Figure out default values here. ValueOf<i32> = 5 should work.
#[starlark_module]
fn validate_module(builder: &mut GlobalsBuilder) {
    fn with_int<'v>(v: ValueOf<'v, i32>) -> anyhow::Result<(Value<'v>, String)> {
        Ok((*v, format!("{}", v.typed)))
    }
    fn with_int_list<'v>(v: ListOf<'v, i32>) -> anyhow::Result<(Value<'v>, String)> {
        let repr = v.to_vec().iter().join(", ");
        Ok((*v, repr))
    }
    fn with_list_list<'v>(v: ListOf<'v, ListOf<'v, i32>>) -> anyhow::Result<(Value<'v>, String)> {
        let repr = v
            .to_vec()
            .iter()
            .map(|l| l.to_vec().iter().join(", "))
            .join(" + ");
        Ok((*v, repr))
    }
    fn with_dict_list<'v>(
        v: ListOf<'v, DictOf<'v, i32, i32>>,
    ) -> anyhow::Result<(Value<'v>, String)> {
        let repr = v
            .to_vec()
            .iter()
            .map(|l| {
                l.to_dict()
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .join(", ")
            })
            .join(" + ");
        Ok((*v, repr))
    }
    fn with_int_dict<'v>(v: DictOf<'v, i32, i32>) -> anyhow::Result<(Value<'v>, String)> {
        let repr = v
            .to_dict()
            .iter()
            .map(|(k, v)| format!("{}: {}", k, v))
            .join(" + ");
        Ok((*v, repr))
    }
    fn with_list_dict<'v>(
        v: DictOf<'v, i32, ListOf<'v, i32>>,
    ) -> anyhow::Result<(Value<'v>, String)> {
        let repr = v
            .to_dict()
            .iter()
            .map(|(k, v)| format!("{}: {}", k, v.to_vec().iter().join(", ")))
            .join(" + ");
        Ok((*v, repr))
    }
    fn with_dict_dict<'v>(
        v: DictOf<'v, i32, DictOf<'v, i32, i32>>,
    ) -> anyhow::Result<(Value<'v>, String)> {
        let repr = v
            .to_dict()
            .iter()
            .map(|(k, v)| {
                let inner_repr = v
                    .to_dict()
                    .iter()
                    .map(|(k2, v2)| format!("{}:{}", k2, v2))
                    .join(", ");
                format!("{}: {}", k, inner_repr)
            })
            .join(" + ");
        Ok((*v, repr))
    }
    fn with_struct_int<'v>(v: StructOf<'v, i32>) -> anyhow::Result<(Value<'v>, String)> {
        let repr = v
            .to_map()
            .iter()
            .map(|(k, v)| format!("{}={}", k, v))
            .join(" + ");
        Ok((v.to_value(), repr))
    }
    fn with_either(v: Either<i32, Either<String, ListOf<i32>>>) -> anyhow::Result<String> {
        match v {
            Either::Left(i) => Ok(i.to_string()),
            Either::Right(nested) => match nested {
                Either::Left(s) => Ok(s),
                Either::Right(l) => Ok(l.to_repr()),
            },
        }
    }
}

// The standard error these raise on incorrect types
const BAD: &str = "Type of parameter";

#[test]
fn test_value_of() {
    let mut a = Assert::new();
    a.globals_add(validate_module);
    a.eq("(1, '1')", "with_int(1)");
    a.fail("with_int(None)", BAD);
}

#[test]
fn test_list_of() {
    let mut a = Assert::new();
    a.globals_add(validate_module);
    a.eq("([1, 2, 3], '1, 2, 3')", "with_int_list([1, 2, 3])");
    a.fail("with_int_list(1)", BAD);
    a.fail("with_int_list([1, 'foo'])", BAD);
    a.fail("with_int_list([[]])", BAD);

    a.eq(
        "([[1, 2], [3]], '1, 2 + 3')",
        "with_list_list([[1, 2], [3]])",
    );

    let expected = r#"([{1: 2, 3: 4}, {5: 6}], "1: 2, 3: 4 + 5: 6")"#;
    let test = r#"with_dict_list([{1: 2, 3: 4}, {5: 6}])"#;
    a.eq(expected, test);
}

#[test]
fn test_dict_of() {
    let mut a = Assert::new();
    a.globals_add(validate_module);
    a.eq("({1: 2}, '1: 2')", "with_int_dict({1: 2})");
    a.fail(r#"with_int_dict(1)"#, BAD);
    a.fail(r#"with_int_dict({1: "str"})"#, BAD);
    a.fail(r#"with_int_dict({1: {}})"#, BAD);

    let expected = r#"({1: [2, 3], 4: [5]}, "1: 2, 3 + 4: 5")"#;
    let test = r#"with_list_dict({1: [2, 3], 4: [5]})"#;
    a.eq(expected, test);

    let expected = r#"({1: {2: 3, 4: 5}, 6: {7: 8}}, "1: 2:3, 4:5 + 6: 7:8")"#;
    let test = r#"with_dict_dict({1: {2: 3, 4: 5}, 6: {7: 8}})"#;
    a.eq(expected, test);
}

#[test]
fn test_struct_of() {
    let mut a = Assert::new();
    a.globals_add(validate_module);
    a.eq("(struct(a=1), '\"a\"=1')", "with_struct_int(struct(a=1))");
    a.fail("with_struct_int(struct(a=True))", BAD);
}

#[test]
fn test_either_of() {
    let mut a = Assert::new();
    a.globals_add(validate_module);
    a.eq("'2'", "with_either(2)");
    a.eq("'[2, 3]'", "with_either([2,3])");
    a.eq("'s'", "with_either('s')");
    a.fail("with_either(None)", BAD);
    a.fail("with_either({})", BAD);
}
