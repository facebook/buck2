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

//! Implementation of `enum` function.
use starlark_derive::starlark_module;

use crate as starlark;
use crate::environment::GlobalsBuilder;
use crate::values::enumeration::EnumType;
use crate::values::Heap;
use crate::values::Value;

#[starlark_module]
pub fn global(builder: &mut GlobalsBuilder) {
    /// The `enum` type represents one value picked from a set of values.
    ///
    /// For example:
    ///
    /// ```python
    /// MyEnum = enum("option1", "option2", True)
    /// ```
    ///
    /// This statement defines an enumeration `MyEnum` that consists of the three values `"option1"`, `"option2"` and `True`.
    ///
    /// Now `MyEnum` is defined, it's possible to do the following:
    ///
    /// * Create values of this type with `MyEnum("option2")`. It is a runtime error if the argument is not one of the predeclared values of the enumeration.
    /// * Get the type of the enum suitable for a type annotation with `MyEnum.type`.
    /// * Given a value of the enum (for example, `v = MyEnum("option2")`), get the underlying value `v.value == "option2"` or the index in the enumeration `v.index = 1`.
    /// * Get a list of the values that make up the array with `MyEnum.values() == ["option1", "option2", True]`.
    /// * Treat `MyEnum` a bit like an array, with `len(MyEnum) == 3`, `MyEnum[1] == MyEnum("option2")` and iteration over enums `[x.value for x in MyEnum] == ["option1", "option2", True]`.
    ///
    /// Enumeration types store each value once, which are then efficiently referenced by enumeration values.
    fn r#enum<'v>(
        #[starlark(args)] args: Vec<Value<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        // Every Value must either be a field or a value (the type)
        EnumType::new(args, heap)
    }
}

#[cfg(test)]
mod tests {
    use crate::assert;
    use crate::assert::Assert;

    #[test]
    fn test_enum() {
        assert::pass(
            r#"
enum_type = enum("option1", "option2", True)
x = enum_type("option1")
assert_eq(x.value, "option1")
assert_eq(enum_type(True).value, True)
assert_eq(enum_type.type, "enum_type")
"#,
        );
        assert::fails(
            r#"
enum_type = enum("option1", "option2", True)
enum_type(False)"#,
            &["Unknown enum element", "`False`", "option1"],
        );
        assert::fails(
            r#"
enum_type = enum("option1", "option2", True)
enum_type("option3")"#,
            &["Unknown enum element", "`option3`"],
        );
        assert::fails(
            r#"
enum_type = enum("option1", "option1")
enum_type("option3")"#,
            &["distinct", "option1"],
        );
        assert::pass(
            r#"
enum_type = enum("option1","option2")
def foo(x: enum_type.type) -> "enum_type":
    return x
foo(enum_type("option1"))"#,
        );
        assert::pass(
            r#"
v = [enum("option1","option2")]
v_0 = v[0]
def foo(x: v_0.type) -> "enum":
    return x
foo(v[0]("option1"))"#,
        );
        assert::pass(
            r#"
enum_type = enum("option1","option2")
assert_eq(enum_type.values(), ["option1","option2"])
assert_eq([enum_type[i].value for i in range(len(enum_type))], ["option1","option2"])
assert_eq(enum_type("option2").index, 1)
assert_eq([x.value for x in enum_type], ["option1","option2"])"#,
        );
        assert::pass(
            r#"
enum_type = enum("option1","option2")
x = enum_type("option1")
assert_eq(str(enum_type), "enum(\"option1\", \"option2\")")
assert_eq(str(x), "\"option1\"")
"#,
        );
        assert::pass(
            r#"
enum_type = enum("option1","option2")
repr(enum_type) # Check it is finite
"#,
        );
    }

    #[test]
    fn test_enum_equality() {
        assert::pass(
            r#"
enum_type = enum("option1", "option2", True)
assert_eq(enum_type("option1"), enum_type("option1"))
assert_eq(enum_type(True), enum_type(True))
assert_ne(enum_type("option1"), enum_type(True))
"#,
        );

        let mut a = Assert::new();
        a.module(
            "m",
            r#"
enum_type = enum("option1", "option2", True)
enum_val = enum_type("option1")
"#,
        );
        a.pass(
            r#"
load('m', 'enum_type', 'enum_val')
assert_eq(enum_val, enum_type("option1"))
assert_ne(enum_val, enum_type(True))
"#,
        );

        a = Assert::new();
        a.module(
            "m1",
            r#"
rt = enum(1)
"#,
        );
        a.module(
            "m2",
            r#"
rt = enum(1, 2)
"#,
        );
        a.pass(
            r#"
load('m1', r1='rt')
load('m2', r2='rt')
rt = enum(1)
diff = enum(1)
assert_eq(r1(1), rt(1))
assert_ne(rt(1), r2(1))
assert_ne(rt(1), diff(1))
"#,
        );
    }
}
