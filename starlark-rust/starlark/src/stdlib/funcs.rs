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

//! A module with the standard function and constants that are by default in all
//! dialect of Starlark

use std::char;
use std::cmp::Ordering;
use std::num::NonZeroI32;

use starlark_derive::starlark_module;

use crate as starlark;
use crate::collections::SmallMap;
use crate::environment::GlobalsBuilder;
use crate::eval::Arguments;
use crate::eval::Evaluator;
use crate::values::bool::BOOL_TYPE;
use crate::values::dict::Dict;
use crate::values::dict::DictRef;
use crate::values::float::StarlarkFloat;
use crate::values::int::INT_TYPE;
use crate::values::list::AllocList;
use crate::values::list::ListRef;
use crate::values::none::NoneType;
use crate::values::range::Range;
use crate::values::string::STRING_TYPE;
use crate::values::tuple::AllocTuple;
use crate::values::tuple::TupleRef;
use crate::values::types::int_or_big::StarlarkInt;
use crate::values::types::int_or_big::StarlarkIntRef;
use crate::values::types::tuple::value::Tuple;
use crate::values::AllocValue;
use crate::values::FrozenStringValue;
use crate::values::Heap;
use crate::values::StringValue;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueError;
use crate::values::ValueLike;

fn unpack_pair<'v>(pair: Value<'v>, heap: &'v Heap) -> anyhow::Result<(Value<'v>, Value<'v>)> {
    let mut it = pair.iterate(heap)?;
    if let Some(first) = it.next() {
        if let Some(second) = it.next() {
            if it.next().is_none() {
                return Ok((first, second));
            }
        }
    }
    Err(anyhow::anyhow!(
        "Found a non-pair element in the positional argument of dict(): {}",
        pair.to_repr(),
    ))
}

fn min_max_iter<'v>(
    mut it: impl Iterator<Item = Value<'v>>,
    key: Option<Value<'v>>,
    eval: &mut Evaluator<'v, '_>,
    // Select min on true, max on false.
    min: bool,
) -> anyhow::Result<Value<'v>> {
    let mut max = match it.next() {
        Some(x) => x,
        None => {
            return Err(anyhow::anyhow!(
                "Argument is an empty iterable, max() expect a non empty iterable"
            ));
        }
    };
    let update_max_ordering = if min {
        Ordering::Greater
    } else {
        Ordering::Less
    };
    match key {
        None => {
            for i in it {
                if max.compare(i)? == update_max_ordering {
                    max = i;
                }
            }
        }
        Some(key) => {
            let mut cached = key.invoke_pos(&[max], eval)?;
            for i in it {
                let keyi = key.invoke_pos(&[i], eval)?;
                if cached.compare(keyi)? == update_max_ordering {
                    max = i;
                    cached = keyi;
                }
            }
        }
    };
    Ok(max)
}

/// Common implementation of `min` and `max`.
fn min_max<'v>(
    mut args: Vec<Value<'v>>,
    key: Option<Value<'v>>,
    eval: &mut Evaluator<'v, '_>,
    // Select min on true, max on false.
    min: bool,
) -> anyhow::Result<Value<'v>> {
    if args.len() == 1 {
        let it = args.swap_remove(0).iterate(eval.heap())?;
        min_max_iter(it, key, eval, min)
    } else {
        min_max_iter(args.into_iter(), key, eval, min)
    }
}

#[starlark_module]
pub(crate) fn global_functions(builder: &mut GlobalsBuilder) {
    /// The `None` value, used to represent nothing.
    /// Implicitly returned from functions that don't have an explicit return.
    const None: NoneType = NoneType;

    /// A boolean representing true.
    const True: bool = true;

    /// A boolean representing false.
    const False: bool = false;

    /// fail: fail the execution
    ///
    /// ```
    /// # starlark::assert::fail(r#"
    /// fail("this is an error")  # fail: this is an error
    /// # "#, "this is an error");
    /// # starlark::assert::fail(r#"
    /// fail("oops", 1, False)  # fail: oops 1 False
    /// # "#, "oops 1 False");
    /// ```
    #[starlark(return_type = "\"never\"")]
    fn fail(#[starlark(args)] args: Vec<Value>) -> anyhow::Result<NoneType> {
        let mut s = String::new();
        for x in args {
            s.push(' ');
            match x.unpack_str() {
                Some(x) => s.push_str(x),
                None => x.collect_repr(&mut s),
            }
        }
        Err(anyhow::anyhow!("fail:{}", s))
    }

    /// [any](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#any
    /// ): returns true if any value in the iterable object have a truth value
    /// of true.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// any([0, True]) == True
    /// any([0, 1]) == True
    /// any([0, 1, True]) == True
    /// any([0, 0]) == False
    /// any([0, False]) == False
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn any<'v>(
        #[starlark(require = pos, type = "iter(\"\")")] x: Value<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<bool> {
        for i in x.iterate(heap)? {
            if i.to_bool() {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// [all](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#all
    /// ): returns true if all values in the iterable object have a truth value
    /// of true.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// all([1, True]) == True
    /// all([1, 1]) == True
    /// all([0, 1, True]) == False
    /// all([True, 1, True]) == True
    /// all([0, 0]) == False
    /// all([0, False]) == False
    /// all([True, 0]) == False
    /// all([1, False]) == False
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn all<'v>(
        #[starlark(require = pos, type = "iter(\"\")")] x: Value<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<bool> {
        for i in x.iterate(heap)? {
            if !i.to_bool() {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// [bool](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#bool
    /// ): returns the truth value of any starlark value.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// bool() == False
    /// bool([]) == False
    /// bool([1]) == True
    /// bool(True) == True
    /// bool(False) == False
    /// bool(None) == False
    /// bool(bool) == True
    /// bool(1) == True
    /// bool(0) == False
    /// bool({}) == False
    /// bool({1:2}) == True
    /// bool(()) == False
    /// bool((1,)) == True
    /// bool("") == False
    /// bool("1") == True
    /// # "#);
    /// ```
    #[starlark(dot_type = BOOL_TYPE, speculative_exec_safe)]
    fn bool(#[starlark(require = pos)] x: Option<Value>) -> anyhow::Result<bool> {
        match x {
            None => Ok(false),
            Some(x) => Ok(x.to_bool()),
        }
    }

    /// [chr](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#bool
    /// ): returns a string encoding a codepoint.
    ///
    /// `chr(i)` returns a returns a string that encodes the single Unicode code
    /// point whose value is specified by the integer `i`. `chr` fails
    /// unless `0 ≤ i ≤ 0x10FFFF`.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// chr(65) == 'A'
    /// chr(1049) == 'Й'
    /// chr(0x1F63F) == '😿'
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn chr(#[starlark(require = pos)] i: i32) -> anyhow::Result<char> {
        let cp = u32::try_from(i)
            .map_err(|_| anyhow::anyhow!("chr() parameter value negative integer {i}"))?;
        match char::from_u32(cp) {
            Some(x) => Ok(x),
            None => Err(anyhow::anyhow!(
                "chr() parameter value is 0x{:x} which is not a valid UTF-8 codepoint",
                cp
            )),
        }
    }

    /// [dict](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#dict
    /// ): creates a dictionary.
    ///
    /// `dict` creates a dictionary. It accepts up to one positional argument,
    /// which is interpreted as an iterable of two-element sequences
    /// (pairs), each specifying a key/value pair in the
    /// resulting dictionary.
    ///
    /// `dict` also accepts any number of keyword arguments, each of which
    /// specifies a key/value pair in the resulting dictionary; each keyword
    /// is treated as a string.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// dict() == {}
    /// dict(**{'a': 1}) == {'a': 1}
    /// dict({'a': 1}) == {'a': 1}
    /// dict([(1, 2), (3, 4)]) == {1: 2, 3: 4}
    /// dict([(1, 2), ['a', 'b']]) == {1: 2, 'a': 'b'}
    /// dict(one=1, two=2) == {'one': 1, 'two': 2}
    /// dict([(1, 2)], x=3) == {1: 2, 'x': 3}
    /// dict([('x', 2)], x=3) == {'x': 3}
    /// # "#);
    /// # starlark::assert::is_true(r#"
    /// x = {'a': 1}
    /// y = dict([('x', 2)], **x)
    /// x == {'a': 1} and y == {'x': 2, 'a': 1}
    /// # "#);
    /// ```
    #[starlark(dot_type = Dict::TYPE, speculative_exec_safe)]
    fn dict<'v>(args: &Arguments<'v, '_>, heap: &'v Heap) -> anyhow::Result<Dict<'v>> {
        // Dict is super hot, and has a slightly odd signature, so we can do a bunch of special cases on it.
        // In particular, we don't generate the kwargs if there are no positional arguments.
        // Therefore we make it take the raw Arguments.
        // It might have one positional argument, which could be a dict or an array of pairs.
        // It might have named/kwargs arguments, which we copy over (afterwards).

        let pos = args.optional1(heap)?;
        let kwargs = args.names()?;

        match pos {
            None => Ok(kwargs),
            Some(pos) => {
                let mut result = match DictRef::from_value(pos) {
                    Some(pos) => {
                        let mut result = pos.clone();
                        result.reserve(kwargs.len());
                        result
                    }
                    None => {
                        let it = pos.iterate(heap)?;
                        let mut result = SmallMap::with_capacity(it.size_hint().0 + kwargs.len());
                        for el in it {
                            let (k, v) = unpack_pair(el, heap)?;
                            let k = k.get_hashed()?;
                            result.insert_hashed(k, v);
                        }
                        Dict::new(result)
                    }
                };
                for (k, v) in kwargs.iter_hashed() {
                    result.insert_hashed(k, v);
                }
                Ok(result)
            }
        }
    }

    /// [dir](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#dir
    /// ): list attributes of a value.
    ///
    /// `dir(x)` returns a list of the names of the attributes (fields and
    /// methods) of its operand. The attributes of a value `x` are the names
    /// `f` such that `x.f` is a valid expression.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "capitalize" in dir("abc")
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn dir(#[starlark(require = pos)] x: Value) -> anyhow::Result<Vec<String>> {
        Ok(x.dir_attr())
    }

    /// [enumerate](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#enumerate
    /// ): return a list of (index, element) from an iterable.
    ///
    /// `enumerate(x)` returns a list of `(index, value)` pairs, each containing
    /// successive values of the iterable sequence and the index of the
    /// value within the sequence.
    ///
    /// The optional second parameter, `start`, specifies an integer value to
    /// add to each index.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// enumerate(["zero", "one", "two"]) == [(0, "zero"), (1, "one"), (2, "two")]
    /// enumerate(["one", "two"], 1) == [(1, "one"), (2, "two")]
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn enumerate<'v>(
        #[starlark(require = pos, type = "iter(\"\")")] it: Value<'v>,
        #[starlark(default = 0)] start: i32,
        heap: &'v Heap,
    ) -> anyhow::Result<impl AllocValue<'v>> {
        let v = it
            .iterate(heap)?
            .enumerate()
            .map(move |(k, v)| (k as i32 + start, v));
        Ok(AllocList(v))
    }

    /// [float](
    /// https://github.com/google/skylark/blob/a5f7082aabed29c0e429c722292c66ec8ecf9591/doc/spec.md#float
    /// ): interprets its argument as a floating-point number.
    ///
    /// If x is a `float`, the result is x.
    /// if x is an `int`, the result is the nearest floating point value to x.
    /// If x is a string, the string is interpreted as a floating-point literal.
    /// With no arguments, `float()` returns `0.0`.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// float() == 0.0
    /// float(1) == 1.0
    /// float('1') == 1.0
    /// float('1.0') == 1.0
    /// float('.25') == 0.25
    /// float('1e2') == 100.0
    /// float(False) == 0.0
    /// float(True) == 1.0
    /// # "#);
    /// # starlark::assert::fail(r#"
    /// float("hello")   # error: not a valid number
    /// # "#, "not a valid number");
    /// # starlark::assert::fail(r#"
    /// float([])   # error: argument must be a string, a number, or a boolean
    /// # "#, "argument must be a string, a number, or a boolean");
    /// ```
    #[starlark(dot_type = StarlarkFloat::TYPE, speculative_exec_safe)]
    fn float(
        #[starlark(require = pos, type = "[str.type, int.type, float.type, bool.type]")] a: Option<
            Value,
        >,
    ) -> anyhow::Result<f64> {
        if a.is_none() {
            return Ok(0.0);
        }
        let a = a.unwrap();
        if let Some(f) = a.unpack_num().map(|n| n.as_float()) {
            Ok(f)
        } else if let Some(s) = a.unpack_str() {
            match s.parse::<f64>() {
                Ok(f) => {
                    if f.is_infinite() && !s.to_lowercase().contains("inf") {
                        // if a resulting float is infinite but the parsed string is not explicitly infinity then we should fail with an error
                        Err(anyhow::anyhow!(
                            "float() floating-point number too large: {}",
                            s
                        ))
                    } else {
                        Ok(f)
                    }
                }
                Err(x) => Err(anyhow::anyhow!(
                    "{} is not a valid number: {}",
                    a.to_repr(),
                    x
                )),
            }
        } else if let Some(b) = a.unpack_bool() {
            Ok(if b { 1.0 } else { 0.0 })
        } else {
            Err(anyhow::anyhow!(
                "float() argument must be a string, a number, or a boolean, not `{}`",
                a.get_type()
            ))
        }
    }

    /// [getattr](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#getattr
    /// ): returns the value of an attribute
    ///
    /// `getattr(x, name)` returns the value of the attribute (field or method)
    /// of x named `name`. It is a dynamic error if x has no such attribute.
    ///
    /// `getattr(x, "f")` is equivalent to `x.f`.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// getattr("banana", "split")("a") == ["b", "n", "n", ""] # equivalent to "banana".split("a")
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn getattr<'v>(
        #[starlark(require = pos)] a: Value<'v>,
        #[starlark(require = pos)] attr: &str,
        #[starlark(require = pos)] default: Option<Value<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        // TODO(nga): this doesn't cache string hash, so it is suboptimal.
        match a.get_attr(attr, heap)? {
            Some(v) => Ok(v),
            None => match default {
                Some(x) => Ok(x),
                None => ValueError::unsupported_owned(a.get_type(), &format!(".{}", attr), None),
            },
        }
    }

    /// [hasattr](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#hasattr
    /// ): test if an object has an attribute
    ///
    /// `hasattr(x, name)` reports whether x has an attribute (field or method)
    /// named `name`.
    #[starlark(speculative_exec_safe)]
    fn hasattr<'v>(
        #[starlark(require = pos)] a: Value<'v>,
        #[starlark(require = pos)] attr: &str,
        heap: &'v Heap,
    ) -> anyhow::Result<bool> {
        Ok(a.has_attr(attr, heap))
    }

    /// [hash](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#hash
    /// ): returns the hash number of a value.
    ///
    /// `hash(x)`` returns an integer hash value for x such that `x == y`
    /// implies `hash(x) == hash(y)``.
    ///
    /// `hash` fails if x, or any value upon which its hash depends, is
    /// unhashable.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// hash("hello") != hash("world")
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn hash(#[starlark(require = pos)] a: &str) -> anyhow::Result<i32> {
        // From the starlark spec:
        // > the hash function for strings is the same as that implemented by java.lang.String.hashCode,
        // > a simple polynomial accumulator over the UTF-16 transcoding of the string:
        // > `s[0]*31^(n-1) + s[1]*31^(n-2) + ... + s[n-1]`
        // As per spec the function should only support string and bytes types.
        // We don't have support for bytes, so parameter is forced to be a string.

        // Most strings are ASCII strings, try them first.
        #[allow(clippy::never_loop)]
        'ascii: loop {
            let mut hash = 0i32;
            for &b in a.as_bytes() {
                if b > 0x7f {
                    break 'ascii;
                }
                hash = hash.wrapping_mul(31i32).wrapping_add(b as i32);
            }
            return Ok(hash);
        }

        Ok(a.encode_utf16().fold(0i32, |hash: i32, c: u16| {
            31i32.wrapping_mul(hash).wrapping_add(c as i32)
        }))
    }

    /// [int](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#int
    /// ): convert a value to integer.
    ///
    /// `int(x[, base])` interprets its argument as an integer.
    ///
    /// If x is an `int`, the result is x.
    /// If x is a `float`, the result is the integer value nearest to x,
    /// truncating towards zero; it is an error if x is not finite (`NaN`,
    /// `+Inf`, `-Inf`).
    /// If x is a `bool`, the result is 0 for `False` or 1 for `True`.
    ///
    /// If x is a string, it is interpreted like a string literal;
    /// an optional base prefix (`0`, `0b`, `0B`, `0x`, `0X`) determines which
    /// base to use. The string may specify an arbitrarily large integer,
    /// whereas true integer literals are restricted to 64 bits.
    /// If a non-zero `base` argument is provided, the string is interpreted
    /// in that base and no base prefix is permitted; the base argument may
    /// specified by name.
    ///
    /// `int()` with no arguments returns 0.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// int() == 0
    /// int(1) == 1
    /// int(False) == 0
    /// int(True) == 1
    /// int('1') == 1
    /// int('16') == 16
    /// int('16', 10) == 16
    /// int('16', 8) == 14
    /// int('16', 16) == 22
    /// int(0.0) == 0
    /// int(3.14) == 3
    /// int(-12345.6789) == -12345
    /// int(2e9) == 2000000000
    /// # "#);
    /// # starlark::assert::fail(r#"
    /// int("hello")   # error: Cannot parse
    /// # "#, "Cannot parse");
    /// # starlark::assert::fail(r#"
    /// int(float("nan"))   # error: cannot be represented as exact integer
    /// # "#, "cannot be represented as exact integer");
    /// # starlark::assert::fail(r#"
    /// int(float("inf"))   # error: cannot be represented as exact integer
    /// # "#, "cannot be represented as exact integer");
    /// ```
    #[starlark(dot_type = INT_TYPE, speculative_exec_safe, return_type = "int.type")]
    fn int<'v>(
        #[starlark(require = pos, type = "[int.type, str.type, float.type, bool.type]")] a: Option<
            Value<'v>,
        >,
        base: Option<i32>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let Some(a) = a else {
            return Ok(heap.alloc(0));
        };
        if let Some(s) = a.unpack_str() {
            let base = base.unwrap_or(0);
            if base == 1 || base < 0 || base > 36 {
                return Err(anyhow::anyhow!(
                    "{} is not a valid base, int() base must be >= 2 and <= 36",
                    base
                ));
            }
            let (negate, s) = {
                match s.chars().next() {
                    Some('+') => (false, s.get(1..).unwrap()),
                    Some('-') => (true, s.get(1..).unwrap()),
                    _ => (false, s),
                }
            };
            let base = if base == 0 {
                match s.get(0..2) {
                    Some("0b") | Some("0B") => 2,
                    Some("0o") | Some("0O") => 8,
                    Some("0x") | Some("0X") => 16,
                    _ => 10,
                }
            } else {
                base as u32
            };
            let s = match base {
                16 => {
                    if s.starts_with("0x") || s.starts_with("0X") {
                        s.get(2..).unwrap()
                    } else {
                        s
                    }
                }
                8 => {
                    if s.starts_with("0o") || s.starts_with("0O") {
                        s.get(2..).unwrap()
                    } else {
                        s
                    }
                }
                2 => {
                    if s.starts_with("0b") || s.starts_with("0B") {
                        s.get(2..).unwrap()
                    } else {
                        s
                    }
                }
                _ => s,
            };
            // We already handled the sign above, so we are not trying to parse another sign.
            if s.starts_with('-') || s.starts_with('+') {
                return Err(anyhow::anyhow!("Cannot parse `{}` as an integer", s,));
            }

            let x = StarlarkInt::from_str_radix(s, base)?;
            let x = if negate { -x } else { x };
            Ok(heap.alloc(x))
        } else if let Some(base) = base {
            Err(anyhow::anyhow!(
                "int() cannot convert non-string with explicit base '{}'",
                base
            ))
        } else if StarlarkIntRef::unpack_value(a).is_some() {
            Ok(a)
        } else if let Some(f) = StarlarkFloat::unpack_value(a) {
            Ok(heap.alloc(StarlarkInt::from_f64_exact(f.0.trunc())?))
        } else if let Some(b) = a.unpack_bool() {
            Ok(heap.alloc(b as i32))
        } else {
            Err(ValueError::IncorrectParameterTypeWithExpected(
                a.get_type().to_owned(),
                "number".to_owned(),
            )
            .into())
        }
    }

    /// [len](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#len
    /// ): get the length of a sequence
    ///
    /// `len(x)` returns the number of elements in its argument.
    ///
    /// It is a dynamic error if its argument is not a sequence.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// len(()) == 0
    /// len({}) == 0
    /// len([]) == 0
    /// len([1]) == 1
    /// len([1,2]) == 2
    /// len({'16': 10}) == 1
    /// # "#);
    /// # starlark::assert::fail(r#"
    /// len(True)    # error: not supported
    /// # "#, "not supported");
    /// ```
    #[starlark(speculative_exec_safe)]
    fn len(#[starlark(require = pos)] a: Value) -> anyhow::Result<i32> {
        a.length()
    }

    /// [list](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#list
    /// ): construct a list.
    ///
    /// `list(x)` returns a new list containing the elements of the
    /// iterable sequence x.
    ///
    /// With no argument, `list()` returns a new empty list.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// list()        == []
    /// list((1,2,3)) == [1, 2, 3]
    /// # "#);
    /// # starlark::assert::fail(r#"
    /// list("strings are not iterable") # error: not supported
    /// # "#, "not supported");
    /// ```
    #[starlark(dot_type = ListRef::TYPE, speculative_exec_safe, return_type = "[\"\"]")]
    fn list<'v>(
        #[starlark(require = pos, type = "iter(\"\")")] a: Option<Value<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        Ok(if let Some(a) = a {
            if let Some(xs) = ListRef::from_value(a) {
                heap.alloc_list(xs.content())
            } else {
                let it = a.iterate(heap)?;
                heap.alloc(AllocList(it))
            }
        } else {
            heap.alloc(AllocList::EMPTY)
        })
    }

    /// [max](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#max
    /// ): returns the maximum of a sequence.
    ///
    /// `max(x)` returns the greatest element in the iterable sequence x.
    ///
    /// It is an error if any element does not support ordered comparison,
    /// or if the sequence is empty.
    ///
    /// The optional named parameter `key` specifies a function to be applied
    /// to each element prior to comparison.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// max([3, 1, 4, 1, 5, 9])               == 9
    /// max("two", "three", "four")           == "two"    # the lexicographically greatest
    /// max("two", "three", "four", key=len)  == "three"  # the longest
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn max<'v>(
        #[starlark(args)] mut args: Vec<Value<'v>>,
        key: Option<Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        min_max(args, key, eval, false)
    }

    /// [min](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#min
    /// ): returns the minimum of a sequence.
    ///
    /// `min(x)` returns the least element in the iterable sequence x.
    ///
    /// It is an error if any element does not support ordered comparison,
    /// or if the sequence is empty.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// min([3, 1, 4, 1, 5, 9])                 == 1
    /// min("two", "three", "four")             == "four"  # the lexicographically least
    /// min("two", "three", "four", key=len)    == "two"   # the shortest
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn min<'v>(
        #[starlark(args)] mut args: Vec<Value<'v>>,
        key: Option<Value<'v>>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        min_max(args, key, eval, true)
    }

    /// [ord](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.mdord
    /// ): returns the codepoint of a character
    ///
    /// `ord(s)` returns the integer value of the sole Unicode code point
    /// encoded by the string `s`.
    ///
    /// If `s` does not encode exactly one Unicode code point, `ord` fails.
    /// Each invalid code within the string is treated as if it encodes the
    /// Unicode replacement character, U+FFFD.
    ///
    /// Example:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// ord("A")                                == 65
    /// ord("Й")                                == 1049
    /// ord("😿")                               == 0x1F63F
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn ord<'v>(#[starlark(require = pos)] a: StringValue<'v>) -> anyhow::Result<i32> {
        let mut chars = a.as_str().chars();
        if let Some(c) = chars.next() {
            if chars.next().is_none() {
                return Ok(u32::from(c) as i32);
            }
        }
        Err(anyhow::anyhow!(
            "ord(): {} is not a single character string",
            a.to_value().to_repr()
        ))
    }

    /// [range](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#range
    /// ): return a range of integers
    ///
    /// `range` returns a tuple of integers defined by the specified interval
    /// and stride.
    ///
    /// ```python
    /// range(stop)                             # equivalent to range(0, stop)
    /// range(start, stop)                      # equivalent to range(start, stop, 1)
    /// range(start, stop, step)
    /// ```
    ///
    /// `range` requires between one and three integer arguments.
    /// With one argument, `range(stop)` returns the ascending sequence of
    /// non-negative integers less than `stop`.
    /// With two arguments, `range(start, stop)` returns only integers not less
    /// than `start`.
    ///
    /// With three arguments, `range(start, stop, step)` returns integers
    /// formed by successively adding `step` to `start` until the value meets or
    /// passes `stop`. A call to `range` fails if the value of `step` is
    /// zero.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// list(range(10))                         == [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
    /// list(range(3, 10))                      == [3, 4, 5, 6, 7, 8, 9]
    /// list(range(3, 10, 2))                   == [3, 5, 7, 9]
    /// list(range(10, 3, -2))                  == [10, 8, 6, 4]
    /// # "#);
    /// ```
    #[starlark(dot_type = Range::TYPE, speculative_exec_safe)]
    fn range(
        #[starlark(require = pos)] a1: i32,
        #[starlark(require = pos)] a2: Option<i32>,
        #[starlark(require = pos, default = 1)] step: i32,
    ) -> anyhow::Result<Range> {
        let start = match a2 {
            None => 0,
            Some(_) => a1,
        };
        let stop = a2.unwrap_or(a1);
        let step = match NonZeroI32::new(step) {
            Some(step) => step,
            None => {
                return Err(anyhow::anyhow!(
                    "Third argument of range (step) cannot be zero"
                ));
            }
        };
        Ok(Range::new(start, stop, step))
    }

    /// [repr](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#repr
    /// ): formats its argument as a string.
    ///
    /// All strings in the result are double-quoted.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// repr(1)                 == '1'
    /// repr("x")               == "\"x\""
    /// repr([1, "x"])          == "[1, \"x\"]"
    /// repr("test \"'")        == "\"test \\\"'\""
    /// repr("x\"y😿 \\'")      == "\"x\\\"y\\U0001f63f \\\\'\""
    /// "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn repr<'v>(
        #[starlark(require = pos)] a: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StringValue<'v>> {
        let mut s = eval.string_pool.alloc();
        a.collect_repr(&mut s);
        let r = eval.heap().alloc_str(&s);
        eval.string_pool.release(s);
        Ok(r)
    }

    /// [reversed](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#reversed
    /// ): reverse a sequence
    ///
    /// `reversed(x)` returns a new list containing the elements of the iterable
    /// sequence x in reverse order.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// reversed(['a', 'b', 'c'])              == ['c', 'b', 'a']
    /// reversed(range(5))                     == [4, 3, 2, 1, 0]
    /// reversed("stressed".elems())           == ["d", "e", "s", "s", "e", "r", "t", "s"]
    /// reversed({"one": 1, "two": 2}.keys())  == ["two", "one"]
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe, return_type = "[\"\"]")]
    fn reversed<'v>(
        #[starlark(require = pos, type = "iter(\"\")")] a: Value<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<Vec<Value<'v>>> {
        let mut v: Vec<Value> = a.iterate(heap)?.collect();
        v.reverse();
        Ok(v)
    }

    /// [sorted](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#sorted
    /// ): sort a sequence
    ///
    /// `sorted(x)` returns a new list containing the elements of the iterable
    /// sequence x, in sorted order.  The sort algorithm is stable.
    ///
    /// The optional named parameter `reverse`, if true, causes `sorted` to
    /// return results in reverse sorted order.
    ///
    /// The optional named parameter `key` specifies a function of one
    /// argument to apply to obtain the value's sort key.
    /// The default behavior is the identity function.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// sorted([3, 1, 4, 1, 5, 9])                               == [1, 1, 3, 4, 5, 9]
    /// sorted([3, 1, 4, 1, 5, 9], reverse=True)                 == [9, 5, 4, 3, 1, 1]
    /// sorted(["two", "three", "four"], key=len)                == ["two", "four", "three"] # shortest to longest
    /// sorted(["two", "three", "four"], key=len, reverse=True)  == ["three", "four", "two"] # longest to shortest
    /// # "#);
    /// ```
    // This function is not spec-safe, because it may call `key` function
    // which might be not spec-safe.
    #[starlark(return_type = "[\"\"]")]
    fn sorted<'v>(
        #[starlark(require = pos, type = "iter(\"\")")] x: Value<'v>,
        #[starlark(require = named)] key: Option<Value<'v>>,
        #[starlark(require = named, default = false)] reverse: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AllocList<impl IntoIterator<Item = Value<'v>>>> {
        let it = x.iterate(eval.heap())?;
        let mut it = match key {
            None => it.map(|x| (x, x)).collect(),
            Some(key) => {
                let mut v = Vec::new();
                for el in it {
                    v.push((el, key.invoke_pos(&[el], eval)?));
                }
                v
            }
        };

        let mut compare_ok = Ok(());

        it.sort_by(|x: &(Value, Value), y: &(Value, Value)| {
            let ord_or_err = if reverse {
                x.1.compare(y.1).map(Ordering::reverse)
            } else {
                x.1.compare(y.1)
            };
            match ord_or_err {
                Ok(r) => r,
                Err(e) => {
                    compare_ok = Err(e);
                    Ordering::Equal // does not matter
                }
            }
        });

        compare_ok?;

        Ok(AllocList(it.into_iter().map(|x| x.0)))
    }

    /// [str](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#str
    /// ): formats its argument as a string.
    ///
    /// If x is a string, the result is x (without quotation).
    /// All other strings, such as elements of a list of strings, are
    /// double-quoted.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// str(1)                          == '1'
    /// str("x")                        == 'x'
    /// str([1, "x"])                   == "[1, \"x\"]"
    /// # "#);
    /// ```
    #[starlark(dot_type = STRING_TYPE, speculative_exec_safe)]
    fn str<'v>(
        #[starlark(require = pos)] a: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StringValue<'v>> {
        if let Some(a) = StringValue::new(a) {
            // Special case that can avoid reallocating, but is equivalent.
            Ok(a)
        } else {
            let mut s = eval.string_pool.alloc();
            a.collect_repr(&mut s);
            let r = eval.heap().alloc_str(&s);
            eval.string_pool.release(s);
            Ok(r)
        }
    }

    /// [tuple](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#tuple
    /// ): returns a tuple containing the elements of the iterable x.
    ///
    /// With no arguments, `tuple()` returns the empty tuple.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// tuple() == ()
    /// tuple([1,2,3]) == (1, 2, 3)
    /// # "#);
    /// ```
    #[starlark(dot_type = Tuple::TYPE, speculative_exec_safe, return_type = "tuple.type")]
    fn tuple<'v>(
        #[starlark(require = pos, type = "iter(\"\")")] a: Option<Value<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        if let Some(a) = a {
            if TupleRef::from_value(a).is_some() {
                return Ok(a);
            }

            let it = a.iterate(heap)?;
            Ok(heap.alloc_tuple_iter(it))
        } else {
            Ok(heap.alloc(AllocTuple::EMPTY))
        }
    }

    /// [type](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#type
    /// ): returns a string describing the type of its operand.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// type(None)              == "NoneType"
    /// type(0)                 == "int"
    /// type(1)                 == "int"
    /// type(())                == "tuple"
    /// type("hello")           == "string"
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn r#type<'v>(#[starlark(require = pos)] a: Value) -> anyhow::Result<FrozenStringValue> {
        Ok(a.get_type_value())
    }

    /// [zip](
    /// https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#zip
    /// ): zip several iterables together
    ///
    /// `zip()` returns a new list of n-tuples formed from corresponding
    /// elements of each of the n iterable sequences provided as arguments to
    /// `zip`.  That is, the first tuple contains the first element of each of
    /// the sequences, the second element contains the second element of each
    /// of the sequences, and so on.  The result list is only as long as the
    /// shortest of the input sequences.
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// zip()                           == []
    /// zip(range(5))                   == [(0,), (1,), (2,), (3,), (4,)]
    /// zip(range(5), "abc".elems())    == [(0, "a"), (1, "b"), (2, "c")]
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe, return_type = "[\"\"]")]
    fn zip<'v>(
        #[starlark(args)] args: Vec<Value<'v>>,
        heap: &'v Heap,
    ) -> anyhow::Result<Vec<Value<'v>>> {
        let mut v = Vec::new();
        let mut first = true;
        for arg in args {
            let mut idx = 0;
            for e in arg.iterate(heap)? {
                if first {
                    v.push(heap.alloc((e,)));
                    idx += 1;
                } else if idx < v.len() {
                    v[idx] = v[idx].add(heap.alloc((e,)), heap)?;
                    idx += 1;
                }
            }
            v.truncate(idx);
            first = false;
        }
        Ok(v)
    }
}

#[cfg(test)]
mod tests {
    use crate::assert;

    #[test]
    fn test_constants() {
        assert::is_true("not None");
        assert::is_true("not False");
        assert::is_true("True");
    }

    #[test]
    fn test_chr() {
        assert::fail("chr(0x110000)", "not a valid UTF-8");
        assert::fail("chr(-1)", "negative");
    }

    #[test]
    fn test_hash() {
        assert::eq("0", "hash('')");
        assert::eq("97", "hash('a')");
        assert::eq("3105", "hash('ab')");
        assert::eq("96354", "hash('abc')");
        assert::eq("2987074", "hash('abcd')");
        assert::eq("92599395", "hash('abcde')");
        assert::eq("-1424385949", "hash('abcdef')");
        assert::all_true(
            r#"
hash("te") == hash("te")
hash("te") != hash("st")
x = "test"; y = "te" + "st"; hash(y) == hash(y)
"#,
        );
        assert::fail("hash(None)", "doesn't match");
        assert::fail("hash(True)", "doesn't match");
        assert::fail("hash(1)", "doesn't match");
        assert::fail("hash([])", "doesn't match");
        assert::fail("hash({})", "doesn't match");
        assert::fail("hash(range(1))", "doesn't match");
        assert::fail("hash((1, 2))", "doesn't match");
        assert::fail(
            r#"
def foo():
    pass
hash(foo)
"#,
            "doesn't match",
        );
    }

    #[test]
    fn test_int() {
        assert::eq("2147483647", "int('2147483647')");
        assert::eq("-2147483647 - 1", "int('-2147483648')");
        assert::eq("0", "int('0')");
        assert::eq("0", "int('-0')");
        assert::eq(
            "999999999999999945322333868247445125709646570021247924665841614848",
            "int(1e66)",
        );
        assert::eq("2147483648", "int('2147483648')");
        assert::eq("-2147483649", "int('-2147483649')");
    }

    #[test]
    fn test_tuple() {
        assert::eq("(1, 2)", "tuple((1, 2))");
        assert::eq("(1, 2)", "tuple([1, 2])");
    }
}
