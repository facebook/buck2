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

use either::Either;
use starlark_derive::starlark_module;

use crate as starlark;
use crate::environment::GlobalsBuilder;
use crate::eval::Evaluator;
use crate::values::bool::StarlarkBool;
use crate::values::float::StarlarkFloat;
use crate::values::int::PointerI32;
use crate::values::list::AllocList;
use crate::values::none::NoneType;
use crate::values::num::value::NumRef;
use crate::values::range::Range;
use crate::values::string::repr::string_repr;
use crate::values::string::StarlarkStr;
use crate::values::tuple::value::FrozenTuple;
use crate::values::tuple::AllocTuple;
use crate::values::tuple::TupleRef;
use crate::values::types::int_or_big::StarlarkInt;
use crate::values::typing::never::StarlarkNever;
use crate::values::typing::StarlarkIter;
use crate::values::value_of_unchecked::ValueOfUnchecked;
use crate::values::AllocValue;
use crate::values::FrozenStringValue;
use crate::values::Heap;
use crate::values::StringValue;
use crate::values::Value;
use crate::values::ValueError;
use crate::values::ValueLike;
use crate::values::ValueOf;

#[starlark_module]
pub(crate) fn register_other(builder: &mut GlobalsBuilder) {
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
    fn fail(#[starlark(args)] args: Vec<Value>) -> anyhow::Result<StarlarkNever> {
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
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#any
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
        #[starlark(require = pos)] x: ValueOfUnchecked<'v, StarlarkIter<Value<'v>>>,
        heap: &'v Heap,
    ) -> anyhow::Result<bool> {
        for i in x.get().iterate(heap)? {
            if i.to_bool() {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// [all](
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#all
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
        #[starlark(require = pos)] x: ValueOfUnchecked<'v, StarlarkIter<Value<'v>>>,
        heap: &'v Heap,
    ) -> anyhow::Result<bool> {
        for i in x.get().iterate(heap)? {
            if !i.to_bool() {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// [bool](
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#bool
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
    #[starlark(as_type = StarlarkBool, speculative_exec_safe)]
    fn bool(#[starlark(require = pos)] x: Option<Value>) -> anyhow::Result<bool> {
        match x {
            None => Ok(false),
            Some(x) => Ok(x.to_bool()),
        }
    }

    /// [chr](
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#bool
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

    /// [dir](
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#dir
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
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#enumerate
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
        #[starlark(require = pos)] it: ValueOfUnchecked<'v, StarlarkIter<Value<'v>>>,
        #[starlark(default = 0)] start: i32,
        heap: &'v Heap,
    ) -> anyhow::Result<impl AllocValue<'v>> {
        let v = it
            .get()
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
    /// float([])   # error
    /// # "#, "Expected type");
    /// ```
    #[starlark(as_type = StarlarkFloat, speculative_exec_safe)]
    fn float(
        #[starlark(require = pos)] a: Option<Either<Either<NumRef, bool>, &str>>,
    ) -> anyhow::Result<f64> {
        if a.is_none() {
            return Ok(0.0);
        }
        let a = a.unwrap();
        match a {
            Either::Left(Either::Left(f)) => Ok(f.as_float()),
            Either::Left(Either::Right(b)) => Ok(if b { 1.0 } else { 0.0 }),
            Either::Right(s) => {
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
                    Err(x) => {
                        let mut repr = String::new();
                        string_repr(s, &mut repr);
                        Err(anyhow::anyhow!("{} is not a valid number: {}", repr, x,))
                    }
                }
            }
        }
    }

    /// [getattr](
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#getattr
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
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#hasattr
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
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#hash
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
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#int
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
    #[starlark(as_type = PointerI32, speculative_exec_safe)]
    fn int<'v>(
        #[starlark(require = pos)] a: Option<
            ValueOf<'v, Either<Either<NumRef<'v>, bool>, &'v str>>,
        >,
        base: Option<i32>,
        heap: &'v Heap,
    ) -> anyhow::Result<ValueOfUnchecked<'v, StarlarkInt>> {
        let Some(a) = a else {
            return Ok(ValueOfUnchecked::new(heap.alloc(0)));
        };
        let num_or_bool = match a.typed {
            Either::Left(num_or_bool) => num_or_bool,
            Either::Right(s) => {
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
                return Ok(ValueOfUnchecked::new(heap.alloc(x)));
            }
        };

        if let Some(base) = base {
            return Err(anyhow::anyhow!(
                "int() cannot convert non-string with explicit base '{}'",
                base
            ));
        }

        match num_or_bool {
            Either::Left(NumRef::Int(_)) => Ok(ValueOfUnchecked::new(a.value)),
            Either::Left(NumRef::Float(f)) => Ok(ValueOfUnchecked::new(
                heap.alloc(StarlarkInt::from_f64_exact(f.trunc())?),
            )),
            Either::Right(b) => Ok(ValueOfUnchecked::new(heap.alloc(b as i32))),
        }
    }

    /// [len](
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#len
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
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#range
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
    #[starlark(as_type = Range, speculative_exec_safe)]
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
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#repr
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
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#reversed
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
    #[starlark(speculative_exec_safe)]
    fn reversed<'v>(
        #[starlark(require = pos)] a: ValueOfUnchecked<'v, StarlarkIter<Value<'v>>>,
        heap: &'v Heap,
    ) -> anyhow::Result<Vec<Value<'v>>> {
        let mut v: Vec<Value> = a.get().iterate(heap)?.collect();
        v.reverse();
        Ok(v)
    }

    /// [sorted](
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#sorted
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
    fn sorted<'v>(
        #[starlark(require = pos)] x: ValueOfUnchecked<'v, ValueOfUnchecked<Value<'v>>>,
        #[starlark(require = named)] key: Option<Value<'v>>,
        #[starlark(require = named, default = false)] reverse: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<AllocList<impl IntoIterator<Item = Value<'v>>>> {
        let it = x.get().iterate(eval.heap())?;
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
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#str
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
    #[starlark(as_type = StarlarkStr, speculative_exec_safe)]
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
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#tuple
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
    #[starlark(as_type = FrozenTuple, speculative_exec_safe)]
    fn tuple<'v>(
        #[starlark(require = pos)] a: Option<ValueOfUnchecked<'v, StarlarkIter<Value<'v>>>>,
        heap: &'v Heap,
    ) -> anyhow::Result<ValueOfUnchecked<'v, &'v TupleRef<'v>>> {
        if let Some(a) = a {
            if TupleRef::from_value(a.get()).is_some() {
                return Ok(ValueOfUnchecked::new(a.get()));
            }

            let it = a.get().iterate(heap)?;
            Ok(ValueOfUnchecked::new(heap.alloc_tuple_iter(it)))
        } else {
            Ok(ValueOfUnchecked::new(heap.alloc(AllocTuple::EMPTY)))
        }
    }

    /// [type](
    /// https://github.com/bazelbuild/starlark/blob/master/spec.md#type
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
}

#[cfg(test)]
mod tests {
    use crate::assert;
    use crate::assert::Assert;

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
        assert::fail("noop(hash)(None)", "doesn't match");
        assert::fail("noop(hash)(True)", "doesn't match");
        assert::fail("noop(hash)(1)", "doesn't match");
        assert::fail("noop(hash)([])", "doesn't match");
        assert::fail("noop(hash)({})", "doesn't match");
        assert::fail("noop(hash)(range(1))", "doesn't match");
        assert::fail("noop(hash)((1, 2))", "doesn't match");
        assert::fail(
            r#"
def foo():
    pass
noop(hash)(foo)
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
        let mut a = Assert::new();
        // TODO(nga): fix and enable.
        a.disable_static_typechecking();
        a.eq("(1, 2)", "tuple((1, 2))");
        a.eq("(1, 2)", "tuple([1, 2])");
    }
}
