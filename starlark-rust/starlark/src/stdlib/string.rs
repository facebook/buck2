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

//! Methods for the `string` type.

use std::cmp;

use anyhow::anyhow;
use gazebo::prelude::*;

use crate as starlark;
use crate::environment::MethodsBuilder;
use crate::eval::Arguments;
use crate::eval::Evaluator;
use crate::stdlib::string::fast_string::convert_str_indices;
use crate::values::none::NoneOr;
use crate::values::string::fast_string;
use crate::values::string::interpolation;
use crate::values::tuple::Tuple;
use crate::values::types::string::fast_string::StrIndices;
use crate::values::types::string::iter::iterate_chars;
use crate::values::types::string::iter::iterate_codepoints;
use crate::values::Heap;
use crate::values::StringValue;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueOf;

// This does not exists in rust, split would cut the string incorrectly and
// split_whitespace cannot take a n parameter.
fn splitn_whitespace(s: &str, maxsplit: usize) -> Vec<String> {
    let mut v = Vec::new();
    let mut cur = String::new();
    let mut split = 1;
    let mut eat_ws = true;
    for c in s.chars() {
        if split >= maxsplit && !eat_ws {
            cur.push(c)
        } else if c.is_whitespace() {
            if !cur.is_empty() {
                v.push(cur);
                cur = String::new();
                split += 1;
                eat_ws = true;
            }
        } else {
            eat_ws = false;
            cur.push(c)
        }
    }
    if !cur.is_empty() {
        v.push(cur)
    }
    v
}

fn rsplitn_whitespace(s: &str, maxsplit: usize) -> Vec<String> {
    let mut v = Vec::new();
    let mut cur = String::new();
    let mut split = 1;
    let mut eat_ws = true;
    for c in s.chars().rev() {
        if split >= maxsplit && !eat_ws {
            cur.push(c)
        } else if c.is_whitespace() {
            if !cur.is_empty() {
                v.push(cur.chars().rev().collect());
                cur = String::new();
                split += 1;
                eat_ws = true;
            }
        } else {
            eat_ws = false;
            cur.push(c)
        }
    }
    if !cur.is_empty() {
        v.push(cur.chars().rev().collect());
    }
    v.reverse();
    v
}

enum StringOrTuple<'v> {
    String(&'v str),
    Tuple(Vec<&'v str>),
}

impl<'v> UnpackValue<'v> for StringOrTuple<'v> {
    fn expected() -> String {
        "str or tuple".to_owned()
    }

    fn unpack_value(value: Value<'v>) -> Option<Self> {
        if let Some(s) = value.unpack_str() {
            Some(Self::String(s))
        } else {
            Some(Self::Tuple(
                Tuple::from_value(value)?
                    .iter()
                    .map(|x| x.unpack_str())
                    .collect::<Option<_>>()?,
            ))
        }
    }
}

#[starlark_module]
pub(crate) fn string_methods(builder: &mut MethodsBuilder) {
    /// [string.elems](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·elems
    /// ): returns an iterable of the bytes values of a string.
    ///
    /// `S.elems()` returns an iterable value containing the
    /// sequence of numeric bytes values in the string S.
    ///
    /// To materialize the entire sequence of bytes, apply `list(...)` to the
    /// result.
    ///
    /// Example:
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::is_true(r#"
    /// list("Hello, 世界".elems()) == [
    ///     "H", "e", "l", "l", "o", ",", " ", "世", "界"]
    /// # "#);
    /// ```
    fn elems<'v>(this: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(iterate_chars(this, heap))
    }

    /// string.capitalize: returns a copy of string S, where the first character (if any) is converted to uppercase;
    /// all other characters are converted to lowercase.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "hello, world!".capitalize() == "Hello, world!"
    /// "Hello, World!".capitalize() == "Hello, world!"
    /// "".capitalize() == ""
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn capitalize(this: &str) -> anyhow::Result<String> {
        let mut result = String::with_capacity(this.len());
        for (i, c) in this.chars().enumerate() {
            if i == 0 {
                result.extend(c.to_uppercase())
            } else {
                result.extend(c.to_lowercase())
            }
        }
        Ok(result)
    }

    /// [string.codepoints](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·codepoints
    /// ): returns an iterable of the unicode codepoint of a string.
    ///
    /// `S.codepoints()` returns an iterable value containing the
    /// sequence of integer Unicode code points encoded by the string S.
    /// Each invalid code within the string is treated as if it encodes the
    /// Unicode replacement character, U+FFFD.
    ///
    /// By returning an iterable, not a list, the cost of decoding the string
    /// is deferred until actually needed; apply `list(...)` to the result to
    /// materialize the entire sequence.
    ///
    /// Example:
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// list("Hello, 世界".codepoints()) == [72, 101, 108, 108, 111, 44, 32, 19990, 30028]
    /// # "#);
    /// ```
    fn codepoints<'v>(this: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(iterate_codepoints(this, heap))
    }

    /// [string.count](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·count
    /// ): count the number of occurrences of a string in another string.
    ///
    /// `S.count(sub[, start[, end]])` returns the number of occcurences of
    /// `sub` within the string S, or, if the optional substring indices
    /// `start` and `end` are provided, within the designated substring of S.
    /// They are interpreted according to Skylark's [indexing conventions](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#indexing).
    ///
    /// This implementation does not count occurence of `sub` in the string `S`
    /// that overlap other occurence of S (which can happen if some suffix of S
    /// is a prefix of S). For instance, `"abababa".count("aba")` returns 2
    /// for `[aba]a[aba]`, not counting the middle occurence: `ab[aba]ba`
    /// (this is following Python behavior).
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "hello, world!".count("o") == 2
    /// "abababa".count("aba") == 2
    /// "hello, world!".count("o", 7, 12) == 1  # in "world"
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn count(
        mut this: &str,
        #[starlark(require = pos)] needle: &str,
        #[starlark(require = pos, default = NoneOr::None)] start: NoneOr<i32>,
        #[starlark(require = pos, default = NoneOr::None)] end: NoneOr<i32>,
    ) -> anyhow::Result<i32> {
        if let Some(StrIndices { haystack, .. }) = convert_str_indices(this, start, end) {
            Ok(fast_string::count_matches(haystack, needle) as i32)
        } else {
            Ok(0)
        }
    }

    /// [string.endswith](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·endswith
    /// ): determine if a string ends with a given suffix.
    ///
    /// `S.endswith(suffix)` reports whether the string S has the specified
    /// suffix.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "filename.sky".endswith(".sky") == True
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn endswith(
        this: &str,
        #[starlark(require = pos)] suffix: StringOrTuple,
    ) -> anyhow::Result<bool> {
        match suffix {
            StringOrTuple::String(x) => Ok(this.ends_with(x)),
            StringOrTuple::Tuple(xs) => Ok(xs.iter().any(|x| this.ends_with(x))),
        }
    }

    /// [string.find](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·find
    /// ): find a substring in a string.
    ///
    /// `S.find(sub[, start[, end]])` returns the index of the first
    /// occurrence of the substring `sub` within S.
    ///
    /// If either or both of `start` or `end` are specified,
    /// they specify a subrange of S to which the search should be restricted.
    /// They are interpreted according to Skylark's [indexing
    /// conventions](#indexing).
    ///
    /// If no occurrence is found, `found` returns -1.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "bonbon".find("on") == 1
    /// "bonbon".find("on", 2) == 4
    /// "bonbon".find("on", 2, 5) == -1
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn find(
        this: &str,
        #[starlark(require = pos)] needle: &str,
        #[starlark(require = pos, default = NoneOr::None)] start: NoneOr<i32>,
        #[starlark(require = pos, default = NoneOr::None)] end: NoneOr<i32>,
    ) -> anyhow::Result<i32> {
        if let Some(StrIndices { start, haystack }) = convert_str_indices(this, start, end) {
            if let Some(index) = haystack.find(needle) {
                let index = fast_string::len(&haystack[..index]);
                return Ok((start + index).0 as i32);
            }
        }
        Ok(-1)
    }

    /// [string.format](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·format
    /// ): format a string.
    ///
    /// `S.format(*args, **kwargs)` returns a version of the format string S
    /// in which bracketed portions `{...}` are replaced
    /// by arguments from `args` and `kwargs`.
    ///
    /// Within the format string, a pair of braces `{{` or `}}` is treated as
    /// a literal open or close brace.
    /// Each unpaired open brace must be matched by a close brace `}`.
    /// The optional text between corresponding open and close braces
    /// specifies which argument to use and how to format it, and consists of
    /// three components, all optional:
    /// a field name, a conversion preceded by '`!`', and a format specifier
    /// preceded by '`:`'.
    ///
    /// ```text
    /// {field}
    /// {field:spec}
    /// {field!conv}
    /// {field!conv:spec}
    /// ```
    ///
    /// The *field name* may be either a decimal number or a keyword.
    /// A number is interpreted as the index of a positional argument;
    /// a keyword specifies the value of a keyword argument.
    /// If all the numeric field names form the sequence 0, 1, 2, and so on,
    /// they may be omitted and those values will be implied; however,
    /// the explicit and implicit forms may not be mixed.
    ///
    /// The *conversion* specifies how to convert an argument value `x` to a
    /// string. It may be either `!r`, which converts the value using
    /// `repr(x)`, or `!s`, which converts the value using `str(x)` and is
    /// the default.
    ///
    /// The *format specifier*, after a colon, specifies field width,
    /// alignment, padding, and numeric precision.
    /// Currently it must be empty, but it is reserved for future use.
    ///
    /// Examples:
    ///
    /// ```rust
    /// # starlark::assert::all_true(r#"
    /// "a {} c".format(3) == "a 3 c"
    /// "a{x}b{y}c{}".format(1, x=2, y=3) == "a2b3c1"
    /// "a{}b{}c".format(1, 2) == "a1b2c"
    /// "({1}, {0})".format("zero", "one") == "(one, zero)"
    /// "Is {0!r} {0!s}?".format("heterological") == "Is \"heterological\" heterological?"
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn format<'v>(
        this: &str,
        args: &Arguments<'v, '_>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StringValue<'v>> {
        let iter = args.positions(eval.heap())?;
        interpolation::format(
            this,
            iter,
            args.names()?,
            &mut eval.string_pool,
            eval.module_env.heap(),
        )
    }

    /// [string.index](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·index
    /// ): search a substring inside a string, failing on not found.
    ///
    /// `S.index(sub[, start[, end]])` returns the index of the first
    /// occurrence of the substring `sub` within S, like `S.find`, except
    /// that if the substring is not found, the operation fails.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "bonbon".index("on") == 1
    /// "bonbon".index("on", 2) == 4
    /// # "#);
    /// # starlark::assert::fail(r#"
    /// "bonbon".index("on", 2, 5)    # error: not found
    /// # "#, "not found");
    /// ```
    #[starlark(speculative_exec_safe)]
    fn index(
        this: &str,
        #[starlark(require = pos)] needle: &str,
        #[starlark(require = pos, default = NoneOr::None)] start: NoneOr<i32>,
        #[starlark(require = pos, default = NoneOr::None)] end: NoneOr<i32>,
    ) -> anyhow::Result<i32> {
        if let Some(StrIndices { start, haystack }) = convert_str_indices(this, start, end) {
            if let Some(index) = haystack.find(needle) {
                let index = fast_string::len(&haystack[..index]);
                return Ok((start + index).0 as i32);
            }
        }
        Err(anyhow!("Substring '{}' not found in '{}'", needle, this))
    }

    /// [string.isalnum](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·isalnum
    /// ): test if a string is composed only of letters and digits.
    ///
    /// `S.isalnum()` reports whether the string S is non-empty and consists
    /// only Unicode letters and digits.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "base64".isalnum() == True
    /// "Catch-22".isalnum() == False
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn isalnum(this: &str) -> anyhow::Result<bool> {
        if this.is_empty() {
            return Ok(false);
        }
        for c in this.chars() {
            if !c.is_alphanumeric() {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// [string.isalpha](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·isalpha
    /// ): test if a string is composed only of letters.
    ///
    /// `S.isalpha()` reports whether the string S is non-empty and consists
    /// only of Unicode letters.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "ABC".isalpha() == True
    /// "Catch-22".isalpha() == False
    /// "".isalpha() == False
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn isalpha(this: &str) -> anyhow::Result<bool> {
        if this.is_empty() {
            return Ok(false);
        }
        for c in this.chars() {
            if !c.is_alphabetic() {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// [string.isdigit](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·isdigit
    /// ): test if a string is composed only of digits.
    ///
    /// `S.isdigit()` reports whether the string S is non-empty and consists
    /// only of Unicode digits.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "123".isdigit() == True
    /// "Catch-22".isdigit() == False
    /// "".isdigit() == False
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn isdigit(this: &str) -> anyhow::Result<bool> {
        if this.is_empty() {
            return Ok(false);
        }
        for c in this.chars() {
            if !c.is_numeric() {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// [string.islower](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·islower
    /// ): test if all letters of a string are lowercase.
    ///
    /// `S.islower()` reports whether the string S contains at least one cased
    /// Unicode letter, and all such letters are lowercase.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "hello, world".islower() == True
    /// "Catch-22".islower() == False
    /// "123".islower() == False
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn islower(this: &str) -> anyhow::Result<bool> {
        let mut result = false;
        for c in this.chars() {
            if c.is_uppercase() {
                return Ok(false);
            } else if c.is_lowercase() {
                result = true;
            }
        }
        Ok(result)
    }

    /// [string.isspace](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·isspace
    /// ): test if all characters of a string are whitespaces.
    ///
    /// `S.isspace()` reports whether the string S is non-empty and consists
    /// only of Unicode spaces.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "    ".isspace() == True
    /// "\r\t\n".isspace() == True
    /// "".isspace() == False
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn isspace(this: &str) -> anyhow::Result<bool> {
        if this.is_empty() {
            return Ok(false);
        }
        for c in this.chars() {
            if !c.is_whitespace() {
                return Ok(false);
            }
        }
        Ok(true)
    }

    /// [string.istitle](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·istitle
    /// ): test if the string is title cased.
    ///
    /// `S.istitle()` reports whether the string S contains at least one cased
    /// Unicode letter, and all such letters that begin a word are in title
    /// case.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "Hello, World!".istitle() == True
    /// "Catch-22".istitle() == True
    /// "HAL-9000".istitle() == False
    /// "123".istitle() == False
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn istitle(this: &str) -> anyhow::Result<bool> {
        let mut last_space = true;
        let mut result = false;

        for c in this.chars() {
            if !c.is_alphabetic() {
                last_space = true;
            } else {
                if last_space {
                    if c.is_lowercase() {
                        return Ok(false);
                    }
                } else if c.is_uppercase() {
                    return Ok(false);
                }
                if c.is_alphabetic() {
                    result = true;
                }
                last_space = false;
            }
        }
        Ok(result)
    }

    /// [string.isupper](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·isupper
    /// ): test if all letters of a string are uppercase.
    ///
    /// `S.isupper()` reports whether the string S contains at least one cased
    /// Unicode letter, and all such letters are uppercase.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "HAL-9000".isupper() == True
    /// "Catch-22".isupper() == False
    /// "123".isupper() == False
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn isupper(this: &str) -> anyhow::Result<bool> {
        let mut result = false;
        for c in this.chars() {
            if c.is_lowercase() {
                return Ok(false);
            } else if c.is_uppercase() {
                result = true;
            }
        }
        Ok(result)
    }

    /// [string.lower](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·lower
    /// ): test if all letters of a string are lowercased.
    ///
    /// `S.lower()` returns a copy of the string S with letters converted to
    /// lowercase.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "Hello, World!".lower() == "hello, world!"
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn lower(this: &str) -> anyhow::Result<String> {
        Ok(this.to_lowercase())
    }

    /// [string.join](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·join
    /// ): join elements with a separator.
    ///
    /// `S.join(iterable)` returns the string formed by concatenating each
    /// element of its argument, with a copy of the string S between
    /// successive elements. The argument must be an iterable whose elements
    /// are strings.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// ", ".join([]) == ""
    /// ", ".join(("x", )) == "x"
    /// ", ".join(["one", "two", "three"]) == "one, two, three"
    /// "a".join("ctmrn".elems()) == "catamaran"
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn join<'v>(
        this: &str,
        #[starlark(require = pos)] to_join: Value<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        #[inline(always)]
        fn as_str<'v>(x: Value<'v>) -> anyhow::Result<&'v str> {
            <&str>::unpack_named_param(x, "to_join")
        }

        to_join.with_iterator(heap, |it| {
            match it.next() {
                None => Ok(Value::new_empty_string()),
                Some(x1) => {
                    match it.next() {
                        None => {
                            as_str(x1)?;
                            // If there is a singleton we can avoid reallocation
                            Ok(x1)
                        }
                        Some(x2) => {
                            let s1 = as_str(x1)?;
                            let s2 = as_str(x2)?;
                            // guess towards the upper bound, since we throw away over-allocations quickly
                            // include a buffer (20 bytes)
                            let n = it.size_hint().0 + 2;
                            let guess =
                                (cmp::max(s1.len(), s2.len()) * n) + (this.len() * (n - 1)) + 20;
                            let mut r = String::with_capacity(guess);
                            r.push_str(s1);
                            r.push_str(this);
                            r.push_str(s2);
                            for x in it {
                                r.push_str(this);
                                r.push_str(as_str(x)?);
                            }
                            Ok(heap.alloc(r))
                        }
                    }
                }
            }
        })?
    }

    /// [string.lstrip](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·lstrip
    /// ): trim leading whitespaces.
    ///
    /// `S.lstrip()` returns a copy of the string S with leading whitespace removed.
    /// In most cases instead of passing an argument you should use `removeprefix`.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "  hello  ".lstrip() == "hello  "
    /// "x!hello  ".lstrip("!x ") == "hello  "
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn lstrip<'v>(
        this: &'v str,
        #[starlark(require = pos)] chars: Option<&str>,
    ) -> anyhow::Result<&'v str> {
        match chars {
            None => Ok(this.trim_start()),
            Some(s) => Ok(this.trim_start_matches(|c| s.contains(c))),
        }
    }

    /// [string.partition](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·partition
    /// ): partition a string in 3 components
    ///
    /// `S.partition(x = " ")` splits string S into three parts and returns them
    /// as a tuple: the portion before the first occurrence of string `x`,
    /// `x` itself, and the portion following it.
    /// If S does not contain `x`, `partition` returns `(S, "", "")`.
    ///
    /// `partition` fails if `x` is not a string, or is the empty string.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "one/two/three".partition("/") == ("one", "/", "two/three")
    /// "one".partition("/") == ("one", "", "")
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn partition<'v>(
        this: ValueOf<'v, &'v str>,
        #[starlark(require = pos)] needle: ValueOf<'v, &'v str>,
        heap: &'v Heap,
    ) -> anyhow::Result<(Value<'v>, Value<'v>, Value<'v>)> {
        if needle.typed.is_empty() {
            return Err(anyhow!("Empty separator cannot be used for partitioning"));
        }
        if let Some(offset) = this.typed.find(needle.typed) {
            let offset2 = offset + needle.typed.len();
            Ok((
                heap.alloc(this.typed.get(..offset).unwrap()),
                needle.value,
                heap.alloc(this.typed.get(offset2..).unwrap()),
            ))
        } else {
            let empty = Value::new_empty_string();
            Ok((this.value, empty, empty))
        }
    }

    /// [string.replace](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·replace
    /// ): replace all occurences of a subtring.
    ///
    /// `S.replace(old, new[, count])` returns a copy of string S with all
    /// occurrences of substring `old` replaced by `new`. If the optional
    /// argument `count`, which must be an `int`, is non-negative, it
    /// specifies a maximum number of occurrences to replace.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "banana".replace("a", "o") == "bonono"
    /// "banana".replace("a", "o", 2) == "bonona"
    /// "# );
    /// # starlark::assert::fail(r#"
    /// "banana".replace("a", "o", -2)  # error: argument was negative
    /// "#, "argument was negative");
    /// ```
    #[starlark(speculative_exec_safe)]
    fn replace(
        this: &str,
        #[starlark(require = pos)] old: &str,
        #[starlark(require = pos)] new: &str,
        #[starlark(require = pos)] count: Option<i32>,
    ) -> anyhow::Result<String> {
        match count {
            Some(count) if count >= 0 => Ok(this.replacen(old, new, count as usize)),
            Some(count) => Err(anyhow!("Replace final argument was negative '{}'", count)),
            None => Ok(this.replace(old, new)),
        }
    }

    /// [string.rfind](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·rfind
    /// ): find the last index of a substring.
    ///
    /// `S.rfind(sub[, start[, end]])` returns the index of the substring `sub`
    /// within S, like `S.find`, except that `rfind` returns the index of
    /// the substring's _last_ occurrence.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "bonbon".rfind("on") == 4
    /// "bonbon".rfind("on", None, 5) == 1
    /// "bonbon".rfind("on", 2, 5) == -1
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn rfind(
        this: &str,
        #[starlark(require = pos)] needle: &str,
        #[starlark(require = pos, default = NoneOr::None)] start: NoneOr<i32>,
        #[starlark(require = pos, default = NoneOr::None)] end: NoneOr<i32>,
    ) -> anyhow::Result<i32> {
        if let Some(StrIndices { start, haystack }) = convert_str_indices(this, start, end) {
            if let Some(index) = haystack.rfind(needle) {
                let index = fast_string::len(&haystack[..index]);
                return Ok((start + index).0 as i32);
            }
        }
        Ok(-1)
    }

    /// [string.rindex](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·rindex
    /// ): find the last index of a substring, failing on not found.
    ///
    /// `S.rindex(sub[, start[, end]])` returns the index of the substring `sub`
    /// within S, like `S.index`, except that `rindex` returns the index of
    /// the substring's _last_ occurrence.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "bonbon".rindex("on") == 4
    /// "bonbon".rindex("on", None, 5) == 1  # in "bonbo"
    /// # "#);
    /// # starlark::assert::fail(r#"
    /// "bonbon".rindex("on", 2, 5) #   error: not found
    /// # "#, "not found");
    /// ```
    #[starlark(speculative_exec_safe)]
    fn rindex(
        this: &str,
        #[starlark(require = pos)] needle: &str,
        #[starlark(require = pos, default = NoneOr::None)] start: NoneOr<i32>,
        #[starlark(require = pos, default = NoneOr::None)] end: NoneOr<i32>,
    ) -> anyhow::Result<i32> {
        if let Some(StrIndices { start, haystack }) = convert_str_indices(this, start, end) {
            if let Some(index) = haystack.rfind(needle) {
                let index = fast_string::len(&haystack[..index]);
                return Ok((start + index).0 as i32);
            }
        }
        Err(anyhow!("Substring '{}' not found in '{}'", needle, this))
    }

    /// [string.rpartition](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·rpartition
    /// ): partition a string in 3 elements.
    ///
    /// `S.rpartition([x = ' '])` is like `partition`, but splits `S` at the
    /// last occurrence of `x`.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "one/two/three".rpartition("/") == ("one/two", "/", "three")
    /// "one".rpartition("/") == ("", "", "one")
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn rpartition<'v>(
        this: ValueOf<'v, &'v str>,
        #[starlark(require = pos)] needle: ValueOf<'v, &'v str>,
        heap: &'v Heap,
    ) -> anyhow::Result<(Value<'v>, Value<'v>, Value<'v>)> {
        if needle.typed.is_empty() {
            return Err(anyhow!("Empty separator cannot be used for partitioning"));
        }
        if let Some(offset) = this.typed.rfind(needle.typed) {
            let offset2 = offset + needle.typed.len();
            Ok((
                heap.alloc(this.typed.get(..offset).unwrap()),
                needle.value,
                heap.alloc(this.typed.get(offset2..).unwrap()),
            ))
        } else {
            let empty = Value::new_empty_string();
            Ok((empty, empty, this.value))
        }
    }

    /// [string.rsplit](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·rsplit
    /// ): splits a string into substrings.
    ///
    /// `S.rsplit([sep[, maxsplit]])` splits a string into substrings like
    /// `S.split`, except that when a maximum number of splits is specified,
    /// `rsplit` chooses the rightmost splits.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "banana".rsplit("n") == ["ba", "a", "a"]
    /// "banana".rsplit("n", 1) == ["bana", "a"]
    /// "one two  three".rsplit(None, 1) == ["one two", "three"]
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn rsplit<'v>(
        this: &str,
        #[starlark(require = pos, default = NoneOr::None)] sep: NoneOr<&str>,
        #[starlark(require = pos, default = NoneOr::None)] maxsplit: NoneOr<i32>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let maxsplit = match maxsplit.into_option() {
            None => None,
            Some(v) => {
                if v < 0 {
                    None
                } else {
                    Some((v + 1) as usize)
                }
            }
        };
        Ok(heap.alloc_list(&match sep.into_option() {
            None => match maxsplit {
                None => this.split_whitespace().map(|x| heap.alloc(x)).collect(),
                Some(maxsplit) => rsplitn_whitespace(this, maxsplit).map(|x| heap.alloc(x)),
            },
            Some(sep) => {
                let mut v: Vec<_> = match maxsplit {
                    None => this.rsplit(sep).map(|x| heap.alloc(x)).collect(),
                    Some(maxsplit) => this.rsplitn(maxsplit, sep).map(|x| heap.alloc(x)).collect(),
                };
                v.reverse();
                v
            }
        }))
    }

    /// [string.rstrip](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·rstrip
    /// ): trim trailing whitespace.
    ///
    /// `S.rstrip()` returns a copy of the string S with trailing whitespace removed.
    /// In most cases instead of passing an argument you should use `removesuffix`.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "  hello  ".rstrip() == "  hello"
    /// "  hello!x".rstrip(" x!") == "  hello"
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn rstrip<'v>(
        this: &'v str,
        #[starlark(require = pos)] chars: Option<&str>,
    ) -> anyhow::Result<&'v str> {
        match chars {
            None => Ok(this.trim_end()),
            Some(s) => Ok(this.trim_end_matches(|c| s.contains(c))),
        }
    }

    /// [string.split](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·split
    /// ): split a string in substrings.
    ///
    /// `S.split([sep [, maxsplit]])` returns the list of substrings of S,
    /// splitting at occurrences of the delimiter string `sep`.
    ///
    /// Consecutive occurrences of `sep` are considered to delimit empty
    /// strings, so `'food'.split('o')` returns `['f', '', 'd']`.
    /// Splitting an empty string with a specified separator returns `['']`.
    /// If `sep` is the empty string, `split` fails.
    ///
    /// If `sep` is not specified or is `None`, `split` uses a different
    /// algorithm: it removes all leading spaces from S
    /// (or trailing spaces in the case of `rsplit`),
    /// then splits the string around each consecutive non-empty sequence of
    /// Unicode white space characters.
    ///
    /// If S consists only of white space, `split` returns the empty list.
    ///
    /// If `maxsplit` is given and non-negative, it specifies a maximum number
    /// of splits.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "one two  three".split() == ["one", "two", "three"]
    /// "one two  three".split(" ") == ["one", "two", "", "three"]
    /// "one two  three".split(None, 1) == ["one", "two  three"]
    /// "banana".split("n") == ["ba", "a", "a"]
    /// "banana".split("n", 1) == ["ba", "ana"]
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn split<'v>(
        this: &str,
        #[starlark(require = pos, default = NoneOr::None)] sep: NoneOr<&str>,
        #[starlark(require = pos, default = NoneOr::None)] maxsplit: NoneOr<i32>,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let maxsplit = match maxsplit.into_option() {
            None => None,
            Some(v) => {
                if v < 0 {
                    None
                } else {
                    Some((v + 1) as usize)
                }
            }
        };
        Ok(heap.alloc_list(&match (sep.into_option(), maxsplit) {
            (None, None) => this.split_whitespace().map(|x| heap.alloc(x)).collect(),
            (None, Some(maxsplit)) => splitn_whitespace(this, maxsplit).map(|x| heap.alloc(x)),
            (Some(sep), None) => {
                if sep.len() == 1 {
                    // If we are searching for a 1-byte string, we can provide a much faster path.
                    // Since it is one byte, given how UTF8 works, all the resultant slices must be UTF8 too.
                    let b = sep.as_bytes()[0];
                    let count = fast_string::count_matches_byte(this, b);
                    let mut res = Vec::with_capacity(count + 1);
                    res.extend(
                        this.as_bytes()
                            .split(|x| *x == b)
                            .map(|x| heap.alloc(unsafe { std::str::from_utf8_unchecked(x) })),
                    );
                    debug_assert_eq!(res.len(), count + 1);
                    res
                } else {
                    this.split(sep).map(|x| heap.alloc(x)).collect()
                }
            }
            (Some(sep), Some(maxsplit)) => {
                this.splitn(maxsplit, sep).map(|x| heap.alloc(x)).collect()
            }
        }))
    }

    /// [string.splitlines](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·splitlines
    /// ): return the list of lines of a string.
    ///
    /// `S.splitlines([keepends])` returns a list whose elements are the
    /// successive lines of S, that is, the strings formed by splitting S at
    /// line terminators ('\n', '\r' or '\r\n').
    ///
    /// The optional argument, `keepends`, is interpreted as a Boolean.
    /// If true, line terminators are preserved in the result, though
    /// the final element does not necessarily end with a line terminator.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "one\n\ntwo".splitlines() == ["one", "", "two"]
    /// "one\n\ntwo".splitlines(True) == ["one\n", "\n", "two"]
    /// "a\nb".splitlines() == ["a", "b"]
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn splitlines<'v>(
        this: &str,
        #[starlark(require = pos, default = false)] keepends: bool,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let mut s = this;
        let mut lines = Vec::new();
        loop {
            if let Some(x) = s.find(|x| x == '\n' || x == '\r') {
                let y = x;
                let x = match s.get(y..y + 2) {
                    Some("\r\n") => y + 2,
                    _ => y + 1,
                };
                if keepends {
                    lines.push(heap.alloc(s.get(..x).unwrap()))
                } else {
                    lines.push(heap.alloc(s.get(..y).unwrap()))
                }
                if x == s.len() {
                    return Ok(heap.alloc_list(&lines));
                }
                s = s.get(x..).unwrap();
            } else {
                if !s.is_empty() {
                    lines.push(heap.alloc(s));
                }
                return Ok(heap.alloc_list(&lines));
            }
        }
    }

    /// [string.startswith](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·startswith
    /// ): test wether a string starts with a given prefix.
    ///
    /// `S.startswith(suffix)` reports whether the string S has the specified
    /// prefix.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "filename.sky".startswith("filename") == True
    /// "filename.sky".startswith("sky") == False
    /// 'abc'.startswith(('a', 'A')) == True
    /// 'ABC'.startswith(('a', 'A')) == True
    /// 'def'.startswith(('a', 'A')) == False
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn startswith(
        this: &str,
        #[starlark(require = pos)] prefix: StringOrTuple,
    ) -> anyhow::Result<bool> {
        match prefix {
            StringOrTuple::String(x) => Ok(this.starts_with(x)),
            StringOrTuple::Tuple(xs) => Ok(xs.iter().any(|x| this.starts_with(x))),
        }
    }

    /// [string.strip](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·strip
    /// ): trim leading and trailing whitespaces.
    ///
    /// `S.strip()` returns a copy of the string S with leading and trailing
    /// whitespace removed.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "  hello  ".strip() == "hello"
    /// "xxhello!!".strip("x!") == "hello"
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn strip<'v>(
        this: &'v str,
        #[starlark(require = pos)] chars: Option<&str>,
    ) -> anyhow::Result<&'v str> {
        match chars {
            None => Ok(this.trim()),
            Some(s) => Ok(this.trim_matches(|c| s.contains(c))),
        }
    }

    /// [string.title](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·title
    /// ): convert a string to title case.
    ///
    /// `S.lower()` returns a copy of the string S with letters converted to
    /// titlecase.
    ///
    /// Letters are converted to uppercase at the start of words, lowercase
    /// elsewhere.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "hElLo, WoRlD!".title() == "Hello, World!"
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn title(this: &str) -> anyhow::Result<String> {
        let mut last_space = true;
        let mut result = String::with_capacity(this.len());
        for c in this.chars() {
            if !c.is_alphabetic() {
                last_space = true;
                result.extend(c.to_lowercase());
            } else {
                if last_space {
                    result.extend(c.to_uppercase())
                } else {
                    result.extend(c.to_lowercase())
                }
                last_space = false;
            }
        }
        Ok(result)
    }

    /// [string.upper](
    /// https://github.com/google/skylark/blob/3705afa472e466b8b061cce44b47c9ddc6db696d/doc/spec.md#string·upper
    /// ): convert a string to all uppercase.
    ///
    /// `S.lower()` returns a copy of the string S with letters converted to
    /// lowercase.
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "Hello, World!".upper() == "HELLO, WORLD!"
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn upper(this: &str) -> anyhow::Result<String> {
        Ok(this.to_uppercase())
    }

    /// [string.removeprefix](
    /// https://docs.python.org/3.9/library/stdtypes.html#str.removeprefix
    /// ): remove a prefix from a string. _Not part of standard Starlark._
    ///
    /// If the string starts with the prefix string, return `string[len(prefix):]`.
    /// Otherwise, return a copy of the original string:
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "Hello, World!".removeprefix("Hello") == ", World!"
    /// "Hello, World!".removeprefix("Goodbye") == "Hello, World!"
    /// "Hello".removeprefix("Hello") == ""
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn removeprefix<'v>(
        this: Value<'v>,
        #[starlark(require = pos)] prefix: &str,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let x = this.unpack_str().unwrap();
        if x.starts_with(prefix) && !prefix.is_empty() {
            Ok(heap.alloc(&x[prefix.len()..]))
        } else {
            Ok(this)
        }
    }

    /// [string.removesuffix](
    /// https://docs.python.org/3.9/library/stdtypes.html#str.removesuffix
    /// ): remove a prefix from a string. _Not part of standard Starlark._
    ///
    /// If the string starts with the prefix string, return `string[len(prefix):]`.
    /// Otherwise, return a copy of the original string:
    ///
    /// Examples:
    ///
    /// ```
    /// # starlark::assert::all_true(r#"
    /// "Hello, World!".removesuffix("World!") == "Hello, "
    /// "Hello, World!".removesuffix("World") == "Hello, World!"
    /// "Hello".removesuffix("Hello") == ""
    /// # "#);
    /// ```
    #[starlark(speculative_exec_safe)]
    fn removesuffix<'v>(
        this: Value<'v>,
        #[starlark(require = pos)] suffix: &str,
        heap: &'v Heap,
    ) -> anyhow::Result<Value<'v>> {
        let x = this.unpack_str().unwrap();
        if x.ends_with(suffix) && !suffix.is_empty() {
            Ok(heap.alloc(&x[..x.len() - suffix.len()]))
        } else {
            Ok(this)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::assert;

    #[test]
    fn test_error_codes() {
        assert::fail(r#""bonbon".index("on", 2, 5)"#, "not found in");
        assert::fail(r#"("banana".replace("a", "o", -2))"#, "negative");
        assert::fail(r#""bonbon".rindex("on", 2, 5)"#, "not found in");
    }

    #[test]
    fn test_count() {
        assert::eq("'abc'.count('a', 10, -10)", "0");
    }

    #[test]
    fn test_find() {
        assert::eq("'Троянская война окончена'.find('война')", "10");
    }

    #[test]
    fn test_opaque_iterator() {
        assert::is_true("type('foo'.elems()) != type([])");
        assert::is_true("type('foo'.codepoints()) != type([])");
    }
}
