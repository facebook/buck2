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

//! String interpolation-related code.
//! Based on <https://docs.python.org/3/library/stdtypes.html#printf-style-string-formatting>

use std::{fmt::Write, mem, str::FromStr};

use anyhow::anyhow;
use gazebo::{cast, prelude::*};
use thiserror::Error;

use crate::{
    collections::string_pool::StringPool,
    values::{
        dict::Dict, float, num, num::Num, tuple::Tuple, Heap, StringValue, UnpackValue, Value,
        ValueError, ValueLike,
    },
};

/// Operator `%` format or evaluation errors
#[derive(Clone, Dupe, Debug, Error)]
enum StringInterpolationError {
    /// Interpolation parameter is too big for the format string.
    #[error("Too many arguments for format string")]
    TooManyParameters,
    /// Interpolation parameter is too small for the format string.
    #[error("Not enough arguments for format string")]
    NotEnoughParameters,
}

pub(crate) fn percent(format: &str, value: Value) -> anyhow::Result<String> {
    // For performance reasons, we treat format as a list of bytes
    // (which is fine, the only thing we care about are '%' and ASCII digits).
    // As a result, we accumulate into a Vec<u8>, which we know at any point
    // we are at the end or at a '%' must be a valid UTF8 buffer.

    // NOTE(nga): use could reuse `Evaluator::string_pool` here, but
    //   * we don't have access to `Evaluator` in `StarlarkValue::percent`
    //   * after single %s made intrinsic, this code is not that hot now

    // random guess as a baseline capacity
    let mut res: Vec<u8> = Vec::with_capacity(format.len() + 20);

    let tuple = Tuple::from_value(value);
    let one = &[value];
    let values = match &tuple {
        Some(xs) => xs.content(),
        None => one,
    };
    let mut values = values.iter().copied();
    let mut next_value = || -> anyhow::Result<Value> {
        values
            .next()
            .ok_or_else(|| StringInterpolationError::NotEnoughParameters.into())
    };

    // because of the way format is defined, we can deal with it as bytes
    let mut format = format.as_bytes().iter().copied();
    while let Some(c) = format.next() {
        if c == b'%' {
            if let Some(c) = format.next() {
                let out: &mut String = unsafe { cast::ptr_mut(&mut res) };
                match c {
                    b'%' => res.push(b'%'),
                    b's' => {
                        let arg = next_value()?;
                        match arg.unpack_str() {
                            None => arg.collect_repr(out),
                            Some(s) => out.push_str(s),
                        }
                    }
                    b'r' => next_value()?.collect_repr(out),
                    b'd' => {
                        let value = next_value()?;
                        if let Some(num::Num::Float(v)) = value.unpack_num() {
                            match num::Num::Float(v.trunc()).as_int() {
                                None => {
                                    return ValueError::unsupported(&float::StarlarkFloat(v), "%d");
                                }
                                Some(v) => write!(out, "{}", v).unwrap(),
                            }
                        } else {
                            write!(out, "{}", value.to_int()?).unwrap()
                        }
                    }
                    b'o' => {
                        let v = next_value()?.to_int()?;
                        write!(
                            out,
                            "{}{:o}",
                            if v < 0 { "-" } else { "" },
                            v.wrapping_abs() as u64
                        )
                        .unwrap();
                    }
                    b'x' => {
                        let v = next_value()?.to_int()?;
                        write!(
                            out,
                            "{}{:x}",
                            if v < 0 { "-" } else { "" },
                            v.wrapping_abs() as u64
                        )
                        .unwrap();
                    }
                    b'X' => {
                        let v = next_value()?.to_int()?;
                        write!(
                            out,
                            "{}{:X}",
                            if v < 0 { "-" } else { "" },
                            v.wrapping_abs() as u64
                        )
                        .unwrap()
                    }
                    b'e' => {
                        let v = Num::unpack_param(next_value()?)?.as_float();
                        float::write_scientific(out, v, 'e', false).unwrap()
                    }
                    b'E' => {
                        let v = Num::unpack_param(next_value()?)?.as_float();
                        float::write_scientific(out, v, 'E', false).unwrap()
                    }
                    b'f' | b'F' => {
                        let v = Num::unpack_param(next_value()?)?.as_float();
                        float::write_decimal(out, v).unwrap()
                    }
                    b'g' => {
                        let v = Num::unpack_param(next_value()?)?.as_float();
                        float::write_compact(out, v, 'e').unwrap()
                    }
                    b'G' => {
                        let v = Num::unpack_param(next_value()?)?.as_float();
                        float::write_compact(out, v, 'E').unwrap()
                    }
                    c => {
                        res.push(b'%');
                        res.push(c);
                    }
                }
            } else {
                res.push(b'%');
            }
        } else {
            res.push(c);
        }
    }
    if values.next().is_some() {
        Err(StringInterpolationError::TooManyParameters.into())
    } else {
        Ok(unsafe { String::from_utf8_unchecked(res) })
    }
}

/// Try parse `"aaa{}bbb"` and return `("aaa", "bbb")`.
pub(crate) fn parse_format_one(s: &str) -> Option<(String, String)> {
    let mut parser = FormatParser {
        format_str: s,
        rem_input: s,
    };
    let mut before = String::with_capacity(s.len());
    loop {
        match parser.next().ok()?? {
            FormatToken::Text(text) => before.push_str(text),
            FormatToken::Capture("") => break,
            FormatToken::Capture(_) => return None,
        }
    }

    let mut after = String::with_capacity(s.len() - before.len());
    loop {
        match parser.next().ok()? {
            Some(FormatToken::Text(text)) => after.push_str(text),
            Some(FormatToken::Capture(_)) => return None,
            None => break,
        }
    }

    Some((before, after))
}

/// Try parse `"aaa%sbbb"` and return `("aaa", "bbb")`.
pub(crate) fn parse_percent_s_one(format: &str) -> Option<(String, String)> {
    let mut before = String::with_capacity(format.len());
    let mut chars = format.chars();
    loop {
        match chars.next()? {
            '%' => match chars.next()? {
                '%' => before.push('%'),
                's' => break,
                _ => return None,
            },
            c => before.push(c),
        }
    }
    let mut after = String::with_capacity(format.len() - before.len());
    loop {
        match chars.next() {
            Some('%') => match chars.next()? {
                '%' => after.push('%'),
                _ => return None,
            },
            Some(c) => after.push(c),
            None => break,
        }
    }
    Some((before, after))
}

/// Evaluate `"<before>{}<after>".format(arg)`.
pub(crate) fn format_one<'v>(
    before: &str,
    arg: Value<'v>,
    after: &str,
    heap: &'v Heap,
) -> StringValue<'v> {
    match StringValue::new(arg) {
        Some(arg) => heap.alloc_str_concat3(before, &arg, after),
        None => {
            let mut result = String::with_capacity(before.len() + after.len() + 10);
            result.push_str(before);
            arg.collect_repr(&mut result);
            result.push_str(after);
            heap.alloc_str(&result)
        }
    }
}

/// Evaluate `"<before>%s<after>" % arg`.
pub(crate) fn percent_s_one<'v>(
    before: &str,
    arg: Value<'v>,
    after: &str,
    heap: &'v Heap,
) -> anyhow::Result<StringValue<'v>> {
    Ok(match StringValue::new(arg) {
        Some(arg) => heap.alloc_str_concat3(before, &arg, after),
        None => {
            let one = match Tuple::from_value(arg) {
                Some(tuple) => match tuple.content() {
                    [] => return Err(StringInterpolationError::NotEnoughParameters.into()),
                    [value] => *value,
                    [_, _, ..] => return Err(StringInterpolationError::TooManyParameters.into()),
                },
                None => arg,
            };
            format_one(before, one, after, heap)
        }
    })
}

/// The format string can either have explicit indices,
/// or grab things sequentially, but not both.
/// FormatArgs knows which we are doing and keeps them in mind.
struct FormatArgs<'v, T: Iterator<Item = Value<'v>>> {
    // Initially we have the iterator set and the args empty.
    // If we ever ask by index, we decant the iterator into args.
    iterator: T,
    args: Vec<Value<'v>>,
    by_index: bool,
    by_order: bool,
}

impl<'v, T: Iterator<Item = Value<'v>>> FormatArgs<'v, T> {
    fn new(iterator: T) -> Self {
        Self {
            iterator,
            args: Vec::new(),
            by_index: false,
            by_order: false,
        }
    }

    fn next_ordered(&mut self) -> anyhow::Result<Value<'v>> {
        if self.by_index {
            Err(anyhow!(
                "Cannot mix manual field specification and automatic field numbering in format string",
            ))
        } else {
            self.by_order = true;
            match self.iterator.next() {
                None => Err(anyhow!("Not enough parameters in format string")),
                Some(x) => Ok(x),
            }
        }
    }

    fn by_index(&mut self, index: usize) -> anyhow::Result<Value<'v>> {
        if self.by_order {
            Err(anyhow!(
                "Cannot mix manual field specification and automatic field numbering in format string",
            ))
        } else {
            if !self.by_index {
                self.args.extend(&mut self.iterator);
                self.by_index = true;
            }
            match self.args.get(index) {
                None => Err(ValueError::IndexOutOfBound(index as i32).into()),
                Some(v) => Ok(*v),
            }
        }
    }
}

/// Parser for `.format()` arguments.
struct FormatParser<'a> {
    /// The original format string.
    format_str: &'a str,
    /// Remaining part.
    rem_input: &'a str,
}

/// Token in the format string.
#[derive(Debug)]
enum FormatToken<'a> {
    /// Text to copy verbatim to the output.
    Text(&'a str),
    /// Format part inside curly braces.
    Capture(&'a str),
}

impl<'a> FormatParser<'a> {
    /// Parse the next token from the format string.
    fn next(&mut self) -> anyhow::Result<Option<FormatToken<'a>>> {
        let mut i = 0;

        while i < self.rem_input.len() {
            match self.rem_input.as_bytes()[i] {
                b'{' | b'}' if i != 0 => {
                    let (text, rem) = self.rem_input.split_at(i);
                    self.rem_input = rem;
                    return Ok(Some(FormatToken::Text(text)));
                }
                b'{' => {
                    assert!(i == 0);
                    if self.rem_input.starts_with("{{") {
                        self.rem_input = &self.rem_input[2..];
                        return Ok(Some(FormatToken::Text("{")));
                    }
                    i = 1;
                    while i < self.rem_input.len() {
                        match self.rem_input.as_bytes()[i] {
                            b'}' => {
                                let capture = &self.rem_input[1..i];
                                self.rem_input = &self.rem_input[i + 1..];
                                return Ok(Some(FormatToken::Capture(capture)));
                            }
                            b'{' => {
                                break;
                            }
                            _ => i += 1,
                        }
                    }
                    return Err(anyhow!(
                        "Unmatched '{{' in format string `{}`",
                        self.format_str
                    ));
                }
                b'}' => {
                    assert!(i == 0);
                    if self.rem_input.starts_with("}}") {
                        self.rem_input = &self.rem_input[2..];
                        return Ok(Some(FormatToken::Text("}")));
                    }
                    return Err(anyhow!(
                        "Standalone '}}' in format string `{}`",
                        self.format_str
                    ));
                }
                _ => i += 1,
            }
        }

        if i == 0 {
            Ok(None)
        } else {
            let text = mem::take(&mut self.rem_input);
            Ok(Some(FormatToken::Text(text)))
        }
    }
}

pub(crate) fn format<'v>(
    this: &str,
    args: impl Iterator<Item = Value<'v>>,
    kwargs: Dict<'v>,
    string_pool: &mut StringPool,
    heap: &'v Heap,
) -> anyhow::Result<StringValue<'v>> {
    let mut parser = FormatParser {
        format_str: this,
        rem_input: this,
    };
    let mut result = string_pool.alloc();
    let mut args = FormatArgs::new(args);
    while let Some(token) = parser.next()? {
        match token {
            FormatToken::Text(text) => result.push_str(text),
            FormatToken::Capture(capture) => {
                format_capture(capture, &mut args, &kwargs, &mut result)?
            }
        }
    }
    let r = heap.alloc_str(&result);
    string_pool.release(result);
    Ok(r)
}

fn format_capture<'v, T: Iterator<Item = Value<'v>>>(
    capture: &str,
    args: &mut FormatArgs<'v, T>,
    kwargs: &Dict,
    result: &mut String,
) -> anyhow::Result<()> {
    let (n, conv) = {
        if let Some((n, conv)) = capture.split_once('!') {
            (n, conv)
        } else {
            (capture, "s")
        }
    };
    let conv_s = |x: Value, result: &mut String| x.collect_str(result);
    let conv_r = |x: Value, result: &mut String| x.collect_repr(result);
    let conv: &dyn Fn(Value, &mut String) = match conv {
        "s" => &conv_s,
        "r" => &conv_r,
        c => {
            return Err(anyhow!(
                concat!(
                    "'{}' is not a valid format string specifier, only ",
                    "'s' and 'r' are valid specifiers",
                ),
                c
            ));
        }
    };
    if n.is_empty() {
        conv(args.next_ordered()?, result);
        Ok(())
    } else if n.chars().all(|c| c.is_ascii_digit()) {
        let i = usize::from_str(n).unwrap();
        conv(args.by_index(i)?, result);
        Ok(())
    } else {
        if let Some(x) = n.chars().find(|c| match c {
            '.' | ',' | '[' | ']' => true,
            _ => false,
        }) {
            return Err(anyhow!(
                "Invalid character '{}' inside replacement field",
                x
            ));
        }
        match kwargs.get_str(n) {
            None => Err(ValueError::KeyNotFound(n.to_owned()).into()),
            Some(v) => {
                conv(v, result);
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use gazebo::coerce::coerce;

    use super::*;
    use crate::{assert, collections::SmallMap, values::Heap};

    fn format_capture_for_test<'v, T: Iterator<Item = Value<'v>>>(
        capture: &str,
        args: &mut FormatArgs<'v, T>,
        kwargs: &Dict,
    ) -> anyhow::Result<String> {
        let mut result = String::new();
        super::format_capture(capture, args, kwargs, &mut result)?;
        Ok(result)
    }

    #[test]
    fn test_format_capture() {
        let heap = Heap::new();
        let original_args = vec![heap.alloc("1"), heap.alloc("2"), heap.alloc("3")];
        let mut args = FormatArgs::new(original_args.iter().copied());
        let mut kwargs = SmallMap::new();

        kwargs.insert_hashed(heap.alloc_str("a").get_hashed(), heap.alloc("x"));
        kwargs.insert_hashed(heap.alloc_str("b").get_hashed(), heap.alloc("y"));
        kwargs.insert_hashed(heap.alloc_str("c").get_hashed(), heap.alloc("z"));
        let kwargs = Dict::new(coerce(kwargs));
        assert_eq!(
            format_capture_for_test("", &mut args, &kwargs).unwrap(),
            "1"
        );
        assert_eq!(
            format_capture_for_test("!s", &mut args, &kwargs).unwrap(),
            "2"
        );
        assert_eq!(
            format_capture_for_test("!r", &mut args, &kwargs).unwrap(),
            "\"3\""
        );
        assert_eq!(
            format_capture_for_test("a!r", &mut args, &kwargs).unwrap(),
            "\"x\""
        );
        assert_eq!(
            format_capture_for_test("a!s", &mut args, &kwargs).unwrap(),
            "x"
        );
        assert!(format_capture_for_test("1", &mut args, &kwargs).is_err());
        let mut args = FormatArgs::new(original_args.iter().copied());
        assert_eq!(
            format_capture_for_test("1", &mut args, &kwargs).unwrap(),
            "2"
        );
        assert!(format_capture_for_test("", &mut args, &kwargs).is_err());
    }

    #[test]
    fn test_format() {
        assert::eq("'a{x}b{y}c{}'.format(1, x=2, y=3)", "'a2b3c1'")
    }

    #[test]
    fn test_parse_format_one() {
        assert_eq!(
            Some(("abc".to_owned(), "def".to_owned())),
            parse_format_one("abc{}def")
        );
        assert_eq!(
            Some(("a{b".to_owned(), "c}d{".to_owned())),
            parse_format_one("a{{b{}c}}d{{")
        );
        assert_eq!(None, parse_format_one("a{"));
        assert_eq!(None, parse_format_one("a{}{}"));
        assert_eq!(None, parse_format_one("{x}"));
    }

    #[test]
    fn test_parse_percent_s_one() {
        assert_eq!(
            Some(("abc".to_owned(), "def".to_owned())),
            parse_percent_s_one("abc%sdef")
        );
        assert_eq!(
            Some(("a%b".to_owned(), "c%d%".to_owned())),
            parse_percent_s_one("a%%b%sc%%d%%")
        );
        assert_eq!(None, parse_percent_s_one("a%"));
        assert_eq!(None, parse_percent_s_one("a%s%"));
        assert_eq!(None, parse_percent_s_one("a%s%s"));
        assert_eq!(None, parse_percent_s_one("%d"));
    }
}
