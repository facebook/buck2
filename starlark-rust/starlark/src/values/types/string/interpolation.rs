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

use std::fmt::Write;

use dupe::Dupe;
use thiserror::Error;

use crate::values::float;
use crate::values::num;
use crate::values::num::Num;
use crate::values::string::dot_format::format_one;
use crate::values::types::tuple::value::Tuple;
use crate::values::Heap;
use crate::values::StringValue;
use crate::values::UnpackValue;
use crate::values::Value;
use crate::values::ValueError;
use crate::values::ValueLike;

/// Operator `%` format or evaluation errors
#[derive(Clone, Dupe, Debug, Error)]
enum StringInterpolationError {
    #[error("Too many arguments for format string")]
    TooManyParameters,
    #[error("Not enough arguments for format string")]
    NotEnoughParameters,
    #[error("Incomplete format")]
    IncompleteFormat,
    #[error("Unsupported format character: {0:?}")]
    UnsupportedFormatCharacter(char),
    #[error("Expecting format character (internal error)")]
    ExpectingFormatCharacter,
}

enum PercentSFormat {
    /// `%s`.
    Str,
    /// `%r`.
    Repr,
    /// `%d`.
    Dec,
    /// `%o`.
    Oct,
    /// `%x`.
    Hex,
    /// `%X`.
    HexUpper,
    /// `%e`.
    Exp,
    /// `%E`.
    ExpUpper,
    /// `%f` or `%F`.
    Float,
    /// `%g`.
    FloatCompact,
    /// `%G`.
    FloatCompactUpper,
}

struct PercentFormatParser<'a> {
    rem: &'a str,
}

struct Item<'a> {
    literal: &'a str,
    format: Option<PercentSFormat>,
}

impl<'a> Iterator for PercentFormatParser<'a> {
    type Item = anyhow::Result<Item<'a>>;

    #[allow(clippy::collapsible_else_if)] // Makes code more readable.
    fn next(&mut self) -> Option<Self::Item> {
        let index_of_percent = self.rem.bytes().position(|c| c == b'%');
        if let Some(index_of_percent) = index_of_percent {
            let prev_rem = self.rem;
            let (literal, rem) = self.rem.split_at(index_of_percent);
            match rem.as_bytes().get(1) {
                None => return Some(Err(StringInterpolationError::IncompleteFormat.into())),
                Some(f) => {
                    let res = match f {
                        b'%' => {
                            // Include the percent in the literal.
                            let literal = &prev_rem[..index_of_percent + 1];
                            Item {
                                literal,
                                format: None,
                            }
                        }
                        b's' => Item {
                            literal,
                            format: Some(PercentSFormat::Str),
                        },
                        b'r' => Item {
                            literal,
                            format: Some(PercentSFormat::Repr),
                        },
                        b'd' => Item {
                            literal,
                            format: Some(PercentSFormat::Dec),
                        },
                        b'o' => Item {
                            literal,
                            format: Some(PercentSFormat::Oct),
                        },
                        b'x' => Item {
                            literal,
                            format: Some(PercentSFormat::Hex),
                        },
                        b'X' => Item {
                            literal,
                            format: Some(PercentSFormat::HexUpper),
                        },
                        b'e' => Item {
                            literal,
                            format: Some(PercentSFormat::Exp),
                        },
                        b'E' => Item {
                            literal,
                            format: Some(PercentSFormat::ExpUpper),
                        },
                        b'f' | b'F' => Item {
                            literal,
                            format: Some(PercentSFormat::Float),
                        },
                        b'g' => Item {
                            literal,
                            format: Some(PercentSFormat::FloatCompact),
                        },
                        b'G' => Item {
                            literal,
                            format: Some(PercentSFormat::FloatCompactUpper),
                        },
                        _ => {
                            // Note we need to find the second character, not the second byte.
                            let Some(c) = rem.chars().nth(1) else {
                                return Some(Err(StringInterpolationError::ExpectingFormatCharacter.into()));
                            };
                            return Some(Err(
                                StringInterpolationError::UnsupportedFormatCharacter(c).into(),
                            ));
                        }
                    };
                    // We reach here only if format character is ASCII,
                    // so we can safely skip 2 bytes.
                    self.rem = &rem[2..];
                    Some(Ok(res))
                }
            }
        } else {
            if self.rem.is_empty() {
                None
            } else {
                let literal = self.rem;
                self.rem = "";
                Some(Ok(Item {
                    literal,
                    format: None,
                }))
            }
        }
    }
}

pub(crate) fn percent(format: &str, value: Value) -> anyhow::Result<String> {
    // NOTE(nga): use could reuse `Evaluator::string_pool` here, but
    //   * we don't have access to `Evaluator` in `StarlarkValue::percent`
    //   * after single %s made intrinsic, this code is not that hot now

    // random guess as a baseline capacity
    let mut res: String = String::with_capacity(format.len() + 20);

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
    for item in (PercentFormatParser { rem: format }) {
        let item = item?;
        res.push_str(item.literal);
        match item.format {
            None => {}
            Some(PercentSFormat::Str) => {
                let arg = next_value()?;
                match arg.unpack_str() {
                    None => arg.collect_repr(&mut res),
                    Some(s) => res.push_str(s),
                }
            }
            Some(PercentSFormat::Repr) => next_value()?.collect_repr(&mut res),
            Some(PercentSFormat::Dec) => {
                let value = next_value()?;
                if let Some(num::Num::Float(v)) = value.unpack_num() {
                    match num::Num::Float(v.trunc()).as_int() {
                        None => {
                            return ValueError::unsupported(&float::StarlarkFloat(v), "%d");
                        }
                        Some(v) => write!(res, "{}", v).unwrap(),
                    }
                } else {
                    write!(res, "{}", value.to_int()?).unwrap()
                }
            }
            Some(PercentSFormat::Oct) => {
                let v = next_value()?.to_int()?;
                write!(
                    res,
                    "{}{:o}",
                    if v < 0 { "-" } else { "" },
                    v.wrapping_abs() as u64
                )
                .unwrap();
            }
            Some(PercentSFormat::Hex) => {
                let v = next_value()?.to_int()?;
                write!(
                    res,
                    "{}{:x}",
                    if v < 0 { "-" } else { "" },
                    v.wrapping_abs() as u64
                )
                .unwrap();
            }
            Some(PercentSFormat::HexUpper) => {
                let v = next_value()?.to_int()?;
                write!(
                    res,
                    "{}{:X}",
                    if v < 0 { "-" } else { "" },
                    v.wrapping_abs() as u64
                )
                .unwrap();
            }
            Some(PercentSFormat::Exp) => {
                let v = Num::unpack_param(next_value()?)?.as_float();
                float::write_scientific(&mut res, v, 'e', false).unwrap()
            }
            Some(PercentSFormat::ExpUpper) => {
                let v = Num::unpack_param(next_value()?)?.as_float();
                float::write_scientific(&mut res, v, 'E', false).unwrap()
            }
            Some(PercentSFormat::Float) => {
                let v = Num::unpack_param(next_value()?)?.as_float();
                float::write_decimal(&mut res, v).unwrap()
            }
            Some(PercentSFormat::FloatCompact) => {
                let v = Num::unpack_param(next_value()?)?.as_float();
                float::write_compact(&mut res, v, 'e').unwrap()
            }
            Some(PercentSFormat::FloatCompactUpper) => {
                let v = Num::unpack_param(next_value()?)?.as_float();
                float::write_compact(&mut res, v, 'E').unwrap()
            }
        }
    }
    if values.next().is_some() {
        Err(StringInterpolationError::TooManyParameters.into())
    } else {
        Ok(res)
    }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::assert;

    #[test]
    fn test_incomplete_format() {
        assert::fail("'%' % ()", "Incomplete format");
    }

    #[test]
    fn test_unsupported_format_character() {
        assert::fail("'xx%qxx' % (1,)", "Unsupported format character: 'q'");
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
