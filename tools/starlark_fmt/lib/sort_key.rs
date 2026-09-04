/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Cow;

use anyhow::Context as _;
use ruff_python_ast::Expr;
use serde::Deserialize;
use vec1::Vec1;

#[derive(Clone, Debug, Eq, PartialEq, Deserialize)]
#[serde(untagged)]
pub(crate) enum SortKey {
    Named(NamedSortKey),
    FirstOf(FirstOf),
    TupleItem(TupleItem),
    CallKeyword(CallKeyword),
}

#[derive(Clone, Debug, Eq, PartialEq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum NamedSortKey {
    String,
    CallName,
}

#[derive(Clone, Debug, Eq, PartialEq, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct FirstOf {
    pub(crate) first_of: Vec1<SortKey>,
}

#[derive(Clone, Debug, Eq, PartialEq, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct TupleItem {
    pub(crate) tuple_item: usize,
}

#[derive(Clone, Debug, Eq, PartialEq, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct CallKeyword {
    pub(crate) call_keyword: String,
}

impl SortKey {
    pub(crate) fn extract<'a>(&self, expr: &'a Expr) -> anyhow::Result<Option<Cow<'a, str>>> {
        match self {
            Self::Named(NamedSortKey::String) => match expr {
                Expr::StringLiteral(string) => Ok(Some(Cow::Borrowed(string.value.to_str()))),
                _ => Ok(None),
            },
            Self::Named(NamedSortKey::CallName) => match expr {
                Expr::Call(call) => call_name(&call.func)
                    .map(|name| Some(Cow::Owned(name)))
                    .context("call has an unsupported callee"),
                _ => Ok(None),
            },
            Self::FirstOf(first_of) => {
                for key in &first_of.first_of {
                    if let Some(value) = key.extract(expr)? {
                        return Ok(Some(value));
                    }
                }
                Ok(None)
            }
            Self::TupleItem(tuple_item) => match expr {
                Expr::Tuple(tuple) => {
                    let item = tuple.elts.get(tuple_item.tuple_item).with_context(|| {
                        format!("tuple has no item at index {}", tuple_item.tuple_item)
                    })?;
                    match item {
                        Expr::StringLiteral(string) => {
                            Ok(Some(Cow::Borrowed(string.value.to_str())))
                        }
                        _ => anyhow::bail!(
                            "tuple item at index {} is not a string literal",
                            tuple_item.tuple_item
                        ),
                    }
                }
                _ => Ok(None),
            },
            Self::CallKeyword(call_keyword) => match expr {
                Expr::Call(call) => {
                    let Some(keyword) = call.arguments.keywords.iter().find(|keyword| {
                        keyword
                            .arg
                            .as_ref()
                            .is_some_and(|arg| arg.as_str() == call_keyword.call_keyword)
                    }) else {
                        return Ok(None);
                    };
                    match &keyword.value {
                        Expr::StringLiteral(string) => {
                            Ok(Some(Cow::Borrowed(string.value.to_str())))
                        }
                        _ => anyhow::bail!(
                            "call keyword `{}` is not a string literal",
                            call_keyword.call_keyword
                        ),
                    }
                }
                _ => Ok(None),
            },
        }
    }
}

pub(crate) fn call_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Name(name) => Some(name.id.to_string()),
        Expr::Attribute(attribute) => {
            let mut name = call_name(&attribute.value)?;
            name.push('.');
            name.push_str(attribute.attr.as_str());
            Some(name)
        }
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use ruff_python_ast::Stmt;

    use super::*;
    use crate::autofixes::parsed_module::ParsedModule;

    fn parse(json: &str) -> Result<SortKey, serde_json::Error> {
        serde_json::from_str(json)
    }

    fn extract(json: &str, expression: &str) -> anyhow::Result<Option<String>> {
        let source = format!("value = {expression}\n");
        let module = ParsedModule::parse(Cow::Owned(source))?;
        let Stmt::Assign(assign) = &module.stmts()[0] else {
            panic!("test input should parse as an assignment");
        };
        parse(json)
            .expect("sort key should parse")
            .extract(&assign.value)
            .map(|value| value.map(Cow::into_owned))
    }

    #[test]
    fn test_named_sort_keys() {
        assert_eq!(
            parse(r#""string""#).unwrap(),
            SortKey::Named(NamedSortKey::String)
        );
        assert_eq!(
            parse(r#""call_name""#).unwrap(),
            SortKey::Named(NamedSortKey::CallName)
        );
    }

    #[test]
    fn test_structural_sort_keys() {
        assert_eq!(
            parse(r#"{"tuple_item": 2}"#).unwrap(),
            SortKey::TupleItem(TupleItem { tuple_item: 2 })
        );
        assert_eq!(
            parse(r#"{"call_keyword": "name"}"#).unwrap(),
            SortKey::CallKeyword(CallKeyword {
                call_keyword: "name".to_owned(),
            })
        );
    }

    #[test]
    fn test_nested_first_of() {
        assert_eq!(
            parse(r#"{"first_of": ["string", {"tuple_item": 0}]}"#).unwrap(),
            SortKey::FirstOf(FirstOf {
                first_of: vec1::vec1![
                    SortKey::Named(NamedSortKey::String),
                    SortKey::TupleItem(TupleItem { tuple_item: 0 }),
                ],
            })
        );
    }

    #[test]
    fn test_rejects_invalid_sort_keys_during_deserialization() {
        for json in [
            r#"{"first_of": []}"#,
            r#"{"unknown": true}"#,
            r#"{"tuple_item": 0, "extra": true}"#,
            r#"{}"#,
        ] {
            assert!(parse(json).is_err(), "unexpectedly accepted {json}");
        }
    }

    #[test]
    fn test_extracts_structural_sort_keys() {
        assert_eq!(
            extract(r#""string""#, r#""value""#).unwrap(),
            Some("value".to_owned())
        );
        assert_eq!(
            extract(r#"{"tuple_item": 1}"#, r#"("ignored", "tuple")"#).unwrap(),
            Some("tuple".to_owned())
        );
        assert_eq!(
            extract(
                r#"{"call_keyword": "name"}"#,
                r#"factory(name = "keyword")"#
            )
            .unwrap(),
            Some("keyword".to_owned())
        );
        assert_eq!(
            extract(r#""call_name""#, "module.factory()").unwrap(),
            Some("module.factory".to_owned())
        );
    }

    #[test]
    fn test_first_of_uses_the_first_applicable_key() {
        assert_eq!(
            extract(
                r#"{"first_of": ["string", {"call_keyword": "name"}, "call_name"]}"#,
                r#"factory(name = "explicit")"#,
            )
            .unwrap(),
            Some("explicit".to_owned())
        );
    }

    #[test]
    fn test_first_of_propagates_candidate_errors() {
        // Intentionally strict: an invalid structural value in an earlier
        // candidate (e.g. non-string call keyword) surfaces as an error
        // instead of silently falling back. Inline directives rely on this
        // fail-closed behavior; configured keys get fail-open handling at
        // the caller, which skips the list on Err.
        assert!(
            extract(
                r#"{"first_of": [{"call_keyword": "name"}, "call_name"]}"#,
                r#"factory(name = dynamic)"#,
            )
            .is_err()
        );
    }

    #[test]
    fn test_extract_reports_invalid_structural_values() {
        assert!(extract(r#"{"tuple_item": 1}"#, r#"("only",)"#).is_err());
        assert!(extract(r#"{"call_keyword": "name"}"#, "factory(name = dynamic)").is_err());
    }
}
