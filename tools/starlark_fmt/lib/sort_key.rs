/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

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

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(json: &str) -> Result<SortKey, serde_json::Error> {
        serde_json::from_str(json)
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
}
