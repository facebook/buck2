/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;

/// Serialized JSON.
/// It can be output as is to a JSON file.
pub(crate) struct QuotedJson(String);

impl QuotedJson {
    pub(crate) fn as_str(&self) -> &str {
        &self.0
    }

    pub(crate) fn quote_str(s: &str) -> QuotedJson {
        QuotedJson(serde_json::ser::to_string(s).unwrap())
    }

    pub(crate) fn quote_display(d: impl Display) -> QuotedJson {
        Self::quote_str(&d.to_string())
    }

    pub(crate) fn list(items: impl IntoIterator<Item = QuotedJson>) -> QuotedJson {
        let mut s = String::new();
        s.push('[');
        for (i, item) in items.into_iter().enumerate() {
            if i != 0 {
                s.push(',');
            }
            s.push_str(item.as_str());
        }
        s.push(']');
        QuotedJson(s)
    }

    pub(crate) fn from_serde_json_value(v: serde_json::Value) -> QuotedJson {
        QuotedJson(serde_json::ser::to_string(&v).unwrap())
    }
}

#[cfg(test)]
mod tests {
    use crate::json::QuotedJson;

    #[test]
    fn test_quote_string() {
        assert_eq!("\"\"", QuotedJson::quote_str("").as_str());
        assert_eq!("\"\\\"\"", QuotedJson::quote_str("\"").as_str());
        assert_eq!("\"\\\\\"", QuotedJson::quote_str("\\").as_str());
        assert_eq!(
            "\"a\\n\\r\\t\\b\\f\"",
            QuotedJson::quote_str("a\n\r\t\x08\x0c").as_str()
        );
    }

    #[test]
    pub fn test_list() {
        assert_eq!("[]", QuotedJson::list(Vec::new()).as_str());
        assert_eq!(
            "[\"a\"]",
            QuotedJson::list(vec![QuotedJson::quote_str("a")]).as_str()
        );
        assert_eq!(
            "[\"a\",\"b\"]",
            QuotedJson::list(vec![QuotedJson::quote_str("a"), QuotedJson::quote_str("b")]).as_str()
        );
    }
}
