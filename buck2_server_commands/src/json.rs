/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub fn quote_json_string(s: &str) -> String {
    serde_json::ser::to_string(s).unwrap()
}

#[cfg(test)]
mod tests {
    use crate::json::quote_json_string;

    #[test]
    fn test_quote_json_string() {
        assert_eq!("\"\"", quote_json_string(""));
        assert_eq!("\"\\\"\"", quote_json_string("\""));
        assert_eq!("\"\\\\\"", quote_json_string("\\"));
        assert_eq!("\"a\\n\\r\\t\\b\\f\"", quote_json_string("a\n\r\t\x08\x0c"));
    }
}
