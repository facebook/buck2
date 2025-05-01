/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/// Check if any of the values in this literal or attribute matches the predicate.
pub trait AnyMatches {
    fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool>;
}

impl AnyMatches for serde_json::Value {
    fn any_matches(
        &self,
        filter: &dyn Fn(&str) -> buck2_error::Result<bool>,
    ) -> buck2_error::Result<bool> {
        match self {
            Self::Null => {}
            Self::Bool(..) => {}
            Self::Number(..) => {}
            Self::String(v) => {
                if filter(v)? {
                    return Ok(true);
                }
            }
            Self::Array(vals) => {
                for v in vals {
                    if v.any_matches(filter)? {
                        return Ok(true);
                    }
                }
            }
            Self::Object(vals) => {
                for (k, v) in vals {
                    if filter(k)? {
                        return Ok(true);
                    }

                    if v.any_matches(filter)? {
                        return Ok(true);
                    }
                }
            }
        }

        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_serde_json_any_matches() {
        let v = serde_json::json!({
            "number": 123,
            "list": ["list1", "list2"],
            "object": {"key": "value"}
        });

        assert!(v.any_matches(&|s| Ok(s == "key")).unwrap());
        assert!(v.any_matches(&|s| Ok(s == "value")).unwrap());
        assert!(v.any_matches(&|s| Ok(s == "list1")).unwrap());
        assert!(!v.any_matches(&|_s| Ok(false)).unwrap());
    }
}
