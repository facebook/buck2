/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! This module defines the API between validators and Buck2 by specifying
//! the schema for a JSON file that represents the result of a validator run.

use serde::Deserialize;
use serde_json::value::RawValue;

const CURRENT_VERSION: i32 = 1;

#[derive(Deserialize, Debug)]
struct ValidationStaticSchema<'a> {
    version: i32,
    #[serde(borrow)]
    data: &'a RawValue,
}

#[derive(Deserialize, Debug, PartialEq)]
pub(crate) enum ValidationStatus {
    #[serde(alias = "success")]
    Success,
    #[serde(alias = "failure")]
    Failure,
}

#[derive(Deserialize, Debug, PartialEq)]
pub(crate) struct ValidationResult {
    pub status: ValidationStatus,
    pub message: Option<String>,
}

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum ValidationApiError {
    #[error("Validation result should contain valid JSON.")]
    InvalidJson {
        #[source]
        error: serde_json::Error,
    },
    #[error("Incompatible version of validation result `{0}`, expected `{1}`.")]
    IncompatibleVersion(i32, i32),
    #[error("JSON content doesn't match schema.")]
    JsonNotMatchingSchema {
        #[source]
        error: serde_json::Error,
    },
}

pub(crate) fn parse_validation_result(content: &str) -> buck2_error::Result<ValidationResult> {
    let result: ValidationStaticSchema = match serde_json::from_str(content) {
        Ok(x) => x,
        Err(error) => return Err((ValidationApiError::InvalidJson { error }).into()),
    };
    if result.version != CURRENT_VERSION {
        return Err(
            ValidationApiError::IncompatibleVersion(result.version, CURRENT_VERSION).into(),
        );
    }
    let result: ValidationResult = match serde_json::from_str(result.data.get()) {
        Ok(x) => x,
        Err(error) => return Err((ValidationApiError::JsonNotMatchingSchema { error }).into()),
    };
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_broken_json() {
        let json = r#"
            {
                "version": 1
        "#;
        let error = parse_validation_result(json).expect_err("Expected parsing to fail");

        assert!(
            error
                .to_string()
                .contains("Validation result should contain valid JSON.")
        )
    }

    #[test]
    fn test_parse_wrong_schema() {
        let json = r#"
            {
                "data": {
                    "foo": 1,
                    "bar": 2
                }
            }
        "#;
        let error = parse_validation_result(json).expect_err("Expected parsing to fail");
        assert!(
            error
                .to_string()
                .contains("Validation result should contain valid JSON.")
        )
    }

    #[test]
    fn test_parse_valid_result() {
        let json = r#"
            {
                "version": 1,
                "data": {
                    "status": "failure",
                    "message": "something somewhere"
                }
            }
        "#;
        let expected = ValidationResult {
            status: ValidationStatus::Failure,
            message: Some("something somewhere".to_owned()),
        };
        assert_eq!(parse_validation_result(json).unwrap(), expected);
    }

    #[test]
    fn test_parse_invalid_version() {
        let json = r#"
            {
                "version": 2,
                "data": {
                    "foo": 1,
                    "bar": 2
                }
            }
        "#;
        let error = parse_validation_result(json).expect_err("Expected parsing to fail");
        assert!(
            error
                .to_string()
                .contains("Incompatible version of validation result")
        )
    }

    #[test]
    fn test_parse_no_matching_schema_with_valid_version() {
        let json = r#"
            {
                "version": 1,
                "data": {
                    "message": "something somewhere"
                }
            }
        "#;
        let error = parse_validation_result(json).expect_err("Expected parsing to fail");
        assert!(
            error
                .to_string()
                .contains("JSON content doesn't match schema.")
        )
    }
}
