/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Categories and identifiers for actions run by buck2.
//!
//! A category is a snake case identifier that identifies a family of actions that are related in some way but differ
//! in their inputs. The canonical example of this is the category `cxx_compile`; conceptually, this represents the
//! category of all actions that invoke a C++ compiler, of which there are potentially many in a single C++ rule
//! implementation.

use std::{convert::TryFrom, fmt};

use once_cell::sync::Lazy;
use regex::Regex;
use thiserror::Error;

/// A category, representing a family of actions.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Category(String);

impl Category {
    /// Returns a string representation of this category.
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl TryFrom<String> for Category {
    type Error = CategoryParseError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        static CATEGORY_REGEX: Lazy<Regex> =
            Lazy::new(|| Regex::new("^[a-z][a-z0-9]*(_[a-z][a-z0-9]*)*$").unwrap());

        if !CATEGORY_REGEX.is_match(&value) {
            Err(CategoryParseError::NotSnakeCase(value))
        } else {
            Ok(Category(value))
        }
    }
}

impl TryFrom<&str> for Category {
    type Error = CategoryParseError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Category::try_from(value.to_owned())
    }
}

impl fmt::Display for Category {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(Debug, Error)]
pub enum CategoryParseError {
    #[error(
        "Invalid category `{0}`. Must be a snake_cased identifier consisting of lowercase alphanumeric characters, e.g. `cxx_compile`. Each section of the snake_cased identifier must begin with a lowercase alphanumeric character."
    )]
    NotSnakeCase(String),
}

#[cfg(test)]
mod tests {
    use std::convert::TryFrom;

    use super::Category;

    #[test]
    pub fn valid_categories() {
        Category::try_from("valid_category").unwrap();
        Category::try_from("valid_category_with_numbers10").unwrap();
        Category::try_from("singleword").unwrap();
    }

    #[test]
    pub fn invalid_categories() {
        Category::try_from("_leading_underscore").unwrap_err();
        Category::try_from("NotSnakeCase").unwrap_err();
        Category::try_from("Not_Snake_Case").unwrap_err();
        Category::try_from("contains_4_number").unwrap_err();
        Category::try_from("trailing_underscore_").unwrap_err();
    }
}
