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

use allocative::Allocative;
use dupe::Dupe;
use once_cell::sync::Lazy;
use regex::Regex;

/// A category, representing a family of actions.
#[derive(Clone, Debug, PartialEq, Eq, Hash, Allocative, derive_more::Display)]
pub struct Category(String);

#[derive(Debug, Clone, Copy, Dupe, PartialEq, Eq, Hash, derive_more::Display)]
pub struct CategoryRef<'a>(&'a str);

impl Category {
    pub fn new(s: String) -> buck2_error::Result<Self> {
        CategoryRef::new(&s)?;
        Ok(Category(s))
    }

    /// Returns a string representation of this category.
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn as_ref(&self) -> CategoryRef<'_> {
        CategoryRef(self.0.as_str())
    }
}

impl<'a> CategoryRef<'a> {
    pub fn unchecked_new(s: &'static str) -> Self {
        CategoryRef(s)
    }

    pub fn as_str(self) -> &'a str {
        self.0
    }

    pub fn new(s: &'a str) -> buck2_error::Result<Self> {
        static CATEGORY_REGEX: Lazy<Regex> =
            Lazy::new(|| Regex::new("^[a-z][a-z0-9]*(_[a-z][a-z0-9]*)*$").unwrap());

        if !CATEGORY_REGEX.is_match(s) {
            Err(CategoryParseError::NotSnakeCase(s.to_owned()).into())
        } else {
            Ok(CategoryRef(s))
        }
    }

    pub fn to_owned(self) -> Category {
        Category(self.0.to_owned())
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(input)]
enum CategoryParseError {
    #[error(
        "Invalid category `{0}`. Must be a snake_cased identifier consisting of lowercase alphanumeric characters, e.g. `cxx_compile`. Each section of the snake_cased identifier must begin with a lowercase letter (not a number)."
    )]
    NotSnakeCase(String),
}

#[cfg(test)]
mod tests {
    use super::CategoryRef;

    #[test]
    fn valid_categories() {
        CategoryRef::new("valid_category").unwrap();
        CategoryRef::new("valid_category_with_numbers10").unwrap();
        CategoryRef::new("singleword").unwrap();
    }

    #[test]
    fn invalid_categories() {
        CategoryRef::new("_leading_underscore").unwrap_err();
        CategoryRef::new("NotSnakeCase").unwrap_err();
        CategoryRef::new("Not_Snake_Case").unwrap_err();
        CategoryRef::new("contains_4_number").unwrap_err();
        CategoryRef::new("trailing_underscore_").unwrap_err();
    }
}
