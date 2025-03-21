/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;

#[derive(Allocative, Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    /// The path to the file that this error occurred in.
    path: String,
    /// The type and possibly variant - name, formatted as either `Type` or `Type::Variant`.
    type_name: Option<String>,
    source_line: Option<u32>,
}

impl SourceLocation {
    /// Converts a file path returned by `file!` or `Location::file()` to a value suitable for use as a
    /// `source_location`.
    pub fn new(path: &str) -> Self {
        // The path is passed in as a host path, not a target path. So we need to manually normalize
        // the path separators
        let path: String = path
            .chars()
            .map(|c| if c == '\\' { '/' } else { c })
            .collect();
        // `buck2_error` should only be used within `buck2/app`, giving us a nice way to make sure we
        // strip any leading parts of the path we don't want.
        // Splitting on app/ instead of buck2/app/ to avoid breaking OSS tests where root path is not buck2.
        let path = if let Some((_, path)) = path.split_once("app/") {
            path.to_owned()
        } else {
            // Shouldn't happen, but we still want to see the path if it does.
            format!("external:{}", path)
        };

        Self {
            path,
            type_name: None,
            source_line: None,
        }
    }

    pub(crate) fn with_source_line(mut self, line: u32) -> Self {
        self.source_line = Some(line);
        self
    }

    pub fn with_type_name(mut self, type_name: &str) -> Self {
        self.type_name = Some(type_name.to_owned());
        self
    }

    pub fn type_name(&self) -> Option<&str> {
        self.type_name.as_deref()
    }
}

impl std::fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path)?;
        if let Some(type_name) = &self.type_name {
            write!(f, "::{}", type_name)?;
        }
        if let Some(source_line) = self.source_line {
            write!(f, "::{}", source_line)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate as buck2_error;
    use crate::conversion::from_any_with_tag;
    use crate::conversion_test::MyError;

    #[test]
    fn test_this_file() {
        assert_eq!(
            SourceLocation::new(file!()).to_string(),
            "buck2_error/src/source_location.rs",
        );

        assert_eq!(
            SourceLocation::new(file!())
                .with_type_name("Type::Variant")
                .to_string(),
            "buck2_error/src/source_location.rs::Type::Variant",
        );
    }

    #[test]
    fn test_windows_path() {
        assert_eq!(
            SourceLocation::new(r"C:\whatever\repo\buck2\app\buck2_error\src\source_location.rs")
                .to_string(),
            "buck2_error/src/source_location.rs",
        );
    }

    #[test]
    fn test_via_error_macro() {
        let err_msg = "some error message";
        let err: crate::Error = crate::buck2_error!(crate::ErrorTag::Input, "some error message");
        assert_eq!(err.to_string(), err_msg);
        assert!(
            err.source_location()
                .to_string()
                .contains("buck2_error/src/source_location.rs")
        );
    }

    #[test]
    fn test_via_error_new() {
        let err_msg = "Test Error";
        let err: crate::Error = crate::Error::new(
            err_msg.to_owned(),
            crate::ErrorTag::Input,
            SourceLocation::new("test_source_location"),
            None,
        );
        assert_eq!(err.to_string(), err_msg);
        assert_eq!(
            err.source_location().to_string(),
            "external:test_source_location"
        );
    }

    #[test]
    fn test_via_anyhow_from() {
        let err: anyhow::Error = anyhow::Error::new(MyError);
        let err: crate::Error = from_any_with_tag(err, crate::ErrorTag::Input);
        assert_eq!(
            err.source_location().to_string(),
            "buck2_error/src/source_location.rs",
        );
    }

    #[test]
    fn test_via_try() {
        fn foo() -> crate::Result<()> {
            Err::<(), _>(MyError)?;
            Ok(())
        }

        let e = foo().unwrap_err();
        assert_eq!(
            e.source_location().to_string(),
            "buck2_error/src/source_location.rs",
        );
    }

    #[test]
    fn test_via_context() {
        use anyhow::Context;

        let e: anyhow::Error = Err::<(), _>(MyError).context("foo").unwrap_err();
        let e: crate::Error = from_any_with_tag(e, crate::ErrorTag::Input);
        assert_eq!(
            e.source_location().to_string(),
            "buck2_error/src/source_location.rs",
        );

        let e: buck2_error::Error = from_any_with_tag(MyError, crate::ErrorTag::Input);
        let e: anyhow::Error = e.into();
        let e: crate::Error = from_any_with_tag(e, crate::ErrorTag::Input);
        assert_eq!(
            e.source_location().to_string(),
            "buck2_error/src/source_location.rs",
        );
    }

    #[test]
    fn test_via_derive() {
        use crate::derive_tests::Error1;
        use crate::derive_tests::Error3;

        let e: crate::Error = Error1.into();
        assert_eq!(
            e.source_location().to_string(),
            "buck2_error/src/derive_tests.rs::Error1",
        );

        let e: crate::Error = Error3::VariantB.into();
        assert_eq!(
            e.source_location().to_string(),
            "buck2_error/src/derive_tests.rs::Error3::VariantB",
        );
    }

    #[test]
    fn test_via_implicit() {
        fn foo() -> Result<(), String> {
            Err("Some string error".to_owned())
        }

        fn bar() -> Result<(), crate::Error> {
            Ok(foo()?)
        }

        let e = bar().unwrap_err();
        assert!(
            e.source_location()
                .to_string()
                .contains("buck2_error/src/source_location.rs")
        );
    }
}
