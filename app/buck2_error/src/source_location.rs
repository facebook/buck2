/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

/// Converts a file path returned by `file!` or `Location::file()` to a value suitable for use as a
/// `source_location`.
///
/// The extra parameter, if present, will be appended to the end of the path.
///
/// May return `None` if the path is not in `buck2/app`.
pub fn from_file(path: &str, extra: Option<&str>) -> Option<String> {
    // The path is passed in as a host path, not a target path. So we need to manually standardize
    // the path separators
    let path: String = path
        .chars()
        .map(|c| if c == '\\' { '/' } else { c })
        .collect();
    // `buck2_error` should only be used within `buck2/app`, giving us a nice way to make sure we
    // strip any leading parts of the path we don't want.
    let (_, path) = path.split_once("app/")?;

    let extra_delimiter = if extra.is_some() { "::" } else { "" };

    Some(format!(
        "{}{}{}",
        path,
        extra_delimiter,
        extra.unwrap_or("")
    ))
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
            from_file(file!(), None).as_deref(),
            Some("buck2_error/src/source_location.rs"),
        );

        assert_eq!(
            from_file(file!(), Some("Type::Variant")).as_deref(),
            Some("buck2_error/src/source_location.rs::Type::Variant"),
        );
    }

    #[test]
    fn test_windows_path() {
        assert_eq!(
            from_file(
                r"C:\whatever\repo\buck2\app\buck2_error\src\source_location.rs",
                None
            )
            .as_deref(),
            Some("buck2_error/src/source_location.rs"),
        );
    }

    #[test]
    fn test_via_error_macro() {
        let err_msg = "some error message";
        let err: crate::Error = crate::buck2_error!([], "some error message");
        assert_eq!(err.to_string(), err_msg);
        assert!(
            err.source_location()
                .unwrap()
                .contains("buck2_error/src/source_location.rs")
        );
    }

    #[test]
    fn test_via_error_new() {
        let err_msg = "Test Error";
        let err: crate::Error = crate::Error::new(
            err_msg.to_owned(),
            Some("test_source_location".to_owned()),
            None,
        );
        assert_eq!(err.to_string(), err_msg);
        assert_eq!(err.source_location(), Some("test_source_location"));
    }

    #[test]
    fn test_via_anyhow_from() {
        let err: anyhow::Error = anyhow::Error::new(MyError);
        let err: crate::Error = from_any_with_tag(err, crate::ErrorTag::Input);
        assert_eq!(
            err.source_location(),
            Some("buck2_error/src/source_location.rs"),
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
            e.source_location(),
            Some("buck2_error/src/source_location.rs"),
        );
    }

    #[test]
    fn test_via_context() {
        use anyhow::Context;

        let e: anyhow::Error = Err::<(), _>(MyError).context("foo").unwrap_err();
        let e: crate::Error = from_any_with_tag(e, crate::ErrorTag::Input);
        assert_eq!(
            e.source_location(),
            Some("buck2_error/src/source_location.rs"),
        );

        let e: buck2_error::Error = from_any_with_tag(MyError, crate::ErrorTag::Input);
        let e: anyhow::Error = e.into();
        let e: crate::Error = from_any_with_tag(e, crate::ErrorTag::Input);
        assert_eq!(
            e.source_location(),
            Some("buck2_error/src/source_location.rs"),
        );
    }

    #[test]
    fn test_via_derive() {
        use crate::derive_tests::Error1;
        use crate::derive_tests::Error3;

        let e: crate::Error = Error1.into();
        assert_eq!(
            e.source_location(),
            Some("buck2_error/src/derive_tests.rs::Error1")
        );

        let e: crate::Error = Error3::VariantB.into();
        assert_eq!(
            e.source_location(),
            Some("buck2_error/src/derive_tests.rs::Error3::VariantB")
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
        assert!(e.source_location().is_some());
        assert!(
            e.source_location()
                .unwrap()
                .contains("buck2_error/src/source_location.rs")
        );
    }
}
