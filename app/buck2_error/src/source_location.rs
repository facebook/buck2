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
pub(crate) fn from_file(path: &str, extra: Option<&str>) -> Option<String> {
    // The path is passed in as a host path, not a target path. So we need to manually standardize
    // the path separators
    let path: String = path
        .chars()
        .map(|c| if c == '\\' { '/' } else { c })
        .collect();
    // `buck2_error` should only be used within `buck2/app`, giving us a nice way to make sure we
    // strip any leading parts of the path we don't want.
    let Some((_, path)) = path.split_once("app/") else {
        return None;
    };

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

    #[derive(Debug, thiserror::Error)]
    #[error("My error")]
    struct MyError;

    #[test]
    fn test_via_error_new() {
        let err: crate::Error = crate::Error::new(MyError);
        assert_eq!(
            err.source_location(),
            Some("buck2_error/src/source_location.rs"),
        );
    }
}
