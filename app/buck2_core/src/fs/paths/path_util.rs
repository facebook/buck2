/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#[inline]
pub(crate) fn path_remove_prefix<'a>(path: &'a str, prefix: &str) -> Option<&'a str> {
    if prefix.is_empty() {
        Some(path)
    } else if path.starts_with(prefix) {
        if path.len() == prefix.len() {
            Some("")
        } else if path.as_bytes()[prefix.len()] == b'/' {
            Some(&path[prefix.len() + 1..])
        } else {
            None
        }
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::fs::paths::path_util::path_remove_prefix;

    #[test]
    fn test_path_remove_prefix() {
        assert_eq!(None, path_remove_prefix("foo", "bar"));
        assert_eq!(Some("bar"), path_remove_prefix("foo/bar", "foo"));
        assert_eq!(None, path_remove_prefix("foobar", "foo"));
    }
}
