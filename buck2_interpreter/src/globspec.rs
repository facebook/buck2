/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{
    collections::HashSet,
    fmt::{self, Debug},
};

use anyhow::Context;
use buck2_core::package::PackageRelativePath;
use derivative::Derivative;

use crate::package_listing::file_listing::PackageFileListing;

/// The default Debug for Pattern is horribly verbose with lots of internal
/// details we don't care about, so create `GlobPattern` so we can have a
/// readable `Debug`.
struct GlobPattern(glob::Pattern);

impl Debug for GlobPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Pattern").field(&self.0.as_str()).finish()
    }
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct GlobSpec {
    common_prefix: String,
    exact_matches: HashSet<String>,
    patterns: Vec<GlobPattern>,
    excludes: Vec<GlobPattern>,
}

/// Returns the no-special-characters common prefix of all patterns. Suitable for binary search.
fn longest_common_glob_prefix<P: AsRef<str>>(patterns: &[P]) -> &str {
    if patterns.is_empty() {
        return "";
    }
    let first: &str = patterns[0].as_ref();
    // Globs have some special characters, ensure they're not present.
    let mut len = first
        .find(|c: char| "*[]?".contains(c))
        .unwrap_or(first.len());
    for pattern in patterns {
        if len == 0 {
            break;
        }
        len = std::cmp::min(
            len,
            pattern
                .as_ref()
                .chars()
                .take(len)
                .zip(first.chars())
                .take_while(|&(a, b)| a == b)
                .count(),
        );
    }
    match first.char_indices().nth(len) {
        None => first,
        Some((idx, _)) => &first[..idx],
    }
}

impl GlobSpec {
    const BINARY_SEARCH_CUTOFF: usize = 100;
    pub fn new<P: AsRef<str>, Q: AsRef<str>>(
        patterns: &[P],
        excludes: &[Q],
    ) -> anyhow::Result<Self> {
        let mut glob_patterns = Vec::new();
        let mut glob_excludes = Vec::new();
        let mut exact_matches = HashSet::new();
        for pattern in patterns {
            let pattern = pattern.as_ref();
            if pattern.contains('*') {
                glob_patterns
                    .push(GlobPattern(glob::Pattern::new(pattern).with_context(
                        || format!("when creating globspec for `{}`", pattern),
                    )?));
            } else {
                exact_matches.insert(pattern.to_owned());
            }
        }
        for pattern in excludes {
            let pattern = pattern.as_ref();
            glob_excludes
                .push(GlobPattern(glob::Pattern::new(pattern).with_context(
                    || format!("when creating globspec for `{}`", pattern),
                )?));
        }
        Ok(Self {
            common_prefix: longest_common_glob_prefix(patterns).to_owned(),
            exact_matches,
            patterns: glob_patterns,
            excludes: glob_excludes,
        })
    }

    pub fn matches(&self, path: &str) -> bool {
        let options = glob::MatchOptions {
            require_literal_separator: true,
            require_literal_leading_dot: true,
            ..glob::MatchOptions::default()
        };
        (self.exact_matches.contains(path)
            || self
                .patterns
                .iter()
                .any(|p| p.0.matches_with(path, options)))
            && !self
                .excludes
                .iter()
                .any(|p| p.0.matches_with(path, options))
    }

    pub(crate) fn resolve_glob<'a>(
        &'a self,
        spec: &'a PackageFileListing,
    ) -> Box<dyn Iterator<Item = &'a PackageRelativePath> + 'a> {
        if spec.files().len() >= Self::BINARY_SEARCH_CUTOFF && !self.common_prefix.is_empty() {
            return box spec
                .files_with_prefix(&self.common_prefix)
                .filter(move |v| self.matches(v.as_str()));
        }
        box spec.files().filter(move |v| self.matches(v.as_str()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_glob_match() -> anyhow::Result<()> {
        let spec = GlobSpec::new(
            &[
                "abc*",
                "**/*.java",
                "*/*/*.txt",
                "unglobbed",
                "excluded/unglobbed",
            ],
            &["excluded/**/*"],
        )?;

        assert!(spec.matches("abcxyz"));
        assert!(spec.matches("abcsomething"));
        assert!(spec.matches("1/2/3.txt"));
        assert!(spec.matches("dir/other/more/some.java"));
        assert!(spec.matches("unglobbed"));

        assert!(!spec.matches("xyz"));
        assert!(!spec.matches("abc/something"));
        assert!(!spec.matches("1/2.txt"));
        assert!(!spec.matches("1/2/3/4.txt"));

        assert!(!spec.matches("excluded/subdir/some.java"));
        assert!(!spec.matches("excluded/some.java"));
        assert!(!spec.matches("excluded/unglobbed"));

        Ok(())
    }

    #[test]
    fn test_resolve_glob() -> anyhow::Result<()> {
        let spec = GlobSpec::new(&["abc*", "**/*.java", "*/*/*.txt"], &["excluded/**/*"])?;

        let package_listing = PackageFileListing::testing_new(&[
            "abcxyz",
            "abcsomething",
            "1/2/3.txt",
            "dir/other/more/some.java",
            "xyz",
            "abc/something",
            "1/2.txt",
            "1/2/3/4.txt",
            "excluded/subdir/some.java",
            "excluded/some.java",
        ]);

        let matches: Vec<_> = spec.resolve_glob(&package_listing).collect();

        assert_eq!(
            vec![
                "1/2/3.txt",
                "abcsomething",
                "abcxyz",
                "dir/other/more/some.java",
            ],
            matches
        );

        Ok(())
    }

    #[test]
    fn test_glob_prefix() {
        assert_eq!("a/", longest_common_glob_prefix(&["a/1", "a/2"]));
        assert_eq!("", longest_common_glob_prefix(&["a/1", "b/1"]));
        assert_eq!("a/", longest_common_glob_prefix(&["a/**/*.h", "a/**/*.c"]));
        assert_eq!("a/", longest_common_glob_prefix(&["a/in/*.h", "a/**/*.c"]));
        // '?' is a single-character wildcard.
        assert_eq!("a/", longest_common_glob_prefix(&["a/foo", "a/?"]));
        // '[' and ']' are special character set tokens.
        assert_eq!("f/", longest_common_glob_prefix(&["f/[ab]/*", "f/[abc]/*"]));
        // These codepoints have a common prefix in bytes, but not in chars.
        assert_eq!("", longest_common_glob_prefix(&["\u{0306}", "\u{0307}"]));
    }
}
