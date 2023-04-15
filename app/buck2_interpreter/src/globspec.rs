/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashSet;
use std::fmt;
use std::fmt::Debug;

use anyhow::Context;
use buck2_common::package_listing::file_listing::PackageFileListing;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_core::soft_error;
use derivative::Derivative;

#[derive(Debug, thiserror::Error)]
enum GlobError {
    #[error(
        "Exact pattern must be valid forward relative path: `{0}` \
        (this pattern cannot match anything)"
    )]
    ExactIncludeMustBeForwardRelativePath(String),
    #[error("Double slash in pattern: `{0}` (this pattern cannot match anything)")]
    DoubleSlash(String),
    #[error("Pattern must not start with `/`: `{0}` (this pattern cannot match anything)")]
    LeadingSlash(String),
    #[error("Pattern must not end with `/`: `{0}` (this pattern cannot match anything)")]
    TrailingSlash(String),
    #[error("Pattern must not contain `..`: `{0}` (this pattern cannot match anything)")]
    DotDot(String),
}

/// The default Debug for Pattern is horribly verbose with lots of internal
/// details we don't care about, so create `GlobPattern` so we can have a
/// readable `Debug`.
struct GlobPattern(glob::Pattern);

impl Debug for GlobPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("Pattern").field(&self.0.as_str()).finish()
    }
}

impl GlobPattern {
    fn new(pattern: &str) -> anyhow::Result<GlobPattern> {
        let parsed_pattern = glob::Pattern::new(pattern)
            .with_context(|| format!("Error creating globspec for `{}`", pattern))?;
        if pattern.contains("//") {
            // Please write a test when converting this error to hard error.
            soft_error!(
                "glob_double_slash",
                GlobError::DoubleSlash(pattern.to_owned()).into()
            )?;
        }
        if pattern.starts_with('/') {
            return Err(GlobError::LeadingSlash(pattern.to_owned()).into());
        }
        if pattern.ends_with('/') {
            // Please write a test when converting this error to hard error.
            soft_error!(
                "glob_trailing_slash",
                GlobError::TrailingSlash(pattern.to_owned()).into()
            )?;
        }
        // TODO(nga): also ban single dot.
        if pattern.split('/').any(|p| p == "..") {
            return Err(GlobError::DotDot(pattern.to_owned()).into());
        }
        Ok(GlobPattern(parsed_pattern))
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
                glob_patterns.push(GlobPattern::new(pattern)?);
            } else {
                // TODO(nga): pattern `*/[bc]` is parsed as glob pattern,
                //   but `a/[bc]` is parsed as exact match?
                //   Does not look right.

                if ForwardRelativePath::new(&pattern).is_err() {
                    return Err(GlobError::ExactIncludeMustBeForwardRelativePath(
                        pattern.to_owned(),
                    )
                    .into());
                }
                exact_matches.insert(pattern.to_owned());
            }
        }
        for pattern in excludes {
            let pattern = pattern.as_ref();
            glob_excludes.push(GlobPattern::new(pattern)?);
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
            // FIXME: We should have case sensitive globs
            case_sensitive: false,
        };
        let include_matches = self.exact_matches.contains(path)
            || self
                .patterns
                .iter()
                .any(|p| p.0.matches_with(path, options));
        include_matches
            && !self
                .excludes
                .iter()
                .any(|p| p.0.matches_with(path, options))
    }

    pub fn resolve_glob<'a>(
        &'a self,
        spec: &'a PackageFileListing,
    ) -> Box<dyn Iterator<Item = &'a PackageRelativePath> + 'a> {
        if spec.files().len() >= Self::BINARY_SEARCH_CUTOFF && !self.common_prefix.is_empty() {
            return Box::new(
                spec.files_with_prefix(&self.common_prefix)
                    .filter(move |v| self.matches(v.as_str()))
                    .map(|p| &**p),
            );
        }
        Box::new(spec.files().filter(move |v| self.matches(v.as_str())))
    }
}

#[cfg(test)]
mod tests {
    use buck2_core::package::package_relative_path::PackageRelativePathBuf;

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
    fn test_glob_match_case_insensitive() -> anyhow::Result<()> {
        // NOTE: We probably should change this. But for now, let's codify the current behavior
        // since that's probably something that should require a migration.

        let spec = GlobSpec::new(&["src/**/*"], &["src/excluded/**/*"])?;
        assert!(spec.matches("src/foo"));
        assert!(spec.matches("SRC/foo"));
        assert!(!spec.matches("src/EXCLUDED/bar"));

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
    fn test_resolve_glob_dot() {
        fn glob(pattern: &str, listing: &PackageFileListing) -> Vec<PackageRelativePathBuf> {
            GlobSpec::new(&[pattern], &[""; 0])
                .unwrap()
                .resolve_glob(listing)
                .map(PackageRelativePath::to_owned)
                .collect()
        }

        let listing = PackageFileListing::testing_new(&["a", ".a", "b/c", "b/.c", ".d/e", ".d/.e"]);

        // Simple difference between dot and non-dot patterns.
        assert_eq!(vec![".a"], glob(".*", &listing));
        assert_eq!(vec!["a"], glob("*", &listing));

        // Match works in subdirectories too, even if directory name starts with dot.
        assert_eq!(vec![".d/e"], glob(".d/*", &listing));
        assert_eq!(vec![".d/.e"], glob(".d/.*", &listing));

        // Star matches directory, but not dot-directory.
        assert_eq!(vec!["b/.c"], glob("*/.*", &listing));
        assert_eq!(vec!["b/c"], glob("*/*", &listing));

        // Dot-star matches dot-directory, but not directory.
        assert_eq!(vec![".d/.e"], glob(".*/.*", &listing));
        assert_eq!(vec![".d/e"], glob(".*/*", &listing));

        // Star-star matches only non-star directories and files.
        assert_eq!(vec!["a", "b/c"], glob("**", &listing));
        assert_eq!(vec![".a", "b/.c"], glob("**/.*", &listing));
    }

    #[test]
    fn test_resolve_glob_square_brackets_bug() {
        let glob = GlobSpec::new(&["*/[bc]"], &[""; 0]).unwrap();
        assert!(glob.matches("a/b"));
        let glob = GlobSpec::new(&["a/[bc]"], &[""; 0]).unwrap();
        // TODO(nga): This is a bug.
        assert!(!glob.matches("a/b"));
    }

    #[test]
    fn test_invalid_patterns() {
        fn test_invalid(pattern: &str) {
            assert!(
                GlobSpec::new(&[pattern], &[""; 0]).is_err(),
                "Pattern must not be accepted: `{pattern}`"
            );
            assert!(
                GlobSpec::new(&[""; 0], &[pattern]).is_err(),
                "Pattern must not be accepted as exclude as well: `{pattern}`"
            );
        }

        test_invalid("a**");
        test_invalid("**a");
        test_invalid(".**");
    }

    #[test]
    fn test_leading_slash_in_pattern() {
        assert!(GlobSpec::new(&["/*"], &[""; 0]).is_err());
        assert!(GlobSpec::new(&["/a"], &[""; 0]).is_err());
    }

    #[test]
    fn test_dot_dot_in_pattern() {
        assert!(GlobSpec::new(&[".."], &[""; 0]).is_err());
        assert!(GlobSpec::new(&["../*"], &[""; 0]).is_err());
        assert!(GlobSpec::new(&["a/../*"], &[""; 0]).is_err());
        assert!(GlobSpec::new(&["*/../b"], &[""; 0]).is_err());
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
