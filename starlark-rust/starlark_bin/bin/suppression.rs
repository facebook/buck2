/*
 * Copyright 2019 The Starlark in Rust Authors.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use std::collections::HashSet;

use globset::Glob;
use globset::GlobMatcher;

#[derive(Debug, Clone)]
pub struct GlobLintSuppression {
    /// Glob pattern to match the suppression rule on.
    pattern: GlobMatcher,

    /// List of rules to be suppressed. This should be the name specified in
    /// [`starlark::analysis::Lint::short_name`].
    rules: HashSet<String>,
}

impl GlobLintSuppression {
    pub fn try_parse(input: impl AsRef<str>) -> anyhow::Result<Self> {
        let rule = input.as_ref();
        let (pattern, rules) = if let Some((lhs, rhs)) = rule.split_once(':') {
            (lhs, rhs)
        } else {
            ("*", rule)
        };

        let rules = rules.split(',').map(|s| s.trim().to_owned()).collect();

        Ok(Self {
            pattern: Glob::new(pattern)?.compile_matcher(),
            rules,
        })
    }

    pub fn is_suppressed(&self, filename: &str, rule: &str) -> bool {
        if self.pattern.is_match(filename) {
            return self.rules.contains(rule);
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parsing() {
        let suppression = GlobLintSuppression::try_parse("*.bzl:rule1,rule2").unwrap();

        assert!(suppression.is_suppressed("foo/bar.bzl", "rule1"));
        assert!(suppression.is_suppressed("foo/bar.bzl", "rule2"));
        assert!(!suppression.is_suppressed("foo/baz.star", "rule2"));
    }

    #[test]
    fn test_parsing_default() {
        let suppression = GlobLintSuppression::try_parse("rule1").unwrap();

        assert!(suppression.is_suppressed("foo/bar.bzl", "rule1"));
        assert!(!suppression.is_suppressed("foo/bar.bzl", "rule2"));
    }
}
