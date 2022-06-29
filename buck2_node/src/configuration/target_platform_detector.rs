/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The target platform detector spec is used to define a mapping of `package-prefix` to `target platform target`.
//! When performing "target platform resolution", if a target doesn't specify a `default_target_platform`, buck
//! will fallback to the target platform detector to determine the default target platform for that target.
//!
//! This is encoded in the buckconfig value `parser.target_platform_detector_spec` and has the format:
//!   `kind1:matcher1->platform1 kind2:matcher2->platform2 ...`
//!
//! The only supported _kind_ currently is "target". The _platform_ must be a target literal that defines a configuration (for example,
//! a `platform()` target).
//!
//! The "target" kind only supports a _matcher_ that is a recursive target pattern (ex. `cell//lib/...`).
//!
//! An example supported mapping is: `target://foo/...->//:tgt target:cell//bar/...->//:tgt2 target:cell//bar/foo/...->//:tgt`.
//! This would map `//foo:x` to platform `//:tgt`, `cell//bar:x` to `//:tgt2` and `cell//bar/foo:x` also to `//:tgt2` (the mapping
//! for `cell//bar/foo/...` has no effect because buck will pick the first matching spec).

use anyhow::Context;
use buck2_core::cells::paths::CellPath;
use buck2_core::cells::CellAliasResolver;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::TargetPattern;
use buck2_core::target::TargetLabel;
use thiserror::Error;

#[derive(Debug, Error)]
enum DetectorSpecParseError {
    #[error(
        "`target:` platform detector only supports a recursive pattern matcher (like `cell//package/...`) but got `{0}`"
    )]
    TargetKindRequiresRecursivePattern(String),
    #[error(
        "Error parsing target platform detector spec, expected format `<kind>:<matcher> <kind>:<matcher>`, got `{0}`"
    )]
    UnrecognizedFormat(String),
    #[error(
        "Unsupported target platform detector kind, supported kinds are <\"target\">, got `{0}`"
    )]
    UnsupportedKind(String),
}

#[derive(Debug, Eq, PartialEq)]
pub struct TargetPlatformDetector {
    detectors: Vec<(CellPath, TargetLabel)>,
}

impl TargetPlatformDetector {
    pub fn empty() -> Self {
        Self {
            detectors: Vec::new(),
        }
    }
    pub fn parse_spec(cell_alias_resolver: &CellAliasResolver, spec: &str) -> anyhow::Result<Self> {
        let mut detectors = Vec::new();
        for value in spec.split_whitespace() {
            match value.split_once(':') {
                Some(("target", value)) => match value.split_once("->") {
                    Some((matcher, target)) => {
                        let matcher_package = match ParsedPattern::<TargetPattern>::parse_precise(
                            cell_alias_resolver,
                            matcher,
                        )? {
                            buck2_core::pattern::ParsedPattern::Recursive(root) => root,
                            _ => {
                                return Err(
                                    DetectorSpecParseError::TargetKindRequiresRecursivePattern(
                                        matcher.to_owned(),
                                    )
                                    .into(),
                                );
                            }
                        };
                        let target = ParsedPattern::<TargetPattern>::parse_precise(
                            cell_alias_resolver,
                            target,
                        )
                        .and_then(|x| x.as_target_label(target))
                        .context("when parsing target platform detector spec")?;
                        detectors.push((matcher_package, target))
                    }
                    None => {
                        return Err(
                            DetectorSpecParseError::UnrecognizedFormat(spec.to_owned()).into()
                        );
                    }
                },
                Some((kind, _)) => {
                    return Err(DetectorSpecParseError::UnsupportedKind(kind.to_owned()).into());
                }
                None => {
                    return Err(DetectorSpecParseError::UnrecognizedFormat(spec.to_owned()).into());
                }
            }
        }
        Ok(TargetPlatformDetector { detectors })
    }

    pub fn detect(&self, target: &TargetLabel) -> Option<&TargetLabel> {
        for (root, platform) in &self.detectors {
            if target.pkg().cell_name() == root.cell()
                && target.pkg().cell_relative_path().starts_with(root.path())
            {
                return Some(platform);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_core::cells::CellAlias;
    use buck2_core::cells::CellName;
    use buck2_core::target::testing::TargetLabelExt;
    use maplit::hashmap;

    use super::*;

    #[test]
    fn test_parse_errors() -> anyhow::Result<()> {
        let cell_alias_resolver = CellAliasResolver::new(Arc::new(hashmap! {
            CellAlias::new("".to_owned()) => CellName::unchecked_new("".to_owned()),
            CellAlias::new("alias1".to_owned()) => CellName::unchecked_new("cell1".to_owned()),
        }))?;

        let check_fails = |spec| {
            if TargetPlatformDetector::parse_spec(&cell_alias_resolver, spec).is_ok() {
                panic!(
                    "Expected spec `{}` to fail parsing, but it succeeded.",
                    spec
                )
            }
        };

        let check_good = |spec| {
            TargetPlatformDetector::parse_spec(&cell_alias_resolver, spec)
                .unwrap_or_else(|_| panic!("Expected parsing `{}` to succeed.", spec))
        };

        check_good("target://...->//:tgt");
        check_good("target://...->//:tgt target:alias1//...->alias1//:tgt");

        // all bad
        check_fails("xxxx");
        // bad kind
        check_fails("xyz://...->//:tgt");
        // missing pattern to match
        check_fails("target:->//:tgt");
        // match pattern not recursive
        check_fails("target://some:->//:tgt");
        // bad cell alias
        check_fails("target://...->missing//:tgt");
        // target not a literal
        check_fails("target://...->//...");
        // first spec good, second spec bad
        check_fails("target://...->//:tgt xyz");

        Ok(())
    }

    #[test]
    fn test_detect() -> anyhow::Result<()> {
        let cell_alias_resolver = CellAliasResolver::new(Arc::new(hashmap! {
            CellAlias::new("".to_owned()) => CellName::unchecked_new("".to_owned()),
            CellAlias::new("alias1".to_owned()) => CellName::unchecked_new("cell1".to_owned()),
        }))?;

        let detector = TargetPlatformDetector::parse_spec(
            &cell_alias_resolver,
            "target://lib/...->//:p1 target://lib2/foo/...->//:p2 target:alias1//map/...->alias1//:alias",
        )?;

        let p1 = TargetLabel::testing_parse("//:p1");
        let p2 = TargetLabel::testing_parse("//:p2");
        let alias = TargetLabel::testing_parse("cell1//:alias");

        assert_eq!(
            detector.detect(&TargetLabel::testing_parse("//lib/bar:xyz")),
            Some(&p1)
        );
        assert_eq!(
            detector.detect(&TargetLabel::testing_parse("//lib:xyz")),
            Some(&p1)
        );

        assert_eq!(
            detector.detect(&TargetLabel::testing_parse("//lib2/foo:xyz")),
            Some(&p2)
        );
        assert_eq!(
            detector.detect(&TargetLabel::testing_parse("//lib2:foo")),
            None
        );

        assert_eq!(detector.detect(&TargetLabel::testing_parse("//:lib")), None);

        assert_eq!(
            detector.detect(&TargetLabel::testing_parse("cell1//:xyz")),
            None
        );
        assert_eq!(
            detector.detect(&TargetLabel::testing_parse("cell1//map:xyz")),
            Some(&alias)
        );

        Ok(())
    }
}
