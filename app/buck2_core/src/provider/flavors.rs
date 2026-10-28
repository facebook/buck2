/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Compatibility with buck1 `//pkg:target#flavor` labels.
//!
//! Buck1 used flavors for two different things: selecting a platform (`#android-arm64`,
//! `#platform010-clang`) and naming a secondary output (`#shared`, `#compilation-database`).
//! In buck2 the first is a configuration and the second is a subtarget (`//pkg:target[shared]`).
//! This module maps the flavors we know about onto those concepts and reports every use as a
//! soft error so the syntax can be removed; see T147637580.

use std::fmt;
use std::sync::OnceLock;

use regex::RegexSet;

use crate::deprecation;
use crate::is_open_source;
use crate::provider::label::NonDefaultProvidersName;
use crate::provider::label::ProviderName;
use crate::provider::label::ProvidersName;

/// Families of buck1 platform flavors, in the order of [`PLATFORM_REGEX_SET`].
///
/// A family stands in for the concrete platform in logview keys so the key space stays small.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum PlatformFamily {
    Android,
    FbcodePlatform,
    Macosx,
    Windows,
    Apple,
}

impl PlatformFamily {
    const ALL: [PlatformFamily; 5] = [
        PlatformFamily::Android,
        PlatformFamily::FbcodePlatform,
        PlatformFamily::Macosx,
        PlatformFamily::Windows,
        PlatformFamily::Apple,
    ];

    fn key(self) -> &'static str {
        match self {
            PlatformFamily::Android => "<android>",
            PlatformFamily::FbcodePlatform => "<fbcode-platform>",
            PlatformFamily::Macosx => "<macosx>",
            PlatformFamily::Windows => "<windows>",
            PlatformFamily::Apple => "<apple>",
        }
    }
}

static PLATFORM_REGEX_SET: OnceLock<RegexSet> = OnceLock::new();

fn platform_family(flavor: &str) -> Option<PlatformFamily> {
    let regex_set = PLATFORM_REGEX_SET.get_or_init(|| {
        // copied from https://fburl.com/code/sgxwq68n and modified for our needs
        RegexSet::new([
            r"^android-.*$",
            r"^(linux-.*|platform[0-9]{3}-(clang|clang-12|gcc)(-nosan)?(-split-dwarf)?)$",
            r"^macosx-(x86_64|arm64)(_minimal_xcode)?$",
            // Too broad pattern, so ignoring it
            // "^(default|host)$"
            r"^windows-x86_64$",
            //  an original data has the following pattern: FBOBJC_REGEX = "^(macosx|iphone|watch|appletv|osmeta-).*$"
            // we use modified version w/o macosx prefix already checked in the pattern above
            r"^(iphone|watch|appletv|osmeta-).*$",
        ])
        .unwrap()
    });

    regex_set
        .matches(flavor)
        .iter()
        .next()
        .map(|i| PlatformFamily::ALL[i])
}

/// What buck2 does with the non-platform part of a flavor list.
#[derive(Clone, Debug, PartialEq, Eq)]
enum FlavorMapping {
    /// The flavors name a secondary output that buck2 exposes as this subtarget.
    Subtarget(&'static str),
    /// The flavors have no buck2 equivalent and are ignored.
    Nothing,
    /// Buck2 does not know these flavors; the label parses but fails at analysis.
    Unrecognized,
}

/// Maps a sorted, platform-free flavor list.
///
/// This is a hardcoded map of flavors that we know can be handled simply as subtargets.
fn map_subtarget_flavors(flavors_parts: &[&str]) -> FlavorMapping {
    use FlavorMapping::Nothing;
    use FlavorMapping::Subtarget;
    use FlavorMapping::Unrecognized;

    match *flavors_parts {
        // Some targets specifically ask for a given platform, we just ignore them
        [] => Nothing,

        // android_binary intermediate/secondary outputs. See https://fburl.com/diffusion/jd3cmnfw
        ["package_string_assets"] => Subtarget("package_string_assets"),
        ["aapt2_link"] => Subtarget("aapt2_link"),
        ["unstripped_native_libraries"] => Subtarget("unstripped_native_libraries"),
        ["proguard_text_output"] => Subtarget("proguard_text_output"),
        ["generate_string_resources"] => Subtarget("generate_string_resources"),
        ["generate_voltron_string_resources"] => Subtarget("generate_voltron_string_resources"),
        ["exo_symlink_tree"] => Subtarget("exo_symlink_tree"),

        // android_library secondary outputs
        ["dummy_r_dot_java"] => Subtarget("dummy_r_dot_java"),

        // Rules depend on `#headers` flavor of C++ libraries to use a
        // dep's headers without linking against it.
        ["headers"] => Subtarget("headers"),

        // This is used by Rust quite a bit
        ["check"] => Subtarget("check"),

        // FIXME(ndmitchell): Most users shouldn't be using strip-debug.
        // We currently can't handle strip-debug, and it's a dependency of Eden,
        // so just ignore it for now. D27984137 aims to add it back properly.
        ["strip-debug"] => Nothing,

        // Used in JEX builder script (https://fburl.com/code/2w2gjkey)
        ["shared"] => Subtarget("shared"),

        // Used by Nullsafe for (android|java)_libraries
        ["nullsafex-json"] => Subtarget("nullsafex-json"),

        // Java/Kotlin sub-targets
        ["class-abi"] => Subtarget("class-abi"),
        ["source-abi"] => Subtarget("source-abi"),
        ["source-only-abi"] => Subtarget("source-only-abi"),

        ["compilation-database"] => Subtarget("compilation-database"),

        // For js_bundle rules. The platform and optimization ("release") flavors are stripped
        // and handled by the configuration. The other flavors are mapped to named outputs.
        ["android"] | ["android", "release"] => Nothing,
        ["android", "dependencies"] | ["android", "dependencies", "release"] => {
            Subtarget("dependencies")
        }
        ["android", "misc"] | ["android", "misc", "release"] => Subtarget("misc"),
        ["android", "source_map"] | ["android", "release", "source_map"] => Subtarget("source_map"),

        _ => Unrecognized,
    }
}

/// The soft error category a flavored label is reported under.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum FlavorCategory {
    /// A platform flavor was present and dropped (whatever else the list contained).
    Platform,
    /// Only flavors buck2 maps to a subtarget or to nothing.
    Subtarget,
    /// Flavors buck2 does not know.
    Unrecognized,
}

impl FlavorCategory {
    fn name(self) -> &'static str {
        match self {
            FlavorCategory::Platform => "platform_flavor",
            FlavorCategory::Subtarget => "legacy_flavor_subtarget",
            FlavorCategory::Unrecognized => "legacy_flavor_unrecognized",
        }
    }
}

/// How a `#flavors` suffix is interpreted, and how its use is reported.
#[derive(Debug, PartialEq, Eq)]
struct ClassifiedFlavors {
    providers: ProvidersName,
    category: FlavorCategory,
    /// Logview trace key: the flavor list with concrete platforms replaced by their family, so
    /// the key space is bounded by the flavor table rather than by the number of labels.
    key: String,
    /// The buck2 spelling the user should switch to, for the error message.
    fix: FlavorFix,
}

#[derive(Debug, PartialEq, Eq)]
enum FlavorFix {
    /// Replace `#flavors` with `[subtarget]`.
    Subtarget(&'static str),
    /// Delete `#flavors`.
    Remove,
    /// No buck2 equivalent.
    Unsupported,
}

impl fmt::Display for FlavorFix {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlavorFix::Subtarget(name) => write!(
                f,
                "write the subtarget as `[{name}]` instead (platforms come from mode files, `--target-platforms` or `-m`, not from the label)"
            ),
            FlavorFix::Remove => write!(
                f,
                "it has no effect in buck2; remove it (platforms come from mode files, `--target-platforms` or `-m`)"
            ),
            FlavorFix::Unsupported => {
                write!(
                    f,
                    "buck2 does not support it and the label fails at analysis"
                )
            }
        }
    }
}

fn classify_flavors(flavors: &str) -> ClassifiedFlavors {
    let mut category = FlavorCategory::Subtarget;
    let mut key_parts: Vec<&str> = Vec::new();
    let mut flavors_parts: Vec<&str> = Vec::new();
    for part in flavors.split(',') {
        match platform_family(part) {
            Some(family) => {
                // Buck1 allowed several platform flavors on one label (`#macosx-x86_64,macosx-arm64`
                // for a fat binary). Only the first one is dropped, to keep today's behaviour.
                if category == FlavorCategory::Platform {
                    flavors_parts.push(part);
                } else {
                    category = FlavorCategory::Platform;
                }
                key_parts.push(family.key());
            }
            None => {
                flavors_parts.push(part);
                key_parts.push(part);
            }
        }
    }

    // sort a flavors list to have a deterministic order.
    flavors_parts.sort_unstable();
    key_parts.sort_unstable();

    let (providers, fix) = match map_subtarget_flavors(&flavors_parts) {
        FlavorMapping::Subtarget(name) => (
            ProvidersName::NonDefault(triomphe::Arc::new(NonDefaultProvidersName::Named(
                buck2_util::arc_str::ArcSlice::new([ProviderName::new_unchecked(name.to_owned())]),
            ))),
            FlavorFix::Subtarget(name),
        ),
        FlavorMapping::Nothing => (ProvidersName::Default, FlavorFix::Remove),
        FlavorMapping::Unrecognized => {
            if category != FlavorCategory::Platform {
                category = FlavorCategory::Unrecognized;
            }
            (
                ProvidersName::NonDefault(triomphe::Arc::new(
                    NonDefaultProvidersName::UnrecognizedFlavor(flavors.into()),
                )),
                FlavorFix::Unsupported,
            )
        }
    };

    let key = match category {
        // Unknown flavors are unbounded (`one_of(dep, source)` attrs probe file names with `#`
        // in them as labels), so they share one key.
        FlavorCategory::Unrecognized => "unrecognized".to_owned(),
        _ => key_parts.join(","),
    };

    ClassifiedFlavors {
        providers,
        category,
        key,
        fix,
    }
}

/// Buck1 uses flavors for a couple different purposes. Some of those flavors have ended
/// up being used by users. In v2, the functionality of most user-visible flavors ends up
/// being handled by either configurations or subtargets. One common use of flavors in
/// v1 was to have references to secondary or intermediate outputs, in v2 we can do that
/// via subtargets.
///
/// Every use is reported as a soft error under one of three categories (`platform_flavor`,
/// `legacy_flavor_subtarget`, `legacy_flavor_unrecognized`) so each family can be migrated and
/// then turned into a hard error on its own. Open source never had buck1 build files to keep
/// working, so there a flavor is an error.
pub fn map_flavors(flavors: &str, full_target: &str) -> buck2_error::Result<ProvidersName> {
    let ClassifiedFlavors {
        providers,
        category,
        key,
        fix,
    } = classify_flavors(flavors);

    if is_open_source() {
        return Err(buck2_error::buck2_error!(
            buck2_error::ErrorTag::Input,
            "buck1 flavor `#{flavors}` on `{full_target}` is not supported: {fix}"
        ));
    }

    let err = buck2_error::buck2_error!(
        buck2_error::ErrorTag::Input,
        "buck1 flavor `#{flavors}` on `{full_target}` is deprecated and will become an error: {fix}"
    );
    // We log the target with the offending flavor here, though in practice we'll have to
    // rely on the wrapping span in order to find it.
    match category {
        FlavorCategory::Platform => deprecation!(
            "platform_flavor",
            err,
            owner: "drollman", started: "2023-03-09",
            low_cardinality_key_for_additional_logview_samples: Some(Box::new(key))
        ),
        FlavorCategory::Subtarget | FlavorCategory::Unrecognized => deprecation!(
            category.name(),
            err,
            owner: "drollman", started: "2023-03-09",
            low_cardinality_key_for_additional_logview_samples: Some(Box::new(key))
        ),
    };

    Ok(providers)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn named(name: &str) -> ProvidersName {
        ProvidersName::NonDefault(triomphe::Arc::new(NonDefaultProvidersName::Named(
            buck2_util::arc_str::ArcSlice::new([ProviderName::new_unchecked(name.to_owned())]),
        )))
    }

    fn unrecognized(flavors: &str) -> ProvidersName {
        ProvidersName::NonDefault(triomphe::Arc::new(
            NonDefaultProvidersName::UnrecognizedFlavor(flavors.into()),
        ))
    }

    #[test]
    fn platform_flavor_alone_is_dropped() {
        let c = classify_flavors("android-arm64");
        assert_eq!(c.providers, ProvidersName::Default);
        assert_eq!(c.category, FlavorCategory::Platform);
        assert_eq!(c.key, "<android>");
        assert_eq!(c.fix, FlavorFix::Remove);
    }

    #[test]
    fn platform_flavor_with_subtarget() {
        let c = classify_flavors("shared,windows-x86_64");
        assert_eq!(c.providers, named("shared"));
        assert_eq!(c.category, FlavorCategory::Platform);
        assert_eq!(c.key, "<windows>,shared");
        assert_eq!(c.fix, FlavorFix::Subtarget("shared"));

        // Order on the label does not matter.
        assert_eq!(
            classify_flavors("android-armv7,shared").key,
            "<android>,shared"
        );
    }

    #[test]
    fn platform_families() {
        for (flavor, key) in [
            ("platform010-clang", "<fbcode-platform>"),
            ("platform010-clang-split-dwarf", "<fbcode-platform>"),
            ("linux-x86_64", "<fbcode-platform>"),
            ("macosx-arm64", "<macosx>"),
            ("macosx-x86_64_minimal_xcode", "<macosx>"),
            ("windows-x86_64", "<windows>"),
            ("iphonesimulator-arm64", "<apple>"),
            // The prefix match swallows the whole flavor, `-tests` included.
            ("iphoneos-arm64-tests", "<apple>"),
            ("watchos-arm64_32", "<apple>"),
        ] {
            let c = classify_flavors(flavor);
            assert_eq!(c.category, FlavorCategory::Platform, "{flavor}");
            assert_eq!(c.key, key, "{flavor}");
            assert_eq!(c.providers, ProvidersName::Default, "{flavor}");
        }
    }

    #[test]
    fn only_first_platform_flavor_is_dropped() {
        // Fat-binary spelling: the second platform survives and makes the label unrecognized.
        let c = classify_flavors("macosx-x86_64,macosx-arm64");
        assert_eq!(c.providers, unrecognized("macosx-x86_64,macosx-arm64"));
        assert_eq!(c.category, FlavorCategory::Platform);
        assert_eq!(c.key, "<macosx>,<macosx>");
        assert_eq!(c.fix, FlavorFix::Unsupported);
    }

    #[test]
    fn subtarget_flavors() {
        for (flavors, subtarget) in [
            ("shared", "shared"),
            ("headers", "headers"),
            ("check", "check"),
            ("compilation-database", "compilation-database"),
            ("unstripped_native_libraries", "unstripped_native_libraries"),
            ("proguard_text_output", "proguard_text_output"),
            ("class-abi", "class-abi"),
            ("android,dependencies", "dependencies"),
            ("android,release,dependencies", "dependencies"),
            ("release,android,misc", "misc"),
            ("android,release,source_map", "source_map"),
        ] {
            let c = classify_flavors(flavors);
            assert_eq!(c.providers, named(subtarget), "{flavors}");
            assert_eq!(c.category, FlavorCategory::Subtarget, "{flavors}");
            assert_eq!(c.fix, FlavorFix::Subtarget(subtarget), "{flavors}");
        }
        // Keys are the sorted flavor list.
        assert_eq!(
            classify_flavors("release,android,misc").key,
            "android,misc,release"
        );
    }

    #[test]
    fn no_op_flavors() {
        for flavors in ["strip-debug", "android", "android,release"] {
            let c = classify_flavors(flavors);
            assert_eq!(c.providers, ProvidersName::Default, "{flavors}");
            assert_eq!(c.category, FlavorCategory::Subtarget, "{flavors}");
            assert_eq!(c.fix, FlavorFix::Remove, "{flavors}");
        }
    }

    #[test]
    fn unrecognized_flavors() {
        for flavors in [
            "link-tree",
            "ios,release,dependencies",
            "default,py3,shared",
            "9.json",
        ] {
            let c = classify_flavors(flavors);
            assert_eq!(c.providers, unrecognized(flavors), "{flavors}");
            assert_eq!(c.category, FlavorCategory::Unrecognized, "{flavors}");
            assert_eq!(c.key, "unrecognized", "{flavors}");
            assert_eq!(c.fix, FlavorFix::Unsupported, "{flavors}");
        }
    }
}
