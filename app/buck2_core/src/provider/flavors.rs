/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::sync::OnceLock;

use regex::RegexSet;

use crate::provider::label::NonDefaultProvidersName;
use crate::provider::label::ProviderName;
use crate::provider::label::ProvidersName;
use crate::soft_error;

static PLATFORM_REGEX_SET: OnceLock<RegexSet> = OnceLock::new();

fn is_platform_flavor(flavor: &str) -> bool {
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

    regex_set.is_match(flavor)
}

/// Buck1 uses flavors for a couple different purposes. Some of those flavors have ended
/// up being used by users. In v2, the functionality of most user-visible flavors ends up
/// being handled by either configurations or subtargets. One common use of flavors in
/// v1 was to have references to secondary or intermediate outputs, in v2 we can do that
/// via subtargets.
///
/// This mapping is a hardcoded map of flavors that we know can be handled simply as subtargets.
pub fn map_flavors(flavors: &str, full_target: &str) -> buck2_error::Result<ProvidersName> {
    let mut flavors_parts: Vec<&str> = flavors.split(',').collect();
    assert!(!flavors_parts.is_empty());

    if let Some(index) = flavors_parts.iter().position(|x| is_platform_flavor(x)) {
        // remove platform flavor from the vector of flavors
        // We log the target with the offending flavor here, though in practice we'll have to
        // rely on the wrapping span in order to find
        soft_error!(
            "platform_flavor",
            buck2_error::buck2_error!(buck2_error::ErrorTag::Input, "Platform flavor found in target: {}", full_target),
            deprecation: true,
            quiet: true
        )?;
        flavors_parts.remove(index);
    }

    // sort a flavors list to have a deterministic order.
    flavors_parts.sort_unstable();
    Ok(ProvidersName::NonDefault(triomphe::Arc::new(
        NonDefaultProvidersName::Named(buck2_util::arc_str::ArcSlice::new([
            ProviderName::new_unchecked(match *flavors_parts {
                // If we only had one flavor that represents some specific platform then return a default provider name.
                [] => {
                    // Some targets specifically ask for a given platform, we just ignore them
                    return Ok(ProvidersName::Default);
                }

                // android_binary intermediate/secondary outputs. See https://fburl.com/diffusion/jd3cmnfw
                ["package_string_assets"] => "package_string_assets".to_owned(),
                ["aapt2_link"] => "aapt2_link".to_owned(),
                ["unstripped_native_libraries"] => "unstripped_native_libraries".to_owned(),
                ["proguard_text_output"] => "proguard_text_output".to_owned(),
                ["generate_string_resources"] => "generate_string_resources".to_owned(),
                ["generate_voltron_string_resources"] => {
                    "generate_voltron_string_resources".to_owned()
                }
                ["exo_symlink_tree"] => "exo_symlink_tree".to_owned(),

                // android_library secondary outputs
                ["dummy_r_dot_java"] => "dummy_r_dot_java".to_owned(),

                // Rules depend on `#headers` flavor of C++ libraries to use a
                // dep's headers without linking against it.
                ["headers"] => "headers".to_owned(),

                // This is used by Rust quite a bit
                ["check"] => "check".to_owned(),

                // FIXME(ndmitchell): Most users shouldn't be using strip-debug.
                // We currently can't handle strip-debug, and it's a dependency of Eden,
                // so just ignore it for now. D27984137 aims to add it back properly.
                ["strip-debug"] => return Ok(ProvidersName::Default),

                // Used in JEX builder script (https://fburl.com/code/2w2gjkey)
                ["shared"] => "shared".to_owned(),

                // Used by Nullsafe for (android|java)_libraries
                ["nullsafex-json"] => "nullsafex-json".to_owned(),

                // Java/Kotlin sub-targets
                ["class-abi"] => "class-abi".to_owned(),
                ["source-abi"] => "source-abi".to_owned(),
                ["source-only-abi"] => "source-only-abi".to_owned(),

                ["compilation-database"] => "compilation-database".to_owned(),

                // For js_bundle rules. The platform and optimization ("release") flavors are stripped
                // and handled by the configuration. The other flavors are mapped to named outputs.
                ["android"] | ["android", "release"] => return Ok(ProvidersName::Default),
                ["android", "dependencies"] | ["android", "dependencies", "release"] => {
                    "dependencies".to_owned()
                }
                ["android", "misc"] | ["android", "misc", "release"] => "misc".to_owned(),
                ["android", "source_map"] | ["android", "release", "source_map"] => {
                    "source_map".to_owned()
                }

                // This allows us to pass parsing for this thing.
                _ => {
                    return Ok(ProvidersName::NonDefault(triomphe::Arc::new(
                        NonDefaultProvidersName::UnrecognizedFlavor(flavors.into()),
                    )));
                }
            }),
        ])),
    )))
}
