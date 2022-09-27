/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use once_cell::sync::OnceCell;
use regex::RegexSet;

use crate::provider::label::ProviderName;
use crate::provider::label::ProvidersName;

static PLATFORM_REGEX_SET: OnceCell<RegexSet> = OnceCell::new();

fn is_platform_flavor(flavor: &str) -> bool {
    let regex_set = PLATFORM_REGEX_SET.get_or_init(|| {
        // copied from https://fburl.com/code/sgxwq68n and modified for our needs
        RegexSet::new(&[
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
pub fn map_flavors(flavors: &str) -> anyhow::Result<ProvidersName> {
    let mut flavors_parts: Vec<&str> = flavors.split(',').collect();
    assert!(!flavors_parts.is_empty());

    match flavors_parts.iter().position(|x| is_platform_flavor(x)) {
        Some(index) => {
            // remove platform flavor from the vector of flavors
            flavors_parts.remove(index);
        }
        None => {}
    }

    // sort a flavors list to have a deterministic order.
    flavors_parts.sort_unstable();
    Ok(ProvidersName::Named(vec![ProviderName::new_unchecked(
        match flavors_parts.len() {
            // If we only had one flavor that represents some specific platform then return a default provider name.
            0 => {
                // Some targets specifically ask for a given platform, we just ignore them
                return Ok(ProvidersName::Default);
            }

            1 => match flavors_parts[0] {
                // android_binary intermediate/secondary outputs. See https://fburl.com/diffusion/jd3cmnfw
                "package_string_assets" => "package_string_assets".to_owned(),
                "aapt2_link" => "aapt2_link".to_owned(),
                "unstripped_native_libraries" => "unstripped_native_libraries".to_owned(),
                "proguard_text_output" => "proguard_text_output".to_owned(),
                "generate_string_resources" => "generate_string_resources".to_owned(),
                "exo_symlink_tree" => "exo_symlink_tree".to_owned(),

                // Rules depend on `#headers` flavor of C++ libraries to use a
                // dep's headers without linking against it.
                "headers" => "headers".to_owned(),

                // This is used by Rust quite a bit
                "check" => "check".to_owned(),

                // FIXME(ndmitchell): Most users shouldn't be using strip-debug.
                // We currently can't handle strip-debug, and it's a dependency of Eden,
                // so just ignore it for now. D27984137 aims to add it back properly.
                "strip-debug" => return Ok(ProvidersName::Default),

                // Used in JEX builder script (https://fburl.com/code/2w2gjkey)
                "shared" => "shared".to_owned(),

                // This is for js_bundle. We strip it and let the configuration handle it instead.
                "android" => return Ok(ProvidersName::Default),

                _ => return Ok(ProvidersName::UnrecognizedFlavor(flavors.to_owned())),
            },

            // For js_bundle rules. The platform and optimization ("release") flavors are stripped
            // and handled by the configuration. The other flavors are mapped to named outputs.
            2 => match (flavors_parts[0], flavors_parts[1]) {
                ("android", "dependencies") => "dependencies".to_owned(),
                ("android", "misc") => "misc".to_owned(),
                ("android", "source_map") => "source_map".to_owned(),
                ("android", "release") => return Ok(ProvidersName::Default),
                _ => return Ok(ProvidersName::UnrecognizedFlavor(flavors.to_owned())),
            },

            3 => match (flavors_parts[0], flavors_parts[1], flavors_parts[2]) {
                ("android", "dependencies", "release") => "dependencies".to_owned(),
                ("android", "misc", "release") => "misc".to_owned(),
                ("android", "release", "source_map") => "source_map".to_owned(),
                _ => return Ok(ProvidersName::UnrecognizedFlavor(flavors.to_owned())),
            },

            4 => match (
                flavors_parts[0],
                flavors_parts[1],
                flavors_parts[2],
                flavors_parts[3],
            ) {
                ("android", "misc", "rambundle-indexed", "release") => {
                    "rambundle-indexed-misc".to_owned()
                }
                _ => return Ok(ProvidersName::UnrecognizedFlavor(flavors.to_owned())),
            },

            // This allows us to pass parsing for this thing.
            _ => return Ok(ProvidersName::UnrecognizedFlavor(flavors.to_owned())),
        },
    )]))
}
