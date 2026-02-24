/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

#![allow(dead_code)] // This mod will be used in the next diffs.

use regex::Regex;

/// Map from target regex to bookmark names.
pub(crate) struct TargetBookmarkMap {
    /// Map from target regex to bookmark names.
    /// If multiple regexes match a target, the first in the vec will be used.
    target_regex_to_bookmark_map: Vec<(Regex, Vec<String>)>,
}

impl TargetBookmarkMap {
    pub(crate) fn new_with_facebook_defaults() -> buck2_error::Result<Self> {
        // This is v0 of the map. Until this is deprecated, new entries should be added here.
        // Options to make this better:
        // 1. Replace with a configerator/buckconfig based map.
        // 2. Auto-determine the bookmark based on the target similar to arc stable.
        Ok(Self {
            target_regex_to_bookmark_map: vec![
                (
                    Regex::new(r"fbsource//fbandroid/apps/instagram/")?,
                    vec!["ig4a_stable".to_owned(), "fbandroid_stable".to_owned()],
                ),
                (
                    Regex::new(
                        r"fbsource//fbandroid/apps/(messenger/fastparse|fb4a/fastparse|stella)",
                    )?,
                    vec!["fbandroid_stable".to_owned()],
                ),
                (
                    Regex::new(r"wa_android//app/whatsapp:whatsapp_(consumer|smb|vr)_debug")?,
                    vec!["fbsource_waandroid_stable".to_owned()],
                ),
                (
                    Regex::new(r"wa_android//apps/wearos:apk")?,
                    vec!["fbsource_waandroid_stable".to_owned()],
                ),
                (
                    Regex::new(r"waios//:WhatsApp(SMB|Catalyst)?")?,
                    vec!["fbsource_waios_stable".to_owned()],
                ),
                (
                    Regex::new(r"waios//modules/UILibrary:UIGallery")?,
                    vec!["fbsource_waios_stable".to_owned()],
                ),
                (
                    Regex::new(r"fbsource//arvr/apps/worlds")?,
                    vec!["worlds_stable".to_owned()],
                ),
                (
                    Regex::new(r"fbcode//aps_models/ads")?,
                    vec!["fbcode/aps/ads/stable".to_owned()],
                ),
                (
                    Regex::new(
                        r"fbsource//fbobjc/Apps/Instagram/(Instagram|Basel|Barcelona):(Instagram|Basel|Barcelona)(NoExtensions)?",
                    )?,
                    vec!["igios_stable".to_owned(), "fbobjc_stable".to_owned()],
                ),
                (
                    Regex::new(r"fbsource//fbobjc/Apps/(LightSpeed|Wilde/Facebook|Stella)")?,
                    vec!["fbobjc_stable".to_owned()],
                ),
                (
                    Regex::new(r"fbcode//rl_robot")?,
                    vec!["fbcode/rl_robot/stable".to_owned()],
                ),
            ],
        })
    }

    pub(crate) fn new_with_target_prefix_to_bookmark_map(map: Vec<(Regex, Vec<String>)>) -> Self {
        Self {
            // This could be constructed from a configerator backed map. Clone the map.
            target_regex_to_bookmark_map: map.clone(),
        }
    }

    /// Returns the bookmarks corresponding to the first regex that matches the given target.
    pub(crate) fn get_best_bookmarks(&self, target: &str) -> Vec<String> {
        self.target_regex_to_bookmark_map
            .iter()
            .find(|(regex, _)| regex.is_match(target))
            .map(|(_, bookmarks)| bookmarks.clone())
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_map() -> TargetBookmarkMap {
        TargetBookmarkMap::new_with_target_prefix_to_bookmark_map(vec![
            (
                Regex::new(r"^fbcode").unwrap(),
                vec!["fbcode/warm".to_owned()],
            ),
            (
                Regex::new(r"^fbsource//arvr/tools").unwrap(),
                vec!["rl_land".to_owned()],
            ),
            (
                Regex::new(r"fbsource//fbandroid/apps/instagram/\w+/fastparse").unwrap(),
                vec!["ig4a_stable".to_owned()],
            ),
        ])
    }

    fn test_map_multiple_bookmarks() -> TargetBookmarkMap {
        TargetBookmarkMap::new_with_target_prefix_to_bookmark_map(vec![
            (
                Regex::new(r"^fbcode").unwrap(),
                vec!["fbcode/warm".to_owned(), "fbcode/stable".to_owned()],
            ),
            (
                Regex::new(r"^fbsource//arvr/tools").unwrap(),
                vec!["rl_land".to_owned(), "arvr_stable".to_owned()],
            ),
        ])
    }

    #[test]
    fn test_best_bookmark() {
        let map = test_map();
        assert_eq!(
            vec!["fbcode/warm"],
            map.get_best_bookmarks("fbcode//buck2/app/buck2_core")
        );
        assert_eq!(
            vec!["rl_land"],
            map.get_best_bookmarks("fbsource//arvr/tools/bin2c:bin2c")
        );
        assert_eq!(vec!["fbcode/warm"], map.get_best_bookmarks("fbcode//tools"));
        assert_eq!(
            vec!["ig4a_stable"],
            map.get_best_bookmarks("fbsource//fbandroid/apps/instagram/instagram/fastparse")
        );
    }

    #[test]
    fn test_multiple_bookmarks() {
        let map = test_map_multiple_bookmarks();
        assert_eq!(
            vec!["fbcode/warm", "fbcode/stable"],
            map.get_best_bookmarks("fbcode//buck2/app/buck2_core")
        );
        assert_eq!(
            vec!["rl_land", "arvr_stable"],
            map.get_best_bookmarks("fbsource//arvr/tools/bin2c:bin2c")
        );
    }

    #[test]
    fn test_unmapped_target() {
        let map = test_map();
        assert_eq!(Vec::<String>::new(), map.get_best_bookmarks("foo//bar"));
        assert_eq!(Vec::<String>::new(), map.get_best_bookmarks(""));
    }

    #[test]
    fn test_partial_match() {
        let map = test_map();
        assert_eq!(Vec::<String>::new(), map.get_best_bookmarks("fbsource")); // Regex expects longer path.
        assert_eq!(Vec::<String>::new(), map.get_best_bookmarks("foo//fbcode")); // Regex expects substring as start.
    }
}
