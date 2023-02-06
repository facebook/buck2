/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;
use buck2_core::fs::fs_util;
use serde::Deserialize;
use thiserror::Error;

#[derive(Error, Debug)]
enum XcodeVersionError {
    #[error("Unable to construct path to `version.plist` in Xcode install directory.")]
    UnableToConstructVersionInfoPath,
    #[error("Expected short version `{0}` to contain at least major and minor versions")]
    MalformedShortVersion(String),
}

const XCODE_SELECT_SYMLINK: &str = "/var/db/xcode_select_link";

/// Only fields we care about from Xcode version.plist.
#[derive(Deserialize)]
#[allow(non_snake_case)]
struct XcodeVersionPlistSchema {
    CFBundleShortVersionString: String,
    ProductBuildVersion: String,
}

/// Versioning information for the currently selected Xcode on the host machine.
#[derive(Debug, Default, PartialEq)]
pub struct XcodeVersionInfo {
    /// e.g. "14.0.1"
    pub version_string: String,
    /// The "14" in "14.0.1"
    pub major_version: String,
    /// The "0" in "14.0.1"
    pub minor_version: String,
    /// The "1" in "14.0.1"
    pub patch_version: String,
    /// Xcode-specific build number like "14A309"
    pub build_number: String,
}

impl XcodeVersionInfo {
    // Construct from version.plist in root of Xcode install dir.
    pub fn new() -> anyhow::Result<Self> {
        let resolved_xcode_path = fs_util::canonicalize(PathBuf::from(XCODE_SELECT_SYMLINK))
            .context("resolve selected xcode link")?;
        let plist_path = resolved_xcode_path
            .parent()
            .map(|base| base.as_path().join("version.plist"))
            .ok_or(XcodeVersionError::UnableToConstructVersionInfoPath)?;
        Self::from_plist(&plist_path)
    }

    pub(crate) fn from_plist(plist_path: &Path) -> anyhow::Result<Self> {
        let plist: XcodeVersionPlistSchema =
            plist::from_file(plist_path).context("deserializing Xcode `version.plist`")?;

        let version_parts = &mut plist.CFBundleShortVersionString.split('.');
        let major = version_parts
            .next()
            .ok_or_else(|| {
                XcodeVersionError::MalformedShortVersion(plist.CFBundleShortVersionString.clone())
            })
            .map(|v| v.to_owned())?;
        let minor = version_parts.next().unwrap_or("0").to_owned();
        let patch = version_parts.next().unwrap_or("0").to_owned();
        let build_number = plist.ProductBuildVersion;

        Ok(Self {
            version_string: plist.CFBundleShortVersionString,
            major_version: major,
            minor_version: minor,
            patch_version: patch,
            build_number,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use super::*;

    fn write_plist(plist_content: &str) -> (tempfile::TempDir, PathBuf) {
        let workspace = tempfile::tempdir().expect("failed to create tempdir");
        let fake_xcode_dir = workspace.path().join("xcode_foo_bar.app").join("Contents");
        fs::DirBuilder::new()
            .recursive(true)
            .create(&fake_xcode_dir)
            .unwrap();
        let plist_path = fake_xcode_dir.join("version.plist");
        fs::write(&plist_path, plist_content).expect("failed to write plist");
        (workspace, plist_path)
    }

    #[test]
    fn test_resolves_version_from_plist() {
        let (_t, plist) = write_plist(
            r#"
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
        <key>BuildVersion</key>
        <string>2</string>
        <key>CFBundleShortVersionString</key>
        <string>14.1.2</string>
        <key>CFBundleVersion</key>
        <string>21534.1</string>
        <key>ProductBuildVersion</key>
        <string>14B47b</string>
        <key>ProjectName</key>
        <string>IDEFrameworks</string>
        <key>SourceVersion</key>
        <string>21534001000000000</string>
</dict>
</plist>
        "#,
        );

        let got = XcodeVersionInfo::from_plist(&plist).expect("failed to parse version info");
        let want = XcodeVersionInfo {
            version_string: "14.1.2".to_owned(),
            major_version: "14".to_owned(),
            minor_version: "1".to_owned(),
            patch_version: "2".to_owned(),
            build_number: "14B47b".to_owned(),
        };
        assert_eq!(want, got);
    }

    #[test]
    fn test_resolves_version_from_plist_no_patch_version() {
        let (_t, plist) = write_plist(
            r#"
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
        <key>BuildVersion</key>
        <string>2</string>
        <key>CFBundleShortVersionString</key>
        <string>14.1</string>
        <key>CFBundleVersion</key>
        <string>21534.1</string>
        <key>ProductBuildVersion</key>
        <string>14B47b</string>
        <key>ProjectName</key>
        <string>IDEFrameworks</string>
        <key>SourceVersion</key>
        <string>21534001000000000</string>
</dict>
</plist>
        "#,
        );

        let got = XcodeVersionInfo::from_plist(&plist).expect("failed to parse version info");
        let want = XcodeVersionInfo {
            version_string: "14.1".to_owned(),
            major_version: "14".to_owned(),
            minor_version: "1".to_owned(),
            patch_version: "0".to_owned(),
            build_number: "14B47b".to_owned(),
        };
        assert_eq!(want, got);
    }

    #[test]
    fn test_resolves_version_from_plist_no_minor_no_patch_version() {
        let (_t, plist) = write_plist(
            r#"
            <?xml version="1.0" encoding="UTF-8"?>
            <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
            <plist version="1.0">
            <dict>
                <key>BuildVersion</key>
                <string>35</string>
                <key>CFBundleShortVersionString</key>
                <string>14</string>
                <key>CFBundleVersion</key>
                <string>21335</string>
                <key>ProductBuildVersion</key>
                <string>14A309</string>
                <key>ProjectName</key>
                <string>IDEFrameworks</string>
                <key>SourceVersion</key>
                <string>21335000000000000</string>
            </dict>
            </plist>
        "#,
        );

        let got = XcodeVersionInfo::from_plist(&plist).expect("failed to parse version info");
        let want = XcodeVersionInfo {
            version_string: "14".to_owned(),
            major_version: "14".to_owned(),
            minor_version: "0".to_owned(),
            patch_version: "0".to_owned(),
            build_number: "14A309".to_owned(),
        };
        assert_eq!(want, got);
    }
}
