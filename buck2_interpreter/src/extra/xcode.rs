use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;
use buck2_core::fs::fs_util;
use serde::Deserialize;

const XCODE_SELECT_SYMLINK: &str = "/var/db/xcode_select_link";

/// Only fields we care about from Xcode Info.plist.
#[derive(Deserialize)]
#[allow(non_snake_case)]
struct XcodeInfoPlist {
    CFBundleShortVersionString: String,
    CFBundleIconName: String,
}

/// Versioning information for the currently selected Xcode on the host machine.
#[derive(Debug, Default, PartialEq)]
pub struct XcodeVersionInfo {
    /// e.g. "14.0.1"
    pub version_string: Option<String>,
    /// The "14" in "14.0.1"
    pub major_version: Option<String>,
    /// The "0" in "14.0.1"
    pub minor_version: Option<String>,
    /// The "1" in "14.0.1"
    pub patch_version: Option<String>,
    /// Xcode-specific build number like "14A309"
    pub build_number: Option<String>,
    /// Whether this Xcode is considered a "beta" version.
    pub is_beta: bool,
}

impl XcodeVersionInfo {
    // Construct from Xcode app name, falling back to Info.plist if possible.
    pub fn new() -> anyhow::Result<Self> {
        let resolved_xcode_path = fs_util::canonicalize(&PathBuf::from(XCODE_SELECT_SYMLINK))
            .context("resolve selected xcode link")?;
        Self::from_path(&resolved_xcode_path).or_else(|_| {
            let plist_path = resolved_xcode_path
                .parent()
                .map(|base| base.join("Info.plist"))
                .ok_or_else(|| anyhow::anyhow!("unable to construct path to Xcode Info.plist"))?;
            Self::from_info_plist(&plist_path)
        })
    }

    // Parse version info from paths like
    // /Applications/Xcode_14.0.0_14A309_fb.app/Contents/Developer
    pub fn from_path(path: &Path) -> anyhow::Result<Self> {
        Self::from_path_impl(path)
            .ok_or_else(|| anyhow::anyhow!("failed to parse path: {}", path.display()))
    }

    fn from_path_impl(path: &Path) -> Option<Self> {
        let app_name = path.components().nth(2)?.as_os_str().to_str()?;
        let mut app_name_parts = app_name.split('_').skip(1);

        // Version number information.
        let version = app_name_parts.next()?;
        let mut version_parts = version.split('.');
        let major = version_parts.next()?;
        let minor = version_parts.next()?;
        let patch = version_parts.next()?;

        let (build_number, is_beta) = match app_name_parts.next() {
            Some("beta") => (None, true),
            Some(build_id) => (Some(build_id), false),
            None => (None, false),
        };

        Some(Self {
            version_string: Some(version.to_owned()),
            major_version: Some(major.to_owned()),
            minor_version: Some(minor.to_owned()),
            patch_version: Some(patch.to_owned()),
            build_number: build_number.map(|b| b.to_owned()),
            is_beta,
        })
    }

    // Parse version information from Info.plist in root of Xcode install dir.
    pub fn from_info_plist(plist_path: &Path) -> anyhow::Result<Self> {
        let plist: XcodeInfoPlist =
            plist::from_file(plist_path).context("deserializing Xcode Info.plist")?;

        let version_parts = &mut plist.CFBundleShortVersionString.split('.');
        let major = version_parts.next().map(|v| v.to_owned());
        let minor = version_parts.next().map(|v| v.to_owned());
        let patch = version_parts
            .next()
            .map(|v| v.to_owned())
            .or_else(|| Some("0".to_owned()));

        Ok(Self {
            version_string: Some(plist.CFBundleShortVersionString),
            major_version: major,
            minor_version: minor,
            patch_version: patch,
            // Build Identifier isn't actually stored in Info.plist. Weird.
            build_number: None,
            // Info.plist marks the icon names as "XcodeBeta".
            is_beta: plist.CFBundleIconName.contains("Beta"),
        })
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::PathBuf;

    use super::XcodeVersionInfo;

    #[test]
    fn test_fails_to_resolve_from_path() {
        let got = XcodeVersionInfo::from_path(&PathBuf::from(
            "/Applications/Xcode.app/Contents/Developer",
        ));
        assert!(got.is_err());
    }

    #[test]
    fn test_resolves_version_from_path() {
        let got = XcodeVersionInfo::from_path(&PathBuf::from(
            "/Applications/Xcode_14.0.0_14A309_fb.app/Contents/Developer",
        ))
        .unwrap();
        let want = XcodeVersionInfo {
            version_string: Some("14.0.0".to_owned()),
            major_version: Some("14".to_owned()),
            minor_version: Some("0".to_owned()),
            patch_version: Some("0".to_owned()),
            build_number: Some("14A309".to_owned()),
            is_beta: false,
        };
        assert_eq!(want, got);
    }

    #[test]
    fn test_resolves_beta_version_from_path() {
        let got = XcodeVersionInfo::from_path(&PathBuf::from(
            "/Applications/Xcode_14.0.0_beta_6.app/Contents/Developer",
        ))
        .unwrap();
        let want = XcodeVersionInfo {
            version_string: Some("14.0.0".to_owned()),
            major_version: Some("14".to_owned()),
            minor_version: Some("0".to_owned()),
            patch_version: Some("0".to_owned()),
            build_number: None,
            is_beta: true,
        };
        assert_eq!(want, got);
    }

    #[test]
    fn test_resolves_version_from_path_fail() {
        let got = XcodeVersionInfo::from_path(&PathBuf::from(
            "/Applications/Xcode_foo_bar.app/Contents/Developer",
        ));
        assert!(got.is_err());
    }

    fn write_plist(plist_content: &str) -> (tempfile::TempDir, PathBuf) {
        let workspace = tempfile::tempdir().expect("failed to create tempdir");
        let fake_xcode_dir = workspace.path().join("xcode_foo_bar.app").join("Contents");
        fs::DirBuilder::new()
            .recursive(true)
            .create(&fake_xcode_dir)
            .unwrap();
        let plist_path = fake_xcode_dir.join("Info.plist");
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
    <key>CFBundleIconName</key>
    <string>Xcode</string>
    <key>CFBundleShortVersionString</key>
    <string>14.0.1</string>
</dict>
</plist>
        "#,
        );

        let got = XcodeVersionInfo::from_info_plist(&plist).expect("failed to parse version info");
        let want = XcodeVersionInfo {
            version_string: Some("14.0.1".to_owned()),
            major_version: Some("14".to_owned()),
            minor_version: Some("0".to_owned()),
            patch_version: Some("1".to_owned()),
            is_beta: false,
            build_number: None,
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
    <key>CFBundleIconName</key>
    <string>Xcode</string>
    <key>CFBundleShortVersionString</key>
    <string>14.0</string>
</dict>
</plist>
        "#,
        );

        let got = XcodeVersionInfo::from_info_plist(&plist).expect("failed to parse version info");
        let want = XcodeVersionInfo {
            version_string: Some("14.0".to_owned()),
            major_version: Some("14".to_owned()),
            minor_version: Some("0".to_owned()),
            patch_version: Some("0".to_owned()),
            is_beta: false,
            build_number: None,
        };
        assert_eq!(want, got);
    }

    #[test]
    fn test_resolves_version_from_plist_beta() {
        let (_t, plist) = write_plist(
            r#"
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleIconName</key>
    <string>XcodeBeta</string>
    <key>CFBundleShortVersionString</key>
    <string>14.0</string>
</dict>
</plist>
        "#,
        );

        let got = XcodeVersionInfo::from_info_plist(&plist).expect("failed to parse version info");
        let want = XcodeVersionInfo {
            version_string: Some("14.0".to_owned()),
            major_version: Some("14".to_owned()),
            minor_version: Some("0".to_owned()),
            patch_version: Some("0".to_owned()),
            is_beta: true,
            build_number: None,
        };
        assert_eq!(want, got);
    }
}
