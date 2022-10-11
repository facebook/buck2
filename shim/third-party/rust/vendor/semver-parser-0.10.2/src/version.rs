//! Version data and functions.
//!
//! This module contains [`Version`] struct, [`parse`] function for building
//! [`Version`] struct from string and some helper data structures and functions.
//!
//! # Examples
//!
//! Parsing `Version` from string and checking its fields:
//!
//! ```
//! use semver_parser::version;
//!
//! # fn try_main() -> Result<(), String> {
//! let version = version::parse("1.2.3-alpha1")?;
//!
//! assert_eq!(version.major, 1);
//! assert_eq!(version.minor, 2);
//! assert_eq!(version.patch, 3);
//!
//! let expected_pre = vec![
//!     version::Identifier::AlphaNumeric(String::from("alpha1")),
//! ];
//!
//! assert_eq!(expected_pre, version.pre);
//! # Ok(())
//! # }
//! #
//! # try_main().unwrap();
//! ```
//! [`Version`]: ./struct.Version.html
//! [`parse`]: ./fn.parse.html

use crate::parser::{self, Parser};
use std::fmt;

/// Structure representing version data.
///
/// `Version` struct has some public fields representing version data, like major/minor version
/// string, patch number and vectors of prefix and build identifiers.
///
/// # Examples
///
/// Parsing `Version` from string and checking its fields:
///
/// ```
/// use semver_parser::version;
///
/// # fn try_main() -> Result<(), String> {
/// let version = version::parse("0.1.2-alpha1")?;
/// assert_eq!(version.major, 0);
/// assert_eq!(version.minor, 1);
/// assert_eq!(version.patch, 2);
/// let expected_pre = vec![version::Identifier::AlphaNumeric(String::from("alpha1"))];
/// assert_eq!(expected_pre, version.pre);
/// # Ok(())
/// # }
/// #
/// # try_main().unwrap();
/// ```
#[derive(Clone, PartialOrd, Ord, Hash, Debug, PartialEq, Eq)]
pub struct Version {
    /// Major version as number (`0` in `"0.1.2"`).
    pub major: u64,
    /// Minor version as number (`1` in `"0.1.2"`).
    pub minor: u64,
    /// Patch version as number (`2` in `"0.1.2"`).
    pub patch: u64,
    /// Pre-release metadata as a vector of `Identifier` (`"alpha1"` in `"0.1.2-alpha1"`
    /// or `7` (numeric) in `"0.1.2-7"`, `"pre"` and `0` (numeric) in `"0.1.2-pre.0"`).
    pub pre: Vec<Identifier>,
    /// Build metadata as a vector of `Identifier` (`"build1"` in `"0.1.2+build1"`
    /// or `7` (numeric) in `"0.1.2+7"`, `"build"` and `0` (numeric) in `"0.1.2+pre.0"`).
    pub build: Vec<Identifier>,
}

/// Helper enum for holding data of alphanumeric or numeric suffix identifiers.
///
/// This enum is used to hold suffix parts of `pre` and `build` fields of
/// [`Version`] struct. Theses suffixes may be either numeric or alphanumeric.
///
/// # Examples
///
/// Parsing [`Version`] with pre-release part composed of two `Identifier`s:
///
/// ```
/// use semver_parser::version;
///
/// # fn try_main() -> Result<(), String> {
/// let version = version::parse("0.1.2-alpha1.0")?;
///
/// let expected_pre = vec![
///     version::Identifier::AlphaNumeric(String::from("alpha1")),
///     version::Identifier::Numeric(0),
/// ];
///
/// assert_eq!(expected_pre, version.pre);
/// # Ok(())
/// # }
/// #
/// # try_main().unwrap();
/// ```
/// [`Version`]: ./struct.Version.html
#[derive(Clone, PartialOrd, Ord, Hash, Debug, PartialEq, Eq)]
pub enum Identifier {
    /// An identifier that's solely numbers.
    Numeric(u64),
    /// An identifier with letters and numbers.
    AlphaNumeric(String),
}

impl Identifier {
    pub fn concat(self, add_str: &str) -> Identifier {
        match self {
            Identifier::Numeric(n) => Identifier::AlphaNumeric(format!("{}{}", n, add_str)),
            Identifier::AlphaNumeric(s) => Identifier::AlphaNumeric(format!("{}{}", s, add_str)),
        }
    }
}

/// Function for parsing version string to [`Version`].
///
/// Returns `Result<`[`Version`]`, String>`, where `String` represents an error while parsing.
///
/// # Examples
///
/// Parsing [`Version`] from string and checking its fields:
///
/// ```
/// use semver_parser::version;
///
/// # fn try_main() -> Result<(), String> {
/// let version = version::parse("0.1.2-alpha1")?;
/// assert_eq!(version.major, 0);
/// assert_eq!(version.minor, 1);
/// assert_eq!(version.patch, 2);
/// let expected_pre = vec![version::Identifier::AlphaNumeric(String::from("alpha1"))];
/// assert_eq!(expected_pre, version.pre);
/// # Ok(())
/// # }
/// #
/// # try_main().unwrap();
/// ```
/// [`Version`]: ./struct.Version.html
pub fn parse(input: &str) -> Result<Version, parser::Error> {
    let mut parser = Parser::new(input)?;
    let version = parser.version()?;

    if !parser.is_eof() {
        return Err(parser::Error::MoreInput(parser.tail()?));
    }

    Ok(version)
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch).expect("write failed");
        if !self.pre.is_empty() {
            let strs: Vec<_> = self.pre.iter().map(ToString::to_string).collect();
            write!(f, "-{}", strs.join(".")).expect("write failed");
        }
        if !self.build.is_empty() {
            let strs: Vec<_> = self.build.iter().map(ToString::to_string).collect();
            write!(f, "+{}", strs.join(".")).expect("write failed");
        }
        Ok(())
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Identifier::Numeric(ref id) => id.fmt(f),
            Identifier::AlphaNumeric(ref id) => id.fmt(f),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::version;

    #[test]
    fn parse_empty() {
        let version = "";

        let parsed = version::parse(version);

        assert!(
            parsed.is_err(),
            "empty string incorrectly considered a valid parse"
        );
    }

    #[test]
    fn parse_blank() {
        let version = "  ";

        let parsed = version::parse(version);

        assert!(
            parsed.is_err(),
            "blank string incorrectly considered a valid parse"
        );
    }

    #[test]
    fn parse_no_minor_patch() {
        let version = "1";

        let parsed = version::parse(version);

        assert!(
            parsed.is_err(),
            format!("'{}' incorrectly considered a valid parse", version)
        );
    }

    #[test]
    fn parse_no_patch() {
        let version = "1.2";

        let parsed = version::parse(version);

        assert!(
            parsed.is_err(),
            format!("'{}' incorrectly considered a valid parse", version)
        );
    }

    #[test]
    fn parse_empty_pre() {
        let version = "1.2.3-";

        let parsed = version::parse(version);

        assert!(
            parsed.is_err(),
            format!("'{}' incorrectly considered a valid parse", version)
        );
    }

    #[test]
    fn parse_letters() {
        let version = "a.b.c";

        let parsed = version::parse(version);

        assert!(
            parsed.is_err(),
            format!("'{}' incorrectly considered a valid parse", version)
        );
    }

    #[test]
    fn parse_with_letters() {
        let version = "1.2.3 a.b.c";

        let parsed = version::parse(version);

        assert!(
            parsed.is_err(),
            format!("'{}' incorrectly considered a valid parse", version)
        );
    }

    #[test]
    fn parse_basic_version() {
        let version = "1.2.3";

        let parsed = version::parse(version).unwrap();

        assert_eq!(1, parsed.major);
        assert_eq!(2, parsed.minor);
        assert_eq!(3, parsed.patch);
    }

    #[test]
    fn parse_trims_input() {
        let version = "  1.2.3  ";

        let parsed = version::parse(version).unwrap();

        assert_eq!(1, parsed.major);
        assert_eq!(2, parsed.minor);
        assert_eq!(3, parsed.patch);
    }

    #[test]
    fn parse_no_major_leading_zeroes() {
        let version = "01.0.0";

        let parsed = version::parse(version);

        assert!(
            parsed.is_err(),
            "01 incorrectly considered a valid major version"
        );
    }

    #[test]
    fn parse_no_minor_leading_zeroes() {
        let version = "0.01.0";

        let parsed = version::parse(version);

        assert!(
            parsed.is_err(),
            "01 incorrectly considered a valid minor version"
        );
    }

    #[test]
    fn parse_no_patch_leading_zeroes() {
        let version = "0.0.01";

        let parsed = version::parse(version);

        assert!(
            parsed.is_err(),
            "01 incorrectly considered a valid patch version"
        );
    }

    #[test]
    fn parse_no_major_overflow() {
        let version = "98765432109876543210.0.0";

        let parsed = version::parse(version);

        assert!(
            parsed.is_err(),
            "98765432109876543210 incorrectly considered a valid major version"
        );
    }

    #[test]
    fn parse_no_minor_overflow() {
        let version = "0.98765432109876543210.0";

        let parsed = version::parse(version);

        assert!(
            parsed.is_err(),
            "98765432109876543210 incorrectly considered a valid minor version"
        );
    }

    #[test]
    fn parse_no_patch_overflow() {
        let version = "0.0.98765432109876543210";

        let parsed = version::parse(version);

        assert!(
            parsed.is_err(),
            "98765432109876543210 incorrectly considered a valid patch version"
        );
    }

    #[test]
    fn parse_basic_prerelease() {
        let version = "1.2.3-pre";

        let parsed = version::parse(version).unwrap();

        let expected_pre = vec![Identifier::AlphaNumeric(String::from("pre"))];
        assert_eq!(expected_pre, parsed.pre);
    }

    #[test]
    fn parse_prerelease_alphanumeric() {
        let version = "1.2.3-alpha1";

        let parsed = version::parse(version).unwrap();

        let expected_pre = vec![Identifier::AlphaNumeric(String::from("alpha1"))];
        assert_eq!(expected_pre, parsed.pre);
    }

    #[test]
    fn parse_prerelease_zero() {
        let version = "1.2.3-pre.0";

        let parsed = version::parse(version).unwrap();

        let expected_pre = vec![
            Identifier::AlphaNumeric(String::from("pre")),
            Identifier::Numeric(0),
        ];
        assert_eq!(expected_pre, parsed.pre);
    }

    #[test]
    fn parse_basic_build() {
        let version = "1.2.3+build";

        let parsed = version::parse(version).unwrap();

        let expected_build = vec![Identifier::AlphaNumeric(String::from("build"))];
        assert_eq!(expected_build, parsed.build);
    }

    #[test]
    fn parse_build_alphanumeric() {
        let version = "1.2.3+build5";

        let parsed = version::parse(version).unwrap();

        let expected_build = vec![Identifier::AlphaNumeric(String::from("build5"))];
        assert_eq!(expected_build, parsed.build);
    }

    #[test]
    fn parse_pre_and_build() {
        let version = "1.2.3-alpha1+build5";

        let parsed = version::parse(version).unwrap();

        let expected_pre = vec![Identifier::AlphaNumeric(String::from("alpha1"))];
        assert_eq!(expected_pre, parsed.pre);

        let expected_build = vec![Identifier::AlphaNumeric(String::from("build5"))];
        assert_eq!(expected_build, parsed.build);
    }

    #[test]
    fn parse_complex_metadata_01() {
        let version = "1.2.3-1.alpha1.9+build5.7.3aedf  ";

        let parsed = version::parse(version).unwrap();

        let expected_pre = vec![
            Identifier::Numeric(1),
            Identifier::AlphaNumeric(String::from("alpha1")),
            Identifier::Numeric(9),
        ];
        assert_eq!(expected_pre, parsed.pre);

        let expected_build = vec![
            Identifier::AlphaNumeric(String::from("build5")),
            Identifier::Numeric(7),
            Identifier::AlphaNumeric(String::from("3aedf")),
        ];
        assert_eq!(expected_build, parsed.build);
    }

    #[test]
    fn parse_complex_metadata_02() {
        let version = "0.4.0-beta.1+0851523";

        let parsed = version::parse(version).unwrap();

        let expected_pre = vec![
            Identifier::AlphaNumeric(String::from("beta")),
            Identifier::Numeric(1),
        ];
        assert_eq!(expected_pre, parsed.pre);

        let expected_build = vec![Identifier::AlphaNumeric(String::from("0851523"))];
        assert_eq!(expected_build, parsed.build);
    }

    #[test]
    fn parse_metadata_overflow() {
        let version = "0.4.0-beta.1+98765432109876543210";

        let parsed = version::parse(version).unwrap();

        let expected_pre = vec![
            Identifier::AlphaNumeric(String::from("beta")),
            Identifier::Numeric(1),
        ];
        assert_eq!(expected_pre, parsed.pre);

        let expected_build = vec![Identifier::AlphaNumeric(String::from(
            "98765432109876543210",
        ))];
        assert_eq!(expected_build, parsed.build);
    }

    #[test]
    fn parse_regression_01() {
        let version = "0.0.0-WIP";

        let parsed = version::parse(version).unwrap();

        assert_eq!(0, parsed.major);
        assert_eq!(0, parsed.minor);
        assert_eq!(0, parsed.patch);

        let expected_pre = vec![Identifier::AlphaNumeric(String::from("WIP"))];
        assert_eq!(expected_pre, parsed.pre);
    }

    #[test]
    fn parse_regression_02() {
        // this is used by really old versions of npm, and is valid according to semver.org
        let version = "1.2.3-beta-1";

        let parsed = version::parse(version).unwrap();

        assert_eq!(1, parsed.major);
        assert_eq!(2, parsed.minor);
        assert_eq!(3, parsed.patch);

        let expected_pre = vec![Identifier::AlphaNumeric(String::from("beta-1"))];
        assert_eq!(expected_pre, parsed.pre);
    }
}
