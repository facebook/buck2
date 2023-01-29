/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;

use allocative::Allocative;
use buck2_util::arc_str::ThinArcStr;
use dupe::Dupe;

use crate::ascii_char_set::AsciiCharSet;

/// 'TargetName' is the name given to a particular target.
/// e.g. `foo` in the label `fbsource//package/path:foo`.
#[derive(
    Clone,
    Debug,
    Dupe,
    derive_more::Display,
    Hash,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Allocative
)]
// TODO intern this?
pub struct TargetName(ThinArcStr);

#[derive(thiserror::Error, Debug)]
enum InvalidTarget {
    #[error(
        "Invalid target name `{}`. Target names are non-empty strings and can only contain alpha numeric characters, and symbols \
        `,`, `.`, `=`, `-`, `/`, `~`, `@`, `!`, `+`, `$`, and `_`. No other characters are allowed.",
        _0
    )]
    InvalidName(String),
    #[error(
        "found inner providers label when target names are expected. remove `[...]` portion of the target name from `{}`",
        _0
    )]
    FoundProvidersLabel(String),
}

impl TargetName {
    pub fn new(name: &str) -> anyhow::Result<Self> {
        if Self::verify(name) {
            Ok(Self(ThinArcStr::from(name)))
        } else {
            if let Some((_, p)) = name.split_once('[') {
                if p.contains(']') {
                    return Err(anyhow::anyhow!(InvalidTarget::FoundProvidersLabel(
                        name.to_owned()
                    )));
                }
            }
            Err(anyhow::anyhow!(InvalidTarget::InvalidName(name.to_owned())))
        }
    }

    pub fn unchecked_new(name: &str) -> Self {
        Self(ThinArcStr::from(name))
    }

    fn verify(name: &str) -> bool {
        const VALID_CHARS: &str =
            r"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_,.=-\/~@!+$";
        const SET: AsciiCharSet = AsciiCharSet::new(VALID_CHARS);

        !name.is_empty() && name.as_bytes().iter().all(|&b| SET.contains(b))
    }

    pub fn value(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for TargetName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for TargetName {
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl PartialEq<String> for TargetName {
    fn eq(&self, other: &String) -> bool {
        *self.0 == *other
    }
}

impl PartialEq<str> for TargetName {
    fn eq(&self, other: &str) -> bool {
        *self.0 == *other
    }
}

#[cfg(test)]
mod tests {

    use buck2_util::arc_str::ThinArcStr;

    use crate::target::name::TargetName;

    #[test]
    fn target_name_validation() {
        assert_eq!(
            TargetName::new("foo").unwrap(),
            TargetName(ThinArcStr::from("foo"))
        );
        assert_eq!(
            // Copied allowed symbols from above.
            // `,`, `.`, `=`, `-`, `/`, `~`, `@`, `!`, `+` and `_`
            TargetName::new("foo,.=-/~@$!+_1").unwrap(),
            TargetName(ThinArcStr::from("foo,.=-/~@$!+_1"))
        );
        assert!(TargetName::new("foo bar").is_err());
        assert!(TargetName::new("foo?bar").is_err());

        if let Err(e) = TargetName::new("target[label]") {
            let msg = format!("{:#}", e);
            assert!(msg.contains("found inner providers label when target names are expected. remove `[...]` portion of the target name from `target[label]`"), "{}", msg);
        } else {
            panic!("should have gotten an error")
        }
    }
}
