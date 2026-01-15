/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::borrow::Borrow;
use std::ops::Deref;

use allocative::Allocative;
use buck2_util::arc_str::ThinArcStr;
use dupe::Dupe;
use pagable::Pagable;
use serde::Deserialize;
use serde::Serialize;
use strong_hash::StrongHash;

use crate::ascii_char_set::AsciiCharSet;
use crate::soft_error;

pub const EQ_SIGN_SUBST: &str = "_eqsb_";

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
    Allocative,
    Serialize,
    Deserialize,
    Pagable
)]
// TODO intern this?
pub struct TargetName(#[pagable(flatten_serde)] ThinArcStr);

#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
enum TargetNameError {
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
    #[error("Target name `{0}` has special character `{1}`, which is discouraged")]
    LabelHasSpecialCharacter(String, char),
    #[error("Target name must not be equal to `...`")]
    DotDotDot,
    #[error("Target name `{0}` should not contain pattern: `{1}`")]
    InvalidPattern(String, String),
}

impl TargetName {
    #[inline]
    pub fn new(name: &str) -> buck2_error::Result<Self> {
        TargetNameRef::new(name)?;
        Ok(Self(ThinArcStr::from(name)))
    }

    pub fn testing_new(name: &str) -> Self {
        TargetName::new(name).unwrap()
    }

    fn bad_name_error(name: &str) -> buck2_error::Error {
        if let Some((_, p)) = name.split_once('[') {
            if p.contains(']') {
                return TargetNameError::FoundProvidersLabel(name.to_owned()).into();
            }
        }
        TargetNameError::InvalidName(name.to_owned()).into()
    }

    fn verify(name: &str) -> buck2_error::Result<()> {
        const VALID_CHARS: &str =
            r"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_,.=-\/~@!+$";
        const SET: AsciiCharSet = AsciiCharSet::new(VALID_CHARS);

        if name.is_empty() || !name.as_bytes().iter().all(|&b| SET.contains(b)) {
            return Err(Self::bad_name_error(name));
        }

        if name.contains(EQ_SIGN_SUBST) {
            return Err(
                TargetNameError::InvalidPattern(name.to_owned(), EQ_SIGN_SUBST.to_owned()).into(),
            );
        }

        if name == "..." {
            return Err(TargetNameError::DotDotDot.into());
        }
        if name.contains(',') {
            soft_error!(
                "label_has_comma",
                TargetNameError::LabelHasSpecialCharacter(name.to_owned(), ',').into(),
                deprecation: true,
                quiet: true
            )?;
        }
        if name.contains('$') {
            soft_error!(
                "label_has_dollar_sign",
                TargetNameError::LabelHasSpecialCharacter(name.to_owned(), '$').into(),
                deprecation: true,
                quiet: true
            )?;
        }

        Ok(())
    }

    // Generic `as_ref` confuses typechecker because of overloads.
    #[allow(clippy::should_implement_trait)]
    #[inline]
    pub fn as_ref(&self) -> &TargetNameRef {
        TargetNameRef::unchecked_new(&self.0)
    }
}

impl AsRef<str> for TargetName {
    #[inline]
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl Borrow<str> for TargetName {
    #[inline]
    fn borrow(&self) -> &str {
        &self.0
    }
}

impl PartialEq<String> for TargetName {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        *self.0 == *other
    }
}

impl PartialEq<str> for TargetName {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        *self.0 == *other
    }
}

impl Deref for TargetName {
    type Target = TargetNameRef;

    fn deref(&self) -> &TargetNameRef {
        self.as_ref()
    }
}

#[derive(
    Debug,
    derive_more::Display,
    Hash,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    StrongHash
)]
#[repr(transparent)]
pub struct TargetNameRef(str);

impl TargetNameRef {
    #[inline]
    pub fn new(name: &str) -> buck2_error::Result<&TargetNameRef> {
        TargetName::verify(name)?;
        Ok(TargetNameRef::unchecked_new(name))
    }

    #[inline]
    pub fn unchecked_new(name: &str) -> &TargetNameRef {
        unsafe {
            // SAFETY: `repr(transparent)`.
            &*(name as *const str as *const TargetNameRef)
        }
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    #[inline]
    pub fn to_owned(&self) -> TargetName {
        TargetName(ThinArcStr::from(&self.0))
    }
}

impl Borrow<TargetNameRef> for TargetName {
    #[inline]
    fn borrow(&self) -> &TargetNameRef {
        self.as_ref()
    }
}

impl AsRef<str> for TargetNameRef {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

#[cfg(test)]
mod tests {
    use std::hash::Hash;
    use std::hash::Hasher;

    use buck2_util::arc_str::ThinArcStr;

    use crate::target::name::TargetName;
    use crate::target::name::TargetNameRef;

    #[test]
    fn target_name_validation() {
        assert_eq!(
            TargetName::new("foo").unwrap(),
            TargetName(ThinArcStr::from("foo"))
        );
        assert_eq!(
            // Copied allowed symbols from above.
            // `.`, `-`, `/`, `~`, `@`, `!`, `+` and `_`
            // `,`, `$`, and `=` are currently soft errors and should eventually be removed.
            TargetName::new("foo.-/~@!+_1").unwrap(),
            TargetName(ThinArcStr::from("foo.-/~@!+_1"))
        );
        assert!(TargetName::new("foo bar").is_err());
        assert!(TargetName::new("foo?bar").is_err());
        assert!(TargetName::new("foo_eqsb_bar").is_err());

        if let Err(e) = TargetName::new("target[label]") {
            let msg = format!("{e:#}");
            assert!(msg.contains("found inner providers label when target names are expected. remove `[...]` portion of the target name from `target[label]`"), "{}", msg);
        } else {
            panic!("should have gotten an error")
        }
    }

    #[test]
    fn test_value_and_ref_hashes_equal() {
        fn hash<T: Hash + ?Sized>(t: &T) -> u64 {
            let mut hasher = std::collections::hash_map::DefaultHasher::new();
            t.hash(&mut hasher);
            hasher.finish()
        }

        assert_eq!(
            hash(TargetNameRef::unchecked_new("foo")),
            hash(&TargetName::testing_new("foo"))
        );
    }
}
