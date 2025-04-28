/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::ffi::OsString;
use std::fmt;
use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;
use std::str::FromStr;

use allocative::Allocative;
use derive_more::Display;
use ref_cast::RefCast;

use crate::fs::cwd;

#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
enum AbsPathError {
    #[error("expected an absolute path but got a relative path instead: `{}`", _0.display())]
    PathNotAbsolute(PathBuf),
    #[error("Cannot convert path to UTF-8, `{0:?}`")]
    PathCannotBeConvertedToUtf8(OsString),
}

#[derive(Hash, Eq, PartialEq, PartialOrd, Ord, RefCast)]
#[repr(transparent)]
pub struct AbsPath(Path);

#[derive(
    Hash,
    Eq,
    PartialEq,
    Ord,
    PartialOrd,
    Clone,
    Allocative,
    Display,
    serde::Serialize,
    serde::Deserialize
)]
#[display("{}", _0.display())]
pub struct AbsPathBuf(PathBuf);

impl fmt::Debug for AbsPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl fmt::Debug for AbsPathBuf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.0, f)
    }
}

impl AsRef<Path> for AbsPath {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

impl AsRef<AbsPath> for AbsPath {
    fn as_ref(&self) -> &AbsPath {
        self
    }
}

impl AsRef<Path> for AbsPathBuf {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

impl AsRef<AbsPath> for AbsPathBuf {
    fn as_ref(&self) -> &AbsPath {
        self
    }
}

impl Deref for AbsPath {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Deref for AbsPathBuf {
    type Target = AbsPath;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self.0.as_path() as *const Path as *const AbsPath) }
    }
}

impl Borrow<AbsPath> for AbsPathBuf {
    fn borrow(&self) -> &AbsPath {
        self
    }
}

impl ToOwned for AbsPath {
    type Owned = AbsPathBuf;

    fn to_owned(&self) -> Self::Owned {
        AbsPathBuf(self.0.to_owned())
    }
}

impl PartialEq<AbsPath> for AbsPathBuf {
    fn eq(&self, other: &AbsPath) -> bool {
        self.0 == other.0
    }
}

impl PartialEq<&'_ AbsPath> for AbsPathBuf {
    fn eq(&self, other: &&AbsPath) -> bool {
        self.0 == other.0
    }
}

impl PartialEq<AbsPathBuf> for AbsPath {
    fn eq(&self, other: &AbsPathBuf) -> bool {
        self.0 == other.0
    }
}

impl PartialEq<AbsPathBuf> for &'_ AbsPath {
    fn eq(&self, other: &AbsPathBuf) -> bool {
        self.0 == other.0
    }
}

impl AbsPath {
    pub fn new<'a, P: AsRef<Path> + ?Sized>(path: &'a P) -> buck2_error::Result<&'a AbsPath> {
        // Wrapper function to make sure the lifetimes are right
        fn inner(path: &Path) -> buck2_error::Result<&AbsPath> {
            if path.is_absolute() {
                // SAFETY: repr transparent.
                Ok(unsafe { &*(path as *const Path as *const AbsPath) })
            } else {
                Err(AbsPathError::PathNotAbsolute(path.to_path_buf()).into())
            }
        }
        inner(path.as_ref())
    }

    pub fn as_path(&self) -> &Path {
        &self.0
    }

    pub fn to_str(&self) -> buck2_error::Result<&str> {
        match self.0.to_str() {
            Some(s) => Ok(s),
            None => Err(AbsPathError::PathCannotBeConvertedToUtf8(self.0.to_owned().into()).into()),
        }
    }

    pub fn join<P: AsRef<Path>>(&self, other: P) -> AbsPathBuf {
        let path = self.0.join(other);
        AbsPathBuf::new(path).unwrap()
    }

    pub fn parent(&self) -> Option<&AbsPath> {
        self.0.parent().map(|p| AbsPath::new(p).unwrap())
    }

    pub fn strip_prefix<P: AsRef<AbsPath>>(&self, prefix: P) -> buck2_error::Result<&Path> {
        Ok(self.0.strip_prefix(prefix.as_ref())?)
    }

    pub fn ancestors(&self) -> impl Iterator<Item = &'_ AbsPath> {
        // Taking the ancestors of an AbsPath gives you more AbsPath.
        self.0.ancestors().map(AbsPath::ref_cast)
    }

    pub fn as_maybe_relativized(&self) -> &Path {
        cwd::maybe_relativize(&self.0)
    }

    pub fn as_maybe_relativized_str(&self) -> buck2_error::Result<&str> {
        Ok(cwd::maybe_relativize_str(self.to_str()?))
    }
}

impl AbsPathBuf {
    pub fn new<P: AsRef<Path>>(path: P) -> buck2_error::Result<Self> {
        let p = AbsPath::new(path.as_ref())?;
        Ok(p.to_owned())
    }

    pub fn into_path_buf(self) -> PathBuf {
        self.0
    }

    pub fn into_os_string(self) -> OsString {
        self.0.into_os_string()
    }

    /// Convert a path into a String. Fails if the path is not UTF8.
    pub fn into_string(self) -> buck2_error::Result<String> {
        self.into_os_string()
            .into_string()
            .map_err(|x| AbsPathError::PathCannotBeConvertedToUtf8(x).into())
    }

    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional)
    }

    pub fn shrink_to_fit(&mut self) {
        self.0.shrink_to_fit()
    }

    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.0.shrink_to(min_capacity)
    }

    pub fn push<P: AsRef<Path>>(&mut self, path: P) {
        self.0.push(path);
        assert!(self.0.is_absolute());
    }

    pub fn pop(&mut self) -> bool {
        let r = self.0.pop();
        assert!(self.0.is_absolute());
        r
    }

    pub fn set_extension<S: AsRef<str>>(&mut self, extension: S) {
        self.0.set_extension(extension.as_ref());
        assert!(self.0.is_absolute());
    }
}

impl TryFrom<PathBuf> for AbsPathBuf {
    type Error = buck2_error::Error;

    fn try_from(path: PathBuf) -> Result<Self, Self::Error> {
        AbsPath::new(&path)?;
        Ok(AbsPathBuf(path))
    }
}

impl TryFrom<String> for AbsPathBuf {
    type Error = buck2_error::Error;

    fn try_from(path: String) -> Result<Self, Self::Error> {
        AbsPathBuf::try_from(PathBuf::from(path))
    }
}

impl FromStr for AbsPathBuf {
    type Err = buck2_error::Error;

    fn from_str(s: &str) -> buck2_error::Result<AbsPathBuf> {
        AbsPathBuf::try_from(s.to_owned())
    }
}
