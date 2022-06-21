/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::hash::Hash;
use std::hash::Hasher;
use std::ops::Deref;
use std::path::Path;

use anyhow::anyhow;
use derive_more::Display;
use ref_cast::RefCast;
use relative_path::RelativePath;
use thiserror::Error;

use crate::fs::paths::ForwardRelativePath;

/// Errors from ForwardRelativePath creation
#[derive(Error, Debug)]
enum FileNameError {
    #[error("file name is empty")]
    Empty,
    #[error("file name is current directory")]
    Dot,
    #[error("file name is parent directory")]
    DotDot,
    #[error("slashes in path: `{0}`")]
    Slashes(String),
}

struct FileNameVerifier;

impl FileNameVerifier {
    fn verify<P: ?Sized + AsRef<str>>(file_name: &P) -> anyhow::Result<()> {
        let file_name = file_name.as_ref();
        if file_name.is_empty() {
            Err(anyhow!(FileNameError::Empty))
        } else if file_name == "." {
            Err(anyhow!(FileNameError::Dot))
        } else if file_name == ".." {
            Err(anyhow!(FileNameError::DotDot))
        } else if file_name.contains('/') || file_name.contains('\\') {
            // Note we do not allow backslashes in file names
            // even if it is valid file name on Linux.
            Err(anyhow!(FileNameError::Slashes(file_name.to_owned())))
        } else {
            // At the moment we allow characters like '\0'
            // even if they are not valid at least on Linux.
            Ok(())
        }
    }
}

/// File name. Cannot be empty, cannot contain slashes, '.' or '..'.
#[repr(transparent)]
#[derive(Display, Debug, RefCast, Ord, PartialOrd, Eq)]
pub struct FileName(str);

impl PartialEq<str> for FileName {
    fn eq(&self, other: &str) -> bool {
        &self.0 == other
    }
}

impl PartialEq<FileName> for str {
    fn eq(&self, other: &FileName) -> bool {
        self == &other.0
    }
}

impl AsRef<Path> for FileName {
    fn as_ref(&self) -> &Path {
        Path::new(&self.0)
    }
}

impl AsRef<str> for FileName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl AsRef<RelativePath> for FileName {
    fn as_ref(&self) -> &RelativePath {
        RelativePath::new(&self.0)
    }
}

impl AsRef<ForwardRelativePath> for FileName {
    fn as_ref(&self) -> &ForwardRelativePath {
        ForwardRelativePath::unchecked_new(&self.0)
    }
}

impl FileName {
    /// Creates an `FileName` if the given path represents a correct
    /// platform-indepentent file name, otherwise error.
    ///
    /// ```
    /// use buck2_core::fs::paths::FileName;
    /// assert!(FileName::new("foo").is_ok());
    /// assert!(FileName::new("").is_err());
    /// assert!(FileName::new(".").is_err());
    /// assert!(FileName::new("..").is_err());
    /// assert!(FileName::new(".x").is_ok());
    /// assert!(FileName::new("foo/bar").is_err());
    /// assert!(FileName::new("foo/").is_err());
    /// assert!(FileName::new("foo\\bar").is_err());
    /// ```
    pub fn new<S: ?Sized + AsRef<str>>(s: &S) -> anyhow::Result<&Self> {
        FileNameVerifier::verify(s)?;
        Ok(Self::unchecked_new(s))
    }

    pub fn unchecked_new<S: ?Sized + AsRef<str>>(s: &S) -> &Self {
        FileName::ref_cast(s.as_ref())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Extracts the stem (non-extension) portion of [`self.file_name`].
    ///
    /// The stem is:
    ///
    /// * [`None`], if there is no file name;
    /// * The entire file name if there is no embedded `.`;
    /// * The entire file name if the file name begins with `.` and has no other
    ///   `.`s within;
    /// * Otherwise, the portion of the file name before the final `.`
    ///
    /// ```
    /// use buck2_core::fs::paths::FileName;
    ///
    /// let path = FileName::new("foo.rs")?;
    ///
    /// assert_eq!(Some("foo"), path.file_stem());
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn file_stem(&self) -> Option<&str> {
        ForwardRelativePath::unchecked_new(&self.0).file_stem()
    }

    /// Extracts the extension of [`self.file_name`], if possible.
    ///
    /// ```
    ///
    /// use buck2_core::fs::paths::FileName;
    ///
    /// assert_eq!(Some("rs"), FileName::new("foo.rs")?.extension());
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn extension(&self) -> Option<&str> {
        ForwardRelativePath::unchecked_new(&self.0).extension()
    }
}

impl PartialEq for FileName {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl Hash for FileName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

impl ToOwned for FileName {
    type Owned = FileNameBuf;

    fn to_owned(&self) -> FileNameBuf {
        FileNameBuf(self.0.to_owned())
    }
}

/// Owned version of [`FileName`].
#[derive(Ord, PartialOrd, Eq, Display, Debug, Clone)]
pub struct FileNameBuf(String);

impl FileNameBuf {
    pub fn unchecked_new(s: String) -> Self {
        Self(s)
    }

    pub fn into_inner(self) -> String {
        self.0
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl PartialEq for FileNameBuf {
    fn eq(&self, other: &Self) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<String> for FileNameBuf {
    fn eq(&self, other: &String) -> bool {
        &self.0 == other
    }
}

impl Hash for FileNameBuf {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.as_str().hash(state)
    }
}

impl Borrow<FileName> for FileNameBuf {
    fn borrow(&self) -> &FileName {
        self.as_ref()
    }
}

impl Deref for FileNameBuf {
    type Target = FileName;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl Borrow<str> for FileNameBuf {
    fn borrow(&self) -> &str {
        self.as_ref()
    }
}

impl AsRef<FileName> for FileNameBuf {
    fn as_ref(&self) -> &FileName {
        FileName::unchecked_new(&self.0)
    }
}

impl AsRef<Path> for FileNameBuf {
    fn as_ref(&self) -> &Path {
        Path::new(self.0.as_str())
    }
}

impl AsRef<str> for FileNameBuf {
    fn as_ref(&self) -> &str {
        self.0.as_str()
    }
}

impl AsRef<RelativePath> for FileNameBuf {
    fn as_ref(&self) -> &RelativePath {
        RelativePath::new(self.0.as_str())
    }
}

impl AsRef<ForwardRelativePath> for FileNameBuf {
    fn as_ref(&self) -> &ForwardRelativePath {
        ForwardRelativePath::unchecked_new(self.0.as_str())
    }
}

impl TryFrom<String> for FileNameBuf {
    type Error = anyhow::Error;

    fn try_from(value: String) -> anyhow::Result<FileNameBuf> {
        FileNameVerifier::verify(value.as_str())?;
        Ok(FileNameBuf(value))
    }
}
