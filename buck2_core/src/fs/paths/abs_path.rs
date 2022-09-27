/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::borrow::Cow;
use std::ffi::OsStr;
use std::ffi::OsString;
use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;

use derive_more::Display;
use ref_cast::RefCast;
use relative_path::RelativePath;
use serde::de::Error;
use serde::Deserialize;
use serde::Serialize;
use thiserror::Error;

use crate::fs::paths::ForwardRelativePath;
use crate::fs::paths::ForwardRelativePathNormalizer;

/// An absolute path. This path is not platform agnostic.
#[derive(Display, Debug, Hash, PartialEq, Eq, Ord, PartialOrd, RefCast)]
#[display(fmt = "{}", "_0.display()")]
#[repr(transparent)]
pub struct AbsPath(Path);

///
/// The owned version of 'AbsPath', like how 'PathBuf' relates to 'Path'
#[derive(Clone, Display, Debug, Hash, PartialEq, Eq, Ord, PartialOrd)]
#[display(fmt = "{}", "_0.display()")]
pub struct AbsPathBuf(PathBuf);

impl AsRef<Path> for AbsPath {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

impl AsRef<Path> for AbsPathBuf {
    fn as_ref(&self) -> &Path {
        &self.0
    }
}

impl AsRef<PathBuf> for AbsPathBuf {
    fn as_ref(&self) -> &PathBuf {
        &self.0
    }
}

impl Deref for AbsPath {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Serialize for AbsPathBuf {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for AbsPathBuf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        AbsPathBuf::new(PathBuf::deserialize(deserializer)?).map_err(D::Error::custom)
    }
}

impl AbsPath {
    pub fn unchecked_new<S: ?Sized + AsRef<OsStr>>(s: &S) -> &Self {
        AbsPath::ref_cast(Path::new(s))
    }

    /// Creates an 'AbsPath' if the given path represents an absolute path,
    /// otherwise error.
    ///
    /// ```
    /// use buck2_core::fs::paths::AbsPath;
    /// use std::path::Path;
    ///
    /// assert!(AbsPath::new("relative/bar").is_err());
    /// assert!(AbsPath::new(Path::new("relative/bar")).is_err());
    ///
    /// if cfg!(not(windows)) {
    ///     assert!(AbsPath::new("/foo/bar").is_ok());
    ///     assert!(AbsPath::new("/").is_ok());
    ///     assert!(AbsPath::new("/normalize/./bar").is_err());
    ///     assert!(AbsPath::new("/normalize/../bar").is_err());
    ///
    ///     assert!(AbsPath::new(Path::new("/foo/bar")).is_ok());
    ///     assert!(AbsPath::new(Path::new("/")).is_ok());
    ///     assert!(AbsPath::new(Path::new("/normalize/./bar")).is_err());
    ///     assert!(AbsPath::new(Path::new("/normalize/../bar")).is_err());
    /// } else {
    ///     assert!(AbsPath::new("c:/foo/bar").is_ok());
    ///     assert!(AbsPath::new("c:/").is_ok());
    ///     assert!(AbsPath::new("c:/normalize/./bar").is_err());
    ///     assert!(AbsPath::new("c:/normalize/../bar").is_err());
    ///     assert!(AbsPath::new("/foo/bar").is_err());
    ///
    ///     assert!(AbsPath::new(Path::new("c:/foo/bar")).is_ok());
    ///     assert!(AbsPath::new(Path::new("c:/")).is_ok());
    ///     assert!(AbsPath::new(Path::new("c:/normalize/./bar")).is_err());
    ///     assert!(AbsPath::new(Path::new("c:/normalize/../bar")).is_err());
    /// }
    /// ```
    pub fn new<P: ?Sized + AsRef<Path>>(p: &P) -> anyhow::Result<&AbsPath> {
        AbsPathVerifier::verify(p.as_ref())?;
        Ok(AbsPath::ref_cast(p.as_ref()))
    }

    /// Creates an owned 'AbsPathBuf' with path adjoined to self.
    ///
    /// ```
    /// use std::path::Path;
    /// use buck2_core::fs::paths::{AbsPathBuf, AbsPath, ForwardRelativePath};
    ///
    /// if cfg!(not(windows)) {
    ///     let abs_path = AbsPath::new("/my")?;
    ///     assert_eq!(AbsPathBuf::from("/my/foo/bar".into())?, abs_path.join(ForwardRelativePath::new("foo/bar")?));
    /// } else {
    ///     let abs_path = AbsPath::new("C:\\my")?;
    ///     assert_eq!("C:\\my\\foo\\bar", abs_path.join(ForwardRelativePath::new("foo/bar")?).to_string());
    /// }
    /// # anyhow::Ok(())
    /// ```
    pub fn join<P: AsRef<ForwardRelativePath>>(&self, path: P) -> AbsPathBuf {
        if cfg!(windows) {
            AbsPathBuf(self.0.join(path.as_ref().as_str().replace('/', "\\")))
        } else {
            AbsPathBuf(self.0.join(path.as_ref().as_str()))
        }
    }

    /// Returns a relative path of the parent directory
    ///
    /// ```
    /// use std::path::Path;
    /// use buck2_core::fs::paths::AbsPath;
    ///
    /// if cfg!(not(windows)) {
    ///     assert_eq!(
    ///         Some(AbsPath::new("/")?),
    ///         AbsPath::new("/my")?.parent()
    ///     );
    ///     assert_eq!(
    ///         None,
    ///         AbsPath::new("/")?.parent()
    ///     );
    /// } else {
    ///     assert_eq!(
    ///         Some(AbsPath::new("c:/")?),
    ///         AbsPath::new("c:/my")?.parent()
    ///     );
    ///     assert_eq!(
    ///         None,
    ///         AbsPath::new("c:/")?.parent()
    ///     );
    /// }
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn parent(&self) -> Option<&AbsPath> {
        self.0.parent().map(AbsPath::ref_cast)
    }

    /// Returns a 'ForwardRelativePath' that, when joined onto `base`, yields
    /// `self`.
    ///
    /// Error if `base` is not a prefix of `self` or the returned
    /// path is not a 'ForwardRelativePath'
    ///
    /// ```
    /// use std::{borrow::Cow, path::Path};
    /// use buck2_core::fs::paths::{AbsPath, ForwardRelativePath, ForwardRelativePathBuf};
    ///
    /// if cfg!(not(windows)) {
    ///     let path = AbsPath::new("/test/foo/bar.txt")?;
    ///
    ///     assert_eq!(
    ///         path.strip_prefix(AbsPath::new("/test")?)?,
    ///         Cow::Borrowed(ForwardRelativePath::new("foo/bar.txt")?)
    ///     );
    ///     assert!(path.strip_prefix(AbsPath::new("/asdf")?).is_err());
    /// } else {
    ///     let path = AbsPath::new(r"C:\test\foo\bar.txt")?;
    ///
    ///     // strip_prefix will return Cow::Owned here but we still
    ///     // can compare it to Cow::Borrowed.
    ///     assert_eq!(
    ///         path.strip_prefix(AbsPath::new("c:/test")?)?,
    ///         Cow::Borrowed(ForwardRelativePath::new("foo/bar.txt")?)
    ///     );
    ///     assert_eq!(
    ///         path.strip_prefix(AbsPath::new(r"c:\test")?)?,
    ///         Cow::Borrowed(ForwardRelativePath::new("foo/bar.txt")?)
    ///     );
    ///     assert_eq!(
    ///         path.strip_prefix(AbsPath::new(r"\\?\c:\test")?)?,
    ///         Cow::Borrowed(ForwardRelativePath::new("foo/bar.txt")?)
    ///     );
    ///     assert!(path.strip_prefix(AbsPath::new("c:/asdf")?).is_err());
    ///
    ///     let shared_path = AbsPath::new(r"\\server\share\foo\bar.txt")?;
    ///     assert_eq!(
    ///         shared_path.strip_prefix(AbsPath::new(r"\\server\share\")?)?,
    ///         Cow::Borrowed(ForwardRelativePath::new("foo/bar.txt")?)
    ///     );
    ///     assert_eq!(
    ///         shared_path.strip_prefix(AbsPath::new(r"\\server\share\foo")?)?,
    ///         Cow::Borrowed(ForwardRelativePath::new("bar.txt")?)
    ///     );
    ///     assert_eq!(
    ///         shared_path.strip_prefix(AbsPath::new(r"\\?\UNC\server\share\foo")?)?,
    ///         Cow::Borrowed(ForwardRelativePath::new("bar.txt")?)
    ///     );
    ///     assert!(shared_path.strip_prefix(AbsPath::new(r"\\server\share2\foo")?).is_err());
    ///     assert!(shared_path.strip_prefix(AbsPath::new(r"\\server\share\fo")?).is_err());
    /// }
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn strip_prefix<P: AsRef<AbsPath>>(
        &self,
        base: P,
    ) -> anyhow::Result<Cow<ForwardRelativePath>> {
        let stripped_path = self.strip_prefix_impl(base.as_ref())?;
        ForwardRelativePathNormalizer::normalize_path(stripped_path)
    }

    #[cfg(not(windows))]
    fn strip_prefix_impl(&self, base: &AbsPath) -> anyhow::Result<&Path> {
        self.0.strip_prefix(&base.0).map_err(anyhow::Error::from)
    }

    #[cfg(windows)]
    fn strip_prefix_impl(&self, base: &AbsPath) -> anyhow::Result<&Path> {
        if self.windows_prefix()? == base.windows_prefix()? {
            self.strip_windows_prefix()?
                .strip_prefix(base.strip_windows_prefix()?)
                .map_err(anyhow::Error::from)
        } else {
            Err(anyhow::anyhow!("Path is not a prefix"))
        }
    }

    /// Determines whether `base` is a prefix of `self`.
    ///
    /// ```
    /// use std::path::Path;
    /// use buck2_core::fs::paths::AbsPath;
    ///
    /// if cfg!(not(windows)) {
    ///     let abs_path = AbsPath::new("/some/foo")?;
    ///     assert!(abs_path.starts_with(AbsPath::new("/some")?));
    ///     assert!(!abs_path.starts_with(AbsPath::new("/som")?));
    /// } else {
    ///     let abs_path = AbsPath::new("c:/some/foo")?;
    ///     assert!(abs_path.starts_with(AbsPath::new("c:/some")?));
    ///     assert!(!abs_path.starts_with(AbsPath::new("c:/som")?));
    ///     assert!(abs_path.starts_with(AbsPath::new(r"\\?\C:\some")?));
    ///
    ///     let shared_path = AbsPath::new(r"\\server\share\foo\bar.txt")?;
    ///     assert!(shared_path.starts_with(AbsPath::new(r"\\server\share\")?));
    ///     assert!(shared_path.starts_with(AbsPath::new(r"\\server\share\foo")?));
    ///     assert!(shared_path.starts_with(AbsPath::new(r"\\?\UNC\server\share\foo")?));
    ///     assert!(!shared_path.starts_with(AbsPath::new(r"\\server\share2\foo")?));
    ///     assert!(!shared_path.starts_with(AbsPath::new(r"\\server\share\fo")?));
    /// }
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn starts_with<P: AsRef<AbsPath>>(&self, base: P) -> bool {
        self.starts_with_impl(base.as_ref())
    }

    #[cfg(not(windows))]
    fn starts_with_impl(&self, base: &AbsPath) -> bool {
        self.0.starts_with(&base.0)
    }

    #[cfg(windows)]
    fn starts_with_impl(&self, base: &AbsPath) -> bool {
        let prefix = self.windows_prefix();
        let base_prefix = base.windows_prefix();
        if let (Ok(prefix), Ok(base_prefix)) = (prefix, base_prefix) {
            if prefix == base_prefix {
                let stripped = self.strip_windows_prefix();
                let base_stripped = base.strip_windows_prefix();
                if let (Ok(stripped), Ok(base_stripped)) = (stripped, base_stripped) {
                    return stripped.starts_with(base_stripped);
                }
            }
        }
        false
    }

    /// Determines whether `child` is a suffix of `self`.
    /// Only considers whole path components to match.
    ///
    /// ```
    /// use std::path::Path;
    /// use buck2_core::fs::paths::AbsPath;
    ///
    /// if cfg!(not(windows)) {
    ///     let abs_path = AbsPath::new("/some/foo")?;
    ///     assert!(abs_path.ends_with("foo"));
    /// } else {
    ///     let abs_path = AbsPath::new("c:/some/foo")?;
    ///     assert!(abs_path.ends_with("foo"));
    /// }
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn ends_with<P: AsRef<Path>>(&self, child: P) -> bool {
        self.0.ends_with(&child.as_ref())
    }

    /// Build an owned `AbsPathBuf`, joined with the given path and normalized.
    ///
    /// ```
    ///
    /// use buck2_core::fs::paths::{AbsPathBuf, AbsPath};
    ///
    /// if cfg!(not(windows)) {
    ///     assert_eq!(
    ///         AbsPathBuf::from("/foo/baz.txt".into())?,
    ///         AbsPath::new("/foo/bar")?.join_normalized("../baz.txt")?
    ///     );
    ///
    ///     assert_eq!(
    ///         AbsPath::new("/foo")?.join_normalized("../../baz.txt").is_err(),
    ///         true
    ///     );
    /// } else {
    ///     assert_eq!(
    ///         AbsPathBuf::from("c:/foo/baz.txt".into())?,
    ///         AbsPath::new("c:/foo/bar")?.join_normalized("../baz.txt")?
    ///     );
    ///
    ///     assert_eq!(
    ///         AbsPath::new("c:/foo")?.join_normalized("../../baz.txt").is_err(),
    ///         true
    ///     );
    /// }
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn join_normalized<P: AsRef<RelativePath>>(&self, path: P) -> anyhow::Result<AbsPathBuf> {
        let mut stack = Vec::new();
        for c in self
            .0
            .components()
            .chain(path.as_ref().components().map(|c| match c {
                relative_path::Component::Normal(s) => std::path::Component::Normal(OsStr::new(s)),
                relative_path::Component::CurDir => std::path::Component::CurDir,
                relative_path::Component::ParentDir => std::path::Component::ParentDir,
            }))
        {
            match c {
                std::path::Component::Normal(_) => stack.push(c),
                std::path::Component::Prefix(_) => stack.push(c),
                std::path::Component::RootDir => stack.push(c),
                std::path::Component::CurDir => {}
                std::path::Component::ParentDir => {
                    if stack.pop().is_none() {
                        return Err(anyhow::anyhow!(PathNormalizationError::OutOfBounds(
                            self.as_os_str().into(),
                            path.as_ref().as_str().into(),
                        )));
                    }
                }
            }
        }
        let path_buf = stack.iter().collect::<PathBuf>();

        AbsPathBuf::try_from(path_buf)
    }

    /// Convert to an owned [`AbsPathBuf`].
    pub fn to_buf(&self) -> AbsPathBuf {
        self.to_owned()
    }

    #[cfg(windows)]
    /// Get Windows path prefix which is either disk drive letter, device or UNC name.
    ///
    /// ```
    /// use buck2_core::fs::paths::AbsPath;
    ///
    /// assert_eq!("D", AbsPath::new("d:/foo/bar")?.windows_prefix()?);
    /// assert_eq!("D", AbsPath::new(r"D:\foo\bar")?.windows_prefix()?);
    /// assert_eq!("E", AbsPath::new(r"\\?\E:\foo\bar")?.windows_prefix()?);
    /// assert_eq!("server\\share", AbsPath::new(r"\\server\share")?.windows_prefix()?);
    /// assert_eq!("server\\share", AbsPath::new(r"\\server\share\foo\bar")?.windows_prefix()?);
    /// assert_eq!("server\\share", AbsPath::new(r"\\?\UNC\server\share")?.windows_prefix()?);
    /// assert_eq!("COM42", AbsPath::new(r"\\.\COM42")?.windows_prefix()?);
    /// assert_eq!("COM42", AbsPath::new(r"\\.\COM42\foo\bar")?.windows_prefix()?);
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn windows_prefix(&self) -> anyhow::Result<OsString> {
        use std::os::windows::ffi::OsStringExt;
        use std::path::Prefix;

        match self
            .0
            .components()
            .next()
            .ok_or_else(|| anyhow::anyhow!("AbsPath is empty."))?
        {
            std::path::Component::Prefix(prefix_component) => match prefix_component.kind() {
                Prefix::Disk(disk) | Prefix::VerbatimDisk(disk) => {
                    Ok(OsString::from_wide(&[disk.into()]))
                }
                Prefix::UNC(server, share) | Prefix::VerbatimUNC(server, share) => {
                    let mut server = server.to_owned();
                    server.push("\\");
                    server.push(share);
                    Ok(server)
                }
                Prefix::DeviceNS(device) => Ok(device.to_owned()),
                prefix => Err(anyhow::anyhow!("Unknown prefix kind: {:?}.", prefix)),
            },
            _ => Err(anyhow::anyhow!("AbsPath doesn't have prefix.")),
        }
    }

    #[cfg(windows)]
    /// Strip Windows path prefix which is either disk drive letter, device or UNC name.
    ///
    /// ```
    /// use std::path::Path;
    /// use buck2_core::fs::paths::AbsPath;
    ///
    /// assert_eq!(Path::new(""), AbsPath::new("C:/")?.strip_windows_prefix()?);
    /// assert_eq!(Path::new(""), AbsPath::new("C:\\")?.strip_windows_prefix()?);
    /// assert_eq!(Path::new("foo/bar"), AbsPath::new("d:/foo/bar")?.strip_windows_prefix()?);
    /// assert_eq!(Path::new("foo\\bar"), AbsPath::new(r"D:\foo\bar")?.strip_windows_prefix()?);
    /// assert_eq!(Path::new("foo\\bar"), AbsPath::new(r"\\?\D:\foo\bar")?.strip_windows_prefix()?);
    /// assert_eq!(Path::new("path"), AbsPath::new(r"\\server\share\path")?.strip_windows_prefix()?);
    /// assert_eq!(Path::new("path"), AbsPath::new(r"\\?\UNC\server\share\path")?.strip_windows_prefix()?);
    /// assert_eq!(Path::new("abc"), AbsPath::new(r"\\.\COM42\abc")?.strip_windows_prefix()?);
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn strip_windows_prefix(&self) -> anyhow::Result<&Path> {
        let mut iter = self.0.iter();
        let prefix = iter
            .next()
            .ok_or_else(|| anyhow::anyhow!("AbsPath is empty."))?;
        let mut prefix = prefix.to_owned();
        // Strip leading path separator as well.
        if let Some(component) = iter.next() {
            prefix.push(component);
        }
        Ok(self.0.strip_prefix(&prefix)?)
    }

    pub fn as_path(&self) -> &Path {
        Path::new(&self.0)
    }
}

impl AbsPathBuf {
    pub fn new(path: PathBuf) -> anyhow::Result<AbsPathBuf> {
        // Validate.
        AbsPath::new(&path)?;
        Ok(AbsPathBuf(path))
    }

    pub fn unchecked_new(s: String) -> Self {
        Self(PathBuf::from(s))
    }

    pub fn into_path_buf(self) -> PathBuf {
        self.0
    }

    pub fn from(s: String) -> anyhow::Result<Self> {
        AbsPathBuf::try_from(s)
    }

    /// Creates a new 'AbsPathBuf' with a given capacity used to create the internal
    /// 'String'. See 'with_capacity' defined on 'PathBuf'
    pub fn with_capacity<P: AsRef<AbsPath>>(cap: usize, base: P) -> Self {
        let mut ret = Self(PathBuf::with_capacity(cap));
        ret.0.push(&base.as_ref().0);

        ret
    }

    /// Returns the capacity of the underlying 'PathBuf'
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    /// Invokes 'reserve' on the underlying 'PathBuf'
    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional)
    }

    /// Invokes 'shrink_to_fit' on the underlying 'PathBuf'
    pub fn shrink_to_fit(&mut self) {
        self.0.shrink_to_fit()
    }

    /// Invokes 'shrink_to' on the underlying 'PathBuf'
    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.0.shrink_to(min_capacity)
    }

    /// Pushes a `ForwardRelativePath` to the existing buffer
    /// ```
    ///
    /// use buck2_core::fs::paths::{ForwardRelativePath, AbsPathBuf};
    ///
    /// let mut path = AbsPathBuf::unchecked_new("/foo".to_owned());
    /// path.push(ForwardRelativePath::unchecked_new("bar"));
    ///
    /// assert_eq!(AbsPathBuf::unchecked_new("/foo/bar".to_owned()), path);
    ///
    /// path.push(ForwardRelativePath::unchecked_new("more/file.rs"));
    /// assert_eq!(AbsPathBuf::unchecked_new("/foo/bar/more/file.rs".to_owned()), path);
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn push<P: AsRef<ForwardRelativePath>>(&mut self, path: P) {
        if cfg!(windows) {
            self.0.push(path.as_ref().as_str().replace('/', "\\"))
        } else {
            self.0.push(path.as_ref().as_str())
        }
    }

    /// Pushes a `RelativePath` to the existing buffer, normalizing it.
    /// Note that this does not visit the filesystem to resolve `..`s. Instead, it cancels out the
    /// components directly, similar to `join_normalized`.
    /// ```
    ///
    /// use buck2_core::fs::paths::{RelativePath, AbsPathBuf};
    ///
    /// let mut path = AbsPathBuf::unchecked_new("/foo".to_owned());
    /// path.push_normalized(RelativePath::new("bar"))?;
    ///
    /// assert_eq!(AbsPathBuf::unchecked_new("/foo/bar".to_owned()), path);
    ///
    /// path.push_normalized(RelativePath::new("more/file.rs"))?;
    /// assert_eq!(AbsPathBuf::unchecked_new("/foo/bar/more/file.rs".to_owned()), path);
    ///
    /// path.push_normalized(RelativePath::new("../other.rs"))?;
    /// assert_eq!(AbsPathBuf::unchecked_new("/foo/bar/more/other.rs".to_owned()), path);
    ///
    /// path.push_normalized(RelativePath::new(".."))?;
    /// assert_eq!(AbsPathBuf::unchecked_new("/foo/bar/more".to_owned()), path);
    ///
    /// path.push_normalized(RelativePath::new("../.."))?;
    /// assert_eq!(AbsPathBuf::unchecked_new("/foo".to_owned()), path);
    ///
    /// path.push_normalized(RelativePath::new(".."))?;
    /// assert_eq!(AbsPathBuf::unchecked_new("/".to_owned()), path);
    ///
    /// assert!(path.push_normalized(RelativePath::new("..")).is_err());
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn push_normalized<P: AsRef<RelativePath>>(&mut self, path: P) -> anyhow::Result<()> {
        for c in path.as_ref().components() {
            match c {
                relative_path::Component::Normal(s) => {
                    self.0.push(s);
                }
                relative_path::Component::CurDir => {}
                relative_path::Component::ParentDir => {
                    if !self.0.pop() {
                        return Err(anyhow::anyhow!(PathNormalizationError::OutOfBounds(
                            self.as_os_str().into(),
                            path.as_ref().as_str().into(),
                        )));
                    }
                }
            }
        }

        Ok(())
    }
}

impl TryFrom<String> for AbsPathBuf {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    ///
    /// use buck2_core::fs::paths::AbsPathBuf;
    /// use std::convert::TryFrom;
    ///
    /// assert!(AbsPathBuf::try_from("relative/bar".to_owned()).is_err());
    ///
    /// if cfg!(not(windows)) {
    ///     assert!(AbsPathBuf::try_from("/foo/bar".to_owned()).is_ok());
    ///     assert!(AbsPathBuf::try_from("/".to_owned()).is_ok());
    ///     assert!(AbsPathBuf::try_from("/normalize/./bar".to_owned()).is_err());
    ///     assert!(AbsPathBuf::try_from("/normalize/../bar".to_owned()).is_err());
    /// } else {
    ///     assert!(AbsPathBuf::try_from("c:/foo/bar".to_owned()).is_ok());
    ///     assert!(AbsPathBuf::try_from("c:/".to_owned()).is_ok());
    ///     assert!(AbsPathBuf::try_from("c:/normalize/./bar".to_owned()).is_err());
    ///     assert!(AbsPathBuf::try_from("c:/normalize/../bar".to_owned()).is_err());
    /// }
    /// ```
    fn try_from(s: String) -> anyhow::Result<AbsPathBuf> {
        AbsPathBuf::try_from(OsString::from(s))
    }
}

impl TryFrom<OsString> for AbsPathBuf {
    type Error = anyhow::Error;

    // no allocation
    fn try_from(s: OsString) -> anyhow::Result<AbsPathBuf> {
        AbsPathBuf::try_from(PathBuf::from(s))
    }
}

impl TryFrom<PathBuf> for AbsPathBuf {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    ///
    /// use buck2_core::fs::paths::AbsPathBuf;
    /// use std::convert::TryFrom;
    /// use std::path::PathBuf;
    ///
    /// assert!(AbsPathBuf::try_from(PathBuf::from("relative/bar")).is_err());
    ///
    /// if cfg!(not(windows)) {
    ///     assert!(AbsPathBuf::try_from(PathBuf::from("/foo/bar")).is_ok());
    ///     assert!(AbsPathBuf::try_from(PathBuf::from("/")).is_ok());
    ///     assert!(AbsPathBuf::try_from(PathBuf::from("/normalize/./bar")).is_err());
    ///     assert!(AbsPathBuf::try_from(PathBuf::from("/normalize/../bar")).is_err());
    /// } else {
    ///     assert!(AbsPathBuf::try_from(PathBuf::from("c:/foo/bar")).is_ok());
    ///     assert!(AbsPathBuf::try_from(PathBuf::from("c:/")).is_ok());
    ///     assert!(AbsPathBuf::try_from(PathBuf::from("c:/normalize/./bar")).is_err());
    ///     assert!(AbsPathBuf::try_from(PathBuf::from("c:/normalize/../bar")).is_err());
    /// }
    /// ```
    fn try_from(p: PathBuf) -> anyhow::Result<AbsPathBuf> {
        AbsPathVerifier::verify(&p)?;
        Ok(AbsPathBuf(p))
    }
}

impl ToOwned for AbsPath {
    type Owned = AbsPathBuf;

    fn to_owned(&self) -> AbsPathBuf {
        AbsPathBuf(self.0.to_owned())
    }
}

impl AsRef<AbsPath> for AbsPath {
    fn as_ref(&self) -> &AbsPath {
        self
    }
}

impl AsRef<AbsPath> for AbsPathBuf {
    fn as_ref(&self) -> &AbsPath {
        AbsPath::ref_cast(&self.0)
    }
}

impl Borrow<AbsPath> for AbsPathBuf {
    fn borrow(&self) -> &AbsPath {
        self.as_ref()
    }
}

impl Deref for AbsPathBuf {
    type Target = AbsPath;

    fn deref(&self) -> &AbsPath {
        AbsPath::ref_cast(&self.0)
    }
}

/// Verifier for AbsPath to ensure the path is absolute
struct AbsPathVerifier;

impl AbsPathVerifier {
    fn verify<P: ?Sized + AsRef<Path>>(path: &P) -> anyhow::Result<()> {
        if !path.as_ref().is_absolute() {
            return Err(anyhow::anyhow!(AbsPathError::PathNotAbsolute(
                path.as_ref().to_owned()
            )));
        }

        // Path::components normalizes '.'s away so we have to manually check this via
        // string split TODO maybe we actually want to allow "."s and just
        // normalize them away entirely.
        if path
            .as_ref()
            .to_string_lossy()
            .split('/')
            .any(|part| part == ".")
        {
            return Err(anyhow::anyhow!(AbsPathError::PathNotNormalized(
                path.as_ref().to_owned()
            )));
        }
        for part in path.as_ref().components() {
            match part {
                std::path::Component::ParentDir => {
                    return Err(anyhow::anyhow!(AbsPathError::PathNotNormalized(
                        path.as_ref().to_owned()
                    )));
                }
                _ => {}
            }
        }
        Ok(())
    }
}

/// Errors from 'AbsPath' creation
#[derive(Error, Debug)]
enum AbsPathError {
    #[error("expected an absolute path but got a relative path instead: `{0}`")]
    PathNotAbsolute(PathBuf),
    #[error("expected a normalized path, but found a non-normalized path instead: `{0}`")]
    PathNotNormalized(PathBuf),
}

/// Errors from normalizing paths
#[derive(Error, Debug)]
enum PathNormalizationError {
    #[error(
        "no such path: normalizing `{}` requires the parent directory of the root of `{}`",
        .1.to_string_lossy(),
        .0.to_string_lossy()
    )]
    OutOfBounds(OsString, OsString),
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::fs::paths::AbsPath;
    use crate::fs::paths::AbsPathBuf;

    #[cfg(not(windows))]
    fn make_absolute(s: &str) -> String {
        s.to_owned()
    }

    #[cfg(windows)]
    fn make_absolute(s: &str) -> String {
        let mut abs_path = "c:".to_owned();
        abs_path.push_str(s);
        abs_path
    }

    #[test]
    fn abs_paths_work_in_maps() -> anyhow::Result<()> {
        let mut map = HashMap::new();
        let foo_string = make_absolute("/foo");
        let bar_string = make_absolute("/bar");

        let p1 = AbsPath::new(foo_string.as_str())?;
        let p2 = AbsPath::new(bar_string.as_str())?;

        map.insert(p1.to_buf(), p2.to_buf());

        assert_eq!(Some(p2), map.get(p1).map(|p| p.as_ref()));

        Ok(())
    }

    #[test]
    fn abs_path_is_comparable() -> anyhow::Result<()> {
        let foo_string = make_absolute("/foo");
        let bar_string = make_absolute("/bar");
        let path1_buf = AbsPathBuf::from(foo_string.clone())?;
        let path2_buf = AbsPathBuf::from(foo_string.clone())?;
        let path3_buf = AbsPathBuf::from(bar_string.clone())?;

        let path1 = AbsPath::new(foo_string.as_str())?;
        let path2 = AbsPath::new(foo_string.as_str())?;
        let path3 = AbsPath::new(bar_string.as_str())?;

        let str2 = foo_string.as_str();
        let str3 = bar_string.as_str();
        let str_not_abs = "ble";

        let string_not_abs = "ble".to_owned();

        assert_eq!(path1_buf, path2_buf);
        assert_ne!(path1_buf, path3_buf);

        assert_eq!(path1, path2);
        assert_ne!(path1, path3);

        assert_eq!(path1_buf, path2);
        assert_ne!(path1, path3_buf);

        assert_eq!(path1_buf, str2);
        assert_ne!(path1_buf, str3);
        assert_ne!(path1_buf, str_not_abs);

        assert_eq!(path1, str2);
        assert_ne!(path1, str3);
        assert_ne!(path1, str_not_abs);

        assert_eq!(path1_buf, foo_string);
        assert_ne!(path1_buf, bar_string);
        assert_ne!(path1_buf, string_not_abs);

        assert_eq!(path1, foo_string);
        assert_ne!(path1, bar_string);
        assert_ne!(path1, string_not_abs);

        Ok(())
    }
}
