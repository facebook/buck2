/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;
use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;

use allocative::Allocative;
use buck2_util::arc_str::ArcS;
use buck2_util::arc_str::StringInside;
use gazebo::transmute;
use ref_cast::RefCast;
use relative_path::RelativePath;
use relative_path::RelativePathBuf;

use crate::fs::paths::file_name::FileName;
use crate::fs::paths::forward_rel_path::ForwardRelativePath;
use crate::fs::paths::forward_rel_path::ForwardRelativePathBuf;
use crate::fs::paths::forward_rel_path::ForwardRelativePathIter;
use crate::package::quoted_display;

/// A 'PackageRelativePath' is a normalized, platform-agnostic path relative to
/// the base directory of the 'Package'.
#[derive(
    derive_more::Display,
    derivative::Derivative,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    RefCast,
    Allocative
)]
#[derivative(Debug)]
#[repr(transparent)]
pub struct PackageRelativePath(
    // Note we transmute between `PackageRelativePath` and `ForwardRelativePath`.
    #[derivative(Debug(format_with = "quoted_display"))] ForwardRelativePath,
);

/// The owned version of 'PackageRelativePath'
#[derive(
    Clone,
    derive_more::Display,
    derivative::Derivative,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Allocative
)]
#[derivative(Debug)]
pub struct PackageRelativePathBuf(
    #[derivative(Debug(format_with = "quoted_display"))] ForwardRelativePathBuf,
);

impl StringInside for PackageRelativePath {
    #[inline]
    fn as_str(wrapper: &PackageRelativePath) -> &str {
        wrapper.as_str()
    }

    #[inline]
    fn from_str(s: &str) -> &PackageRelativePath {
        PackageRelativePath::unchecked_new(s)
    }
}

impl AsRef<ForwardRelativePath> for PackageRelativePath {
    #[inline]
    fn as_ref(&self) -> &ForwardRelativePath {
        &self.0
    }
}

impl AsRef<RelativePath> for PackageRelativePath {
    #[inline]
    fn as_ref(&self) -> &RelativePath {
        self.0.as_ref()
    }
}

impl AsRef<ForwardRelativePath> for PackageRelativePathBuf {
    #[inline]
    fn as_ref(&self) -> &ForwardRelativePath {
        &self.0
    }
}

impl AsRef<RelativePath> for PackageRelativePathBuf {
    #[inline]
    fn as_ref(&self) -> &RelativePath {
        self.0.as_ref()
    }
}

impl AsRef<ForwardRelativePathBuf> for PackageRelativePathBuf {
    #[inline]
    fn as_ref(&self) -> &ForwardRelativePathBuf {
        &self.0
    }
}

impl Clone for Box<PackageRelativePath> {
    #[inline]
    fn clone(&self) -> Self {
        Self::from(&**self)
    }
}

impl PackageRelativePath {
    #[inline]
    pub fn unchecked_new<S: ?Sized + AsRef<str>>(s: &S) -> &Self {
        PackageRelativePath::ref_cast(ForwardRelativePath::unchecked_new(s))
    }

    #[inline]
    pub fn new_box(p: Box<ForwardRelativePath>) -> Box<PackageRelativePath> {
        unsafe {
            // SAFETY: `PackageRelativePath` is a transparent wrapper around `ForwardRelativePath`.
            transmute!(Box<ForwardRelativePath>, Box<PackageRelativePath>, p)
        }
    }

    #[inline]
    pub fn empty() -> &'static PackageRelativePath {
        PackageRelativePath::unchecked_new("")
    }

    /// Creates an 'PackageRelativePath' if the given path represents a forward,
    /// normalized relative path, otherwise error.
    ///
    /// ```
    /// use std::path::Path;
    ///
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    ///
    /// assert!(PackageRelativePath::new("foo/bar").is_ok());
    /// assert!(PackageRelativePath::new("").is_ok());
    /// assert!(PackageRelativePath::new("/abs/bar").is_err());
    /// assert!(PackageRelativePath::new("normalize/./bar").is_err());
    /// assert!(PackageRelativePath::new("normalize/../bar").is_err());
    ///
    /// assert!(PackageRelativePath::new(Path::new("foo/bar")).is_ok());
    /// assert!(PackageRelativePath::new(Path::new("")).is_ok());
    /// assert!(PackageRelativePath::new(Path::new("/abs/bar")).is_err());
    /// assert!(PackageRelativePath::new(Path::new("normalize/./bar")).is_err());
    /// assert!(PackageRelativePath::new(Path::new("normalize/../bar")).is_err());
    /// ```
    #[inline]
    pub fn new<P: ?Sized + AsRef<Path>>(p: &P) -> anyhow::Result<&PackageRelativePath> {
        Ok(PackageRelativePath::ref_cast(ForwardRelativePath::new(
            p.as_ref(),
        )?))
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    #[inline]
    pub fn as_forward_rel_path(&self) -> &ForwardRelativePath {
        &self.0
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Creates an owned 'PackageRelativePathBuf' with path adjoined to self.
    ///
    /// ```
    /// use std::path::Path;
    ///
    /// use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    /// use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    ///
    /// let path = PackageRelativePath::new("foo/bar")?;
    /// let other = ForwardRelativePath::new("baz")?;
    /// assert_eq!(
    ///     PackageRelativePathBuf::unchecked_new("foo/bar/baz".to_owned()),
    ///     path.join(other)
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    #[inline]
    pub fn join<P: AsRef<ForwardRelativePath>>(&self, path: P) -> PackageRelativePathBuf {
        PackageRelativePathBuf(self.0.join(path.as_ref()))
    }

    /// Returns a relative path of the parent directory
    ///
    /// ```
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    ///
    /// assert_eq!(
    ///     Some(PackageRelativePath::new("foo")?),
    ///     PackageRelativePath::new("foo/bar")?.parent()
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    #[inline]
    pub fn parent(&self) -> Option<&PackageRelativePath> {
        self.0.parent().map(PackageRelativePath::ref_cast)
    }

    /// Returns the final component of the `PackageRelativePath`, if there is
    /// one.
    ///
    /// If the path is a normal file, this is the file name. If it's the path of
    /// a directory, this is the directory name.
    ///
    /// ```
    /// use buck2_core::fs::paths::file_name::FileName;
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    ///
    /// assert_eq!(
    ///     Some(FileName::unchecked_new("bin")),
    ///     PackageRelativePath::new("usr/bin")?.file_name()
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    #[inline]
    pub fn file_name(&self) -> Option<&FileName> {
        self.0.file_name()
    }

    /// Returns a 'ForwardRelativePath' that, when joined onto `base`, yields
    /// `self`. Error if `base` is not a prefix of `self` or the returned
    /// path is not a 'ForwardRelativePath'
    ///
    /// ```
    /// use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    ///
    /// let path = PackageRelativePath::new("test/haha/foo.txt")?;
    ///
    /// assert_eq!(
    ///     path.strip_prefix(PackageRelativePath::new("test")?)?,
    ///     ForwardRelativePath::new("haha/foo.txt")?
    /// );
    /// assert_eq!(
    ///     path.strip_prefix(PackageRelativePath::new("asdf")?)
    ///         .is_err(),
    ///     true
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    #[inline]
    pub fn strip_prefix<'a, P: ?Sized>(
        &'a self,
        base: &'a P,
    ) -> anyhow::Result<&'a ForwardRelativePath>
    where
        P: AsRef<PackageRelativePath>,
    {
        self.0.strip_prefix(&base.as_ref().0)
    }

    /// Determines whether `base` is a prefix of `self`.
    ///
    /// ```
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    ///
    /// let path = PackageRelativePath::new("some/foo")?;
    ///
    /// assert!(path.starts_with(PackageRelativePath::new("some")?));
    ///
    /// # anyhow::Ok(())
    /// ```
    #[inline]
    pub fn starts_with<P: AsRef<PackageRelativePath>>(&self, base: P) -> bool {
        self.0.starts_with(&base.as_ref().0)
    }

    /// Determines whether `child` is a suffix of `self`.
    /// Only considers whole path components to match.
    ///
    /// ```
    /// use std::path::Path;
    ///
    /// use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    ///
    /// let path = PackageRelativePath::new("some/foo")?;
    ///
    /// assert!(path.ends_with(ForwardRelativePath::new("foo").unwrap()));
    ///
    /// # anyhow::Ok(())
    /// ```
    #[inline]
    pub fn ends_with<P: AsRef<ForwardRelativePath>>(&self, child: P) -> bool {
        self.0.ends_with(child.as_ref())
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
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    ///
    /// let path = PackageRelativePath::new("foo.rs")?;
    ///
    /// assert_eq!(Some("foo"), path.file_stem());
    ///
    /// # anyhow::Ok(())
    /// ```
    #[inline]
    pub fn file_stem(&self) -> Option<&str> {
        self.0.file_stem()
    }

    /// Extracts the extension of [`self.file_name`], if possible.
    ///
    /// ```
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    ///
    /// assert_eq!(
    ///     Some("rs"),
    ///     PackageRelativePath::new("hi/foo.rs")?.extension()
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    #[inline]
    pub fn extension(&self) -> Option<&str> {
        self.0.extension()
    }

    /// Iterator over the components of this path
    ///
    /// ```
    /// use buck2_core::fs::paths::file_name::FileName;
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    ///
    /// let p = PackageRelativePath::new("foo/bar/baz")?;
    /// let mut it = p.iter();
    ///
    /// assert_eq!(it.next(), Some(FileName::unchecked_new("foo")));
    /// assert_eq!(it.next(), Some(FileName::unchecked_new("bar")));
    /// assert_eq!(it.next(), Some(FileName::unchecked_new("baz")));
    /// assert_eq!(it.next(), None);
    ///
    /// # anyhow::Ok(())
    /// ```
    #[inline]
    pub fn iter(&self) -> ForwardRelativePathIter {
        self.0.iter()
    }

    #[inline]
    pub fn to_buf(&self) -> PackageRelativePathBuf {
        self.to_owned()
    }

    #[inline]
    pub fn to_box(&self) -> Box<PackageRelativePath> {
        self.to_buf().into_box()
    }

    #[inline]
    pub fn to_arc(&self) -> ArcS<PackageRelativePath> {
        ArcS::from(self)
    }
}

impl<'a> From<&'a ForwardRelativePath> for &'a PackageRelativePath {
    ///
    /// ```
    /// use std::convert::From;
    ///
    /// use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    ///
    /// let f = ForwardRelativePath::new("foo")?;
    ///
    /// assert_eq!(
    ///     <&PackageRelativePath>::from(f),
    ///     PackageRelativePath::new("foo")?
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    #[inline]
    fn from(p: &'a ForwardRelativePath) -> &'a PackageRelativePath {
        PackageRelativePath::ref_cast(p)
    }
}

impl PackageRelativePathBuf {
    #[inline]
    pub fn unchecked_new(s: String) -> Self {
        Self(ForwardRelativePathBuf::unchecked_new(s))
    }

    #[inline]
    pub fn as_path(&self) -> &PackageRelativePath {
        self
    }

    /// Creates a new 'PackageRelativePathBuf' with a given capacity used to create the internal
    /// 'String'. See 'with_capacity' defined on 'ForwardRelativePathBuf'
    #[inline]
    pub fn with_capacity(cap: usize) -> Self {
        Self(ForwardRelativePathBuf::with_capacity(cap))
    }

    /// Returns the capacity of the underlying 'ForwardRelativePathBuf'
    #[inline]
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    /// Invokes 'reserve' on the underlying 'ForwardRelativePathBuf'
    #[inline]
    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional)
    }

    /// Invokes 'shrink_to_fit' on the underlying 'ForwardRelativePathBuf'
    #[inline]
    pub fn shrink_to_fit(&mut self) {
        self.0.shrink_to_fit()
    }

    /// Invokes 'shrink_to' on the underlying 'String'
    #[inline]
    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.0.shrink_to(min_capacity)
    }

    /// Pushes a `ForwardRelativePath` to the existing buffer
    #[inline]
    pub fn push<P: AsRef<ForwardRelativePath>>(&mut self, path: P) {
        self.0.push(path)
    }

    /// Pushes a `RelativePath` to the existing buffer, normalizing it
    #[inline]
    pub fn push_normalized<P: AsRef<RelativePath>>(&mut self, path: P) -> anyhow::Result<()> {
        self.0.push_normalized(path)
    }

    #[inline]
    pub fn into_box(self) -> Box<PackageRelativePath> {
        let s: Box<str> = self.0.into_string().into_boxed_str();
        PackageRelativePath::new_box(ForwardRelativePath::unchecked_new_box(s))
    }
}

impl<'a> From<&'a PackageRelativePath> for Box<PackageRelativePath> {
    #[inline]
    fn from(p: &'a PackageRelativePath) -> Box<PackageRelativePath> {
        let path: Box<str> = Box::from(p.as_str());
        PackageRelativePath::new_box(ForwardRelativePath::unchecked_new_box(path))
    }
}

impl From<ForwardRelativePathBuf> for PackageRelativePathBuf {
    #[inline]
    fn from(p: ForwardRelativePathBuf) -> Self {
        Self(p)
    }
}

impl From<PackageRelativePathBuf> for ForwardRelativePathBuf {
    #[inline]
    fn from(p: PackageRelativePathBuf) -> Self {
        p.0
    }
}

impl<'a> TryFrom<&'a str> for &'a PackageRelativePath {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    ///
    /// assert!(<&PackageRelativePath>::try_from("foo/bar").is_ok());
    /// assert!(<&PackageRelativePath>::try_from("").is_ok());
    /// assert!(<&PackageRelativePath>::try_from("/abs/bar").is_err());
    /// assert!(<&PackageRelativePath>::try_from("normalize/./bar").is_err());
    /// assert!(<&PackageRelativePath>::try_from("normalize/../bar").is_err());
    /// ```
    #[inline]
    fn try_from(s: &'a str) -> anyhow::Result<&'a PackageRelativePath> {
        Ok(PackageRelativePath::ref_cast(ForwardRelativePath::new(s)?))
    }
}

impl<'a> TryFrom<&'a RelativePath> for &'a PackageRelativePath {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use buck2_core::fs::paths::RelativePath;
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    ///
    /// assert!(<&PackageRelativePath>::try_from(RelativePath::new("foo/bar")).is_ok());
    /// assert!(<&PackageRelativePath>::try_from(RelativePath::new("")).is_ok());
    /// assert!(<&PackageRelativePath>::try_from(RelativePath::new("normalize/./bar")).is_err());
    /// assert!(<&PackageRelativePath>::try_from(RelativePath::new("normalize/../bar")).is_err());
    /// ```
    #[inline]
    fn try_from(s: &'a RelativePath) -> anyhow::Result<&'a PackageRelativePath> {
        Ok(PackageRelativePath::ref_cast(ForwardRelativePath::new(
            s.as_str(),
        )?))
    }
}

impl TryFrom<String> for PackageRelativePathBuf {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    /// use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    ///
    /// assert!(PackageRelativePathBuf::try_from("foo/bar".to_owned()).is_ok());
    /// assert!(PackageRelativePathBuf::try_from("".to_owned()).is_ok());
    /// assert!(PackageRelativePathBuf::try_from("/abs/bar".to_owned()).is_err());
    /// assert!(PackageRelativePathBuf::try_from("normalize/./bar".to_owned()).is_err());
    /// assert!(PackageRelativePathBuf::try_from("normalize/../bar".to_owned()).is_err());
    /// ```
    #[inline]
    fn try_from(s: String) -> anyhow::Result<PackageRelativePathBuf> {
        Ok(PackageRelativePathBuf::from(
            ForwardRelativePathBuf::try_from(s)?,
        ))
    }
}

impl TryFrom<RelativePathBuf> for PackageRelativePathBuf {
    type Error = anyhow::Error;

    /// no allocation conversion (TODO make ForwardRelativePath a no allocation
    /// conversion)
    ///
    /// ```
    /// use std::convert::TryFrom;
    ///
    /// use buck2_core::fs::paths::RelativePathBuf;
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    /// use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    ///
    /// assert!(PackageRelativePathBuf::try_from(RelativePathBuf::from("foo/bar")).is_ok());
    /// assert!(PackageRelativePathBuf::try_from(RelativePathBuf::from("")).is_ok());
    /// assert!(PackageRelativePathBuf::try_from(RelativePathBuf::from("normalize/./bar")).is_err());
    /// assert!(PackageRelativePathBuf::try_from(RelativePathBuf::from("normalize/../bar")).is_err());
    /// ```
    #[inline]
    fn try_from(p: RelativePathBuf) -> anyhow::Result<PackageRelativePathBuf> {
        Ok(PackageRelativePathBuf::from(
            ForwardRelativePathBuf::try_from(p)?,
        ))
    }
}

impl TryFrom<PathBuf> for PackageRelativePathBuf {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    /// use std::convert::TryFrom;
    /// use std::path::PathBuf;
    ///
    /// use buck2_core::package::package_relative_path::PackageRelativePath;
    /// use buck2_core::package::package_relative_path::PackageRelativePathBuf;
    ///
    /// assert!(PackageRelativePathBuf::try_from(PathBuf::from("foo/bar")).is_ok());
    /// assert!(PackageRelativePathBuf::try_from(PathBuf::from("")).is_ok());
    /// assert!(PackageRelativePathBuf::try_from(PathBuf::from("/abs/bar")).is_err());
    /// assert!(PackageRelativePathBuf::try_from(PathBuf::from("normalize/./bar")).is_err());
    /// assert!(PackageRelativePathBuf::try_from(PathBuf::from("normalize/../bar")).is_err());
    /// ```
    #[inline]
    fn try_from(p: PathBuf) -> anyhow::Result<PackageRelativePathBuf> {
        Ok(PackageRelativePathBuf::from(
            ForwardRelativePathBuf::try_from(p)?,
        ))
    }
}

impl ToOwned for PackageRelativePath {
    type Owned = PackageRelativePathBuf;

    #[inline]
    fn to_owned(&self) -> PackageRelativePathBuf {
        PackageRelativePathBuf(self.0.to_owned())
    }
}

impl AsRef<PackageRelativePath> for PackageRelativePath {
    #[inline]
    fn as_ref(&self) -> &PackageRelativePath {
        self
    }
}

impl AsRef<PackageRelativePath> for PackageRelativePathBuf {
    #[inline]
    fn as_ref(&self) -> &PackageRelativePath {
        PackageRelativePath::ref_cast(&self.0)
    }
}

impl Borrow<PackageRelativePath> for PackageRelativePathBuf {
    #[inline]
    fn borrow(&self) -> &PackageRelativePath {
        self.as_ref()
    }
}

impl Deref for PackageRelativePathBuf {
    type Target = PackageRelativePath;

    #[inline]
    fn deref(&self) -> &PackageRelativePath {
        PackageRelativePath::ref_cast(&self.0)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::package::package_relative_path::PackageRelativePath;
    use crate::package::package_relative_path::PackageRelativePathBuf;

    #[test]
    fn paths_work_in_maps() -> anyhow::Result<()> {
        let mut map = HashMap::new();

        let p1 = PackageRelativePath::new("foo")?;
        let p2 = PackageRelativePath::new("bar")?;

        map.insert(p1.to_buf(), p2.to_buf());

        assert_eq!(Some(p2), map.get(p1).map(|p| p.as_ref()));

        Ok(())
    }

    #[test]
    fn path_is_comparable() -> anyhow::Result<()> {
        let path1_buf = PackageRelativePathBuf::unchecked_new("foo".into());
        let path2_buf = PackageRelativePathBuf::unchecked_new("foo".into());
        let path3_buf = PackageRelativePathBuf::unchecked_new("bar".into());

        let path1 = PackageRelativePath::new("foo")?;
        let path2 = PackageRelativePath::new("foo")?;
        let path3 = PackageRelativePath::new("bar")?;

        let str2 = "foo";
        let str3 = "bar";
        let str_abs = "/ble";

        let string2 = "foo".to_owned();
        let string3 = "bar".to_owned();
        let string_abs = "/ble".to_owned();

        assert_eq!(path1_buf, path2_buf);
        assert_ne!(path1_buf, path3_buf);

        assert_eq!(path1, path2);
        assert_ne!(path1, path3);

        assert_eq!(path1_buf, path2);
        assert_ne!(path1, path3_buf);

        assert_eq!(path1_buf, str2);
        assert_ne!(path1_buf, str3);
        assert_ne!(path1_buf, str_abs);

        assert_eq!(path1, str2);
        assert_ne!(path1, str3);
        assert_ne!(path1, str_abs);

        assert_eq!(path1_buf, string2);
        assert_ne!(path1_buf, string3);
        assert_ne!(path1_buf, string_abs);

        assert_eq!(path1, string2);
        assert_ne!(path1, string3);
        assert_ne!(path1, string_abs);

        Ok(())
    }
}
