/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//!
//! A 'Package' in Buck corresponds to the subdirectories containing the
//! repository sources that are accessible to the targets defined in the build
//! file of current package. Each 'Package' can only contain one build file.
//!
//! A 'Package' is usually the entire directory contents where directory
//! contains a build file, including all transitive subdirectories that do not
//! contain a build file themselves, i.e. excluding all sub-packages. There's
//! also a set of outputs that corresponds to building all the targets of the
//! 'Package'.
//!
//! Example:
//! ```ignore
//! fbsource
//! +-- .buck
//! +-- package1
//! |   +-- TARGETS
//! |   +-- my.java
//! +-- package2
//! |   +-- subdir     // package 2 contains this subdir
//! |   |   +-- foo.cpp
//! |   +-- bar.cpp
//! |   +-- TARGETS
//! +-- package3
//! |   +-- package4  // package 3 excludes all subdirectories rooted at package4
//! |   |   +-- a.cpp
//! |   |   +-- TARGETS
//! |   +-- faz.java
//! |   +-- TARGETS
//! ```
//!
//!

use std::{
    borrow::Borrow,
    convert::TryFrom,
    hash::{Hash, Hasher},
    ops::Deref,
    path::{Path, PathBuf},
};

use derivative::Derivative;
use derive_more::Display;
use fnv::FnvHasher;
use gazebo::prelude::*;
use internment_tweaks::{Equiv, Intern, StaticInterner};
use ref_cast::RefCast;
use relative_path::RelativePathBuf;

use crate::{
    cells::{
        paths::{CellPath, CellRelativePath},
        CellName, CellResolver,
    },
    fs::{
        paths::{
            fmt::quoted_display, FileName, FileNameBuf, ForwardRelativePath,
            ForwardRelativePathBuf, ForwardRelativePathIter, RelativePath,
        },
        project::ProjectRelativePathBuf,
    },
};

/// A 'Package' as defined above.
#[derive(Clone, Debug, Display, Eq, PartialEq, Ord, PartialOrd)]
pub struct Package(Intern<PackageData>);

/// Intern is Copy, so Clone is super cheap
impl Dupe for Package {}

/// The Hash impl for Intern will hash the pointer. That's not stable across runs and its too
/// easy for us to assume that it would be, so use a deterministic hash here.
#[allow(clippy::derive_hash_xor_eq)] // The derived PartialEq is still correct.
impl std::hash::Hash for Package {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (*self.0).hash(state)
    }
}

#[derive(Debug, Display, Ord, PartialOrd)]
struct PackageData(CellPath);

#[derive(Hash, Eq, PartialEq)]
struct PackageDataRef<'a> {
    cell: &'a CellName,
    path: &'a CellRelativePath,
}

impl<'a> From<PackageDataRef<'a>> for PackageData {
    fn from(package_data: PackageDataRef<'a>) -> Self {
        PackageData(CellPath::new(
            package_data.cell.clone(),
            package_data.path.to_buf(),
        ))
    }
}

impl PackageData {
    fn as_ref(&self) -> PackageDataRef {
        PackageDataRef {
            cell: self.0.cell(),
            path: self.0.path(),
        }
    }
}

impl PartialEq for PackageData {
    fn eq(&self, other: &Self) -> bool {
        self.as_ref() == other.as_ref()
    }
}

impl Eq for PackageData {}

impl Hash for PackageData {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl<'a> Equiv<PackageData> for PackageDataRef<'a> {
    fn equivalent(&self, key: &PackageData) -> bool {
        self == &key.as_ref()
    }
}

static INTERNER: StaticInterner<PackageData, FnvHasher> = StaticInterner::new();

impl Package {
    pub fn new(cell: &CellName, path: &CellRelativePath) -> Self {
        Self(INTERNER.intern(PackageDataRef { cell, path }))
    }

    pub fn from_cell_path(path: &CellPath) -> Self {
        Self::new(path.cell(), path.path())
    }

    pub fn cell_name(&self) -> &CellName {
        self.0.0.cell()
    }

    pub fn cell_relative_path(&self) -> &CellRelativePath {
        self.0.0.path()
    }

    pub fn to_cell_path(&self) -> CellPath {
        self.0.0.clone()
    }

    pub fn as_cell_path(&self) -> &CellPath {
        &self.0.0
    }

    pub fn join_unnormalized(&self, path: &ForwardRelativePath) -> Self {
        if path.is_empty() {
            self.dupe()
        } else {
            Package::new(
                self.as_cell_path().cell(),
                &self.as_cell_path().path().join_unnormalized(path),
            )
        }
    }
}

///
/// Resolves 'Package' to a corresponding 'ProjectRelativePath'
impl CellResolver {
    ///
    /// resolves a given 'Package' to the 'ProjectRelativePath' that points to
    /// the 'Package'
    ///
    /// ```
    /// use buck2_core::cells::{CellResolver, CellsConfigParser, CellName};
    /// use buck2_core::fs::project::{ProjectFilesystem, ProjectRelativePath, ProjectRelativePathBuf};
    /// use buck2_core::fs::paths::{ForwardRelativePathBuf, ForwardRelativePath, AbsPathBuf};
    /// use buck2_core::package::Package;
    /// use gazebo::file;
    /// use std::convert::TryFrom;
    /// use buck2_core::cells::paths::CellRelativePath;
    ///
    /// let temp = tempfile::tempdir()?;
    /// let fs = ProjectFilesystem::new(
    ///     AbsPathBuf::try_from(temp.into_path())?
    /// );
    /// let cell_config = ForwardRelativePathBuf::unchecked_new("myconfig".into());
    /// let cell_path = ProjectRelativePath::new("my/cell")?;
    ///
    /// file::create_dirs_and_write(
    ///     fs.resolve(&cell_path.join_unnormalized(&cell_config)),
    ///     "mycell=.\n",
    /// )?;
    ///
    /// let cells = CellsConfigParser::parse_cells_from_path(cell_path.to_buf(), &fs, &cell_config)?;
    ///
    /// let pkg = Package::new(
    ///     &CellName::unchecked_new("mycell".into()),
    ///     CellRelativePath::unchecked_new("somepkg"),
    /// );
    ///
    /// assert_eq!(
    ///     cells.resolve_package(&pkg)?,
    ///     ProjectRelativePathBuf::unchecked_new("my/cell/somepkg".into()),
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn resolve_package(&self, pkg: &Package) -> anyhow::Result<ProjectRelativePathBuf> {
        self.resolve_path(&pkg.0.0)
    }
}

/// A 'PackageRelativePath' is a normalized, platform-agnostic path relative to
/// the base directory of the 'Package'.
#[derive(Display, Derivative, Hash, PartialEq, Eq, PartialOrd, Ord, RefCast)]
#[derivative(Debug)]
#[repr(transparent)]
pub struct PackageRelativePath(
    #[derivative(Debug(format_with = "quoted_display"))] ForwardRelativePath,
);

/// The owned version of 'PackageRelativePath'
#[derive(Clone, Display, Derivative, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[derivative(Debug)]
pub struct PackageRelativePathBuf(
    #[derivative(Debug(format_with = "quoted_display"))] ForwardRelativePathBuf,
);

impl AsRef<ForwardRelativePath> for PackageRelativePath {
    fn as_ref(&self) -> &ForwardRelativePath {
        &self.0
    }
}

impl AsRef<RelativePath> for PackageRelativePath {
    fn as_ref(&self) -> &RelativePath {
        self.0.as_ref()
    }
}

impl AsRef<ForwardRelativePath> for PackageRelativePathBuf {
    fn as_ref(&self) -> &ForwardRelativePath {
        &self.0
    }
}

impl AsRef<RelativePath> for PackageRelativePathBuf {
    fn as_ref(&self) -> &RelativePath {
        self.0.as_ref()
    }
}

impl AsRef<ForwardRelativePathBuf> for PackageRelativePathBuf {
    fn as_ref(&self) -> &ForwardRelativePathBuf {
        &self.0
    }
}

impl PackageRelativePath {
    pub fn unchecked_new<S: ?Sized + AsRef<str>>(s: &S) -> &Self {
        PackageRelativePath::ref_cast(ForwardRelativePath::unchecked_new(s))
    }

    /// Creates an 'PackageRelativePath' if the given path represents a forward,
    /// normalized relative path, otherwise error.
    ///
    /// ```
    /// use buck2_core::package::PackageRelativePath;
    /// use std::path::Path;
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
    pub fn new<P: ?Sized + AsRef<Path>>(p: &P) -> anyhow::Result<&PackageRelativePath> {
        Ok(PackageRelativePath::ref_cast(ForwardRelativePath::new(
            p.as_ref(),
        )?))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Creates an owned 'PackageRelativePathBuf' with path adjoined to self.
    ///
    /// ```
    /// use std::path::Path;
    /// use buck2_core::fs::paths::ForwardRelativePath;
    /// use buck2_core::package::{PackageRelativePathBuf, PackageRelativePath};
    ///
    /// let path = PackageRelativePath::new("foo/bar")?;
    /// let other = ForwardRelativePath::new("baz")?;
    /// assert_eq!(PackageRelativePathBuf::unchecked_new("foo/bar/baz".to_owned()), path.join_unnormalized(other));
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn join_unnormalized<P: AsRef<ForwardRelativePath>>(
        &self,
        path: P,
    ) -> PackageRelativePathBuf {
        PackageRelativePathBuf(self.0.join_unnormalized(path.as_ref()))
    }

    /// Returns a relative path of the parent directory
    ///
    /// ```
    /// use buck2_core::package::PackageRelativePath;
    ///
    /// assert_eq!(
    ///     Some(PackageRelativePath::new("foo")?),
    ///     PackageRelativePath::new("foo/bar")?.parent()
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
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
    /// use buck2_core::package::PackageRelativePath;
    /// use buck2_core::fs::paths::FileName;
    ///
    /// assert_eq!(Some(FileName::unchecked_new("bin")), PackageRelativePath::new("usr/bin")?.file_name());
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn file_name(&self) -> Option<&FileName> {
        self.0.file_name()
    }

    /// Returns a 'ForwardRelativePath' that, when joined onto `base`, yields
    /// `self`. Error if `base` is not a prefix of `self` or the returned
    /// path is not a 'ForwardRelativePath'
    ///
    /// ```
    /// use buck2_core::fs::paths::ForwardRelativePath;
    /// use buck2_core::package::PackageRelativePath;
    ///
    /// let path = PackageRelativePath::new("test/haha/foo.txt")?;
    ///
    /// assert_eq!(
    ///     path.strip_prefix(PackageRelativePath::new("test")?)?,
    ///     ForwardRelativePath::new("haha/foo.txt")?
    /// );
    /// assert_eq!(path.strip_prefix(PackageRelativePath::new("asdf")?).is_err(), true);
    ///
    /// # anyhow::Ok(())
    /// ```
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
    ///
    /// use buck2_core::package::PackageRelativePath;
    ///
    /// let path = PackageRelativePath::new("some/foo")?;
    ///
    /// assert!(path.starts_with(PackageRelativePath::new("some")?));
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn starts_with<P: AsRef<PackageRelativePath>>(&self, base: P) -> bool {
        self.0.starts_with(&base.as_ref().0)
    }

    /// Determines whether `child` is a suffix of `self`.
    /// Only considers whole path components to match.
    ///
    /// ```
    /// use std::path::Path;
    /// use buck2_core::fs::paths::ForwardRelativePath;
    /// use buck2_core::package::PackageRelativePath;
    ///
    /// let path = PackageRelativePath::new("some/foo")?;
    ///
    /// assert!(path.ends_with(ForwardRelativePath::new("foo").unwrap()));
    ///
    /// # anyhow::Ok(())
    /// ```
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
    /// use buck2_core::package::PackageRelativePath;
    ///
    /// let path = PackageRelativePath::new("foo.rs")?;
    ///
    /// assert_eq!(Some("foo"), path.file_stem());
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn file_stem(&self) -> Option<&str> {
        self.0.file_stem()
    }

    /// Extracts the extension of [`self.file_name`], if possible.
    ///
    /// ```
    ///
    /// use buck2_core::package::PackageRelativePath;
    ///
    /// assert_eq!(Some("rs"), PackageRelativePath::new("hi/foo.rs")?.extension());
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn extension(&self) -> Option<&str> {
        self.0.extension()
    }

    /// Iterator over the components of this path
    ///
    /// ```
    /// use buck2_core::package::PackageRelativePath;
    /// use buck2_core::fs::paths::FileName;
    ///
    /// let p = PackageRelativePath::new("foo/bar/baz")?;
    /// let mut it = p.iter();
    ///
    /// assert_eq!(
    ///     it.next(),
    ///     Some(FileName::unchecked_new("foo"))
    /// );
    /// assert_eq!(
    ///     it.next(),
    ///     Some(FileName::unchecked_new("bar"))
    /// );
    /// assert_eq!(
    ///     it.next(),
    ///     Some(FileName::unchecked_new("baz"))
    /// );
    /// assert_eq!(
    ///     it.next(),
    ///     None
    /// );
    ///
    /// # anyhow::Ok(())
    /// ```
    pub fn iter(&self) -> ForwardRelativePathIter {
        self.0.iter()
    }

    pub fn to_buf(&self) -> PackageRelativePathBuf {
        self.to_owned()
    }
}

impl<'a> From<&'a ForwardRelativePath> for &'a PackageRelativePath {
    ///
    /// ```
    ///
    /// use buck2_core::package::PackageRelativePath;
    /// use buck2_core::fs::paths::ForwardRelativePath;
    /// use std::convert::From;
    ///
    /// let f = ForwardRelativePath::new("foo")?;
    ///
    /// assert_eq!(<&PackageRelativePath>::from(f), PackageRelativePath::new("foo")?);
    ///
    /// # anyhow::Ok(())
    /// ```
    fn from(p: &'a ForwardRelativePath) -> &'a PackageRelativePath {
        PackageRelativePath::ref_cast(p)
    }
}

impl PackageRelativePathBuf {
    pub fn unchecked_new(s: String) -> Self {
        Self(ForwardRelativePathBuf::unchecked_new(s))
    }

    /// Creates a new 'PackageRelativePathBuf' with a given capacity used to create the internal
    /// 'String'. See 'with_capacity' defined on 'ForwardRelativePathBuf'
    pub fn with_capacity(cap: usize) -> Self {
        Self(ForwardRelativePathBuf::with_capacity(cap))
    }

    /// Returns the capacity of the underlying 'ForwardRelativePathBuf'
    pub fn capacity(&self) -> usize {
        self.0.capacity()
    }

    /// Invokes 'reserve' on the underlying 'ForwardRelativePathBuf'
    pub fn reserve(&mut self, additional: usize) {
        self.0.reserve(additional)
    }

    /// Invokes 'shrink_to_fit' on the underlying 'ForwardRelativePathBuf'
    pub fn shrink_to_fit(&mut self) {
        self.0.shrink_to_fit()
    }

    /// Invokes 'shrink_to' on the underlying 'String'
    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.0.shrink_to(min_capacity)
    }

    /// Pushes a `ForwardRelativePath` to the existing buffer
    pub fn push_unnormalized<P: AsRef<ForwardRelativePath>>(&mut self, path: P) {
        self.0.push_unnormalized(path)
    }

    /// Pushes a `RelativePath` to the existing buffer, normalizing it
    pub fn push_normalized<P: AsRef<RelativePath>>(&mut self, path: P) -> anyhow::Result<()> {
        self.0.push_normalized(path)
    }
}

impl From<ForwardRelativePathBuf> for PackageRelativePathBuf {
    fn from(p: ForwardRelativePathBuf) -> Self {
        Self(p)
    }
}

impl From<PackageRelativePathBuf> for ForwardRelativePathBuf {
    fn from(p: PackageRelativePathBuf) -> Self {
        p.0
    }
}

impl From<FileNameBuf> for PackageRelativePathBuf {
    fn from(n: FileNameBuf) -> Self {
        PackageRelativePathBuf::unchecked_new(n.into_inner())
    }
}

impl<'a> TryFrom<&'a str> for &'a PackageRelativePath {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    ///
    /// use buck2_core::package::PackageRelativePath;
    /// use std::convert::TryFrom;
    /// use buck2_core::fs::paths::ForwardRelativePath;
    ///
    /// assert!(<&PackageRelativePath>::try_from("foo/bar").is_ok());
    /// assert!(<&PackageRelativePath>::try_from("").is_ok());
    /// assert!(<&PackageRelativePath>::try_from("/abs/bar").is_err());
    /// assert!(<&PackageRelativePath>::try_from("normalize/./bar").is_err());
    /// assert!(<&PackageRelativePath>::try_from("normalize/../bar").is_err());
    /// ```
    fn try_from(s: &'a str) -> anyhow::Result<&'a PackageRelativePath> {
        Ok(PackageRelativePath::ref_cast(ForwardRelativePath::new(s)?))
    }
}

impl<'a> TryFrom<&'a RelativePath> for &'a PackageRelativePath {
    type Error = anyhow::Error;

    /// no allocation conversion
    ///
    /// ```
    ///
    /// use buck2_core::package::PackageRelativePath;
    /// use std::convert::TryFrom;
    /// use buck2_core::fs::paths::RelativePath;
    ///
    /// assert!(<&PackageRelativePath>::try_from(RelativePath::new("foo/bar")).is_ok());
    /// assert!(<&PackageRelativePath>::try_from(RelativePath::new("")).is_ok());
    /// assert!(<&PackageRelativePath>::try_from(RelativePath::new("normalize/./bar")).is_err());
    /// assert!(<&PackageRelativePath>::try_from(RelativePath::new("normalize/../bar")).is_err());
    /// ```
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
    ///
    /// use buck2_core::package::PackageRelativePathBuf;
    /// use std::convert::TryFrom;
    ///
    /// assert!(PackageRelativePathBuf::try_from("foo/bar".to_owned()).is_ok());
    /// assert!(PackageRelativePathBuf::try_from("".to_owned()).is_ok());
    /// assert!(PackageRelativePathBuf::try_from("/abs/bar".to_owned()).is_err());
    /// assert!(PackageRelativePathBuf::try_from("normalize/./bar".to_owned()).is_err());
    /// assert!(PackageRelativePathBuf::try_from("normalize/../bar".to_owned()).is_err());
    /// ```
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
    /// use buck2_core::package::PackageRelativePathBuf;
    /// use buck2_core::fs::paths::RelativePathBuf;
    /// use std::convert::TryFrom;
    ///
    /// assert!(PackageRelativePathBuf::try_from(RelativePathBuf::from("foo/bar")).is_ok());
    /// assert!(PackageRelativePathBuf::try_from(RelativePathBuf::from("")).is_ok());
    /// assert!(PackageRelativePathBuf::try_from(RelativePathBuf::from("normalize/./bar")).is_err());
    /// assert!(PackageRelativePathBuf::try_from(RelativePathBuf::from("normalize/../bar")).is_err());
    /// ```
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
    ///
    /// use buck2_core::package::PackageRelativePathBuf;
    /// use std::convert::TryFrom;
    /// use std::path::PathBuf;
    ///
    /// assert!(PackageRelativePathBuf::try_from(PathBuf::from("foo/bar")).is_ok());
    /// assert!(PackageRelativePathBuf::try_from(PathBuf::from("")).is_ok());
    /// assert!(PackageRelativePathBuf::try_from(PathBuf::from("/abs/bar")).is_err());
    /// assert!(PackageRelativePathBuf::try_from(PathBuf::from("normalize/./bar")).is_err());
    /// assert!(PackageRelativePathBuf::try_from(PathBuf::from("normalize/../bar")).is_err());
    /// ```
    fn try_from(p: PathBuf) -> anyhow::Result<PackageRelativePathBuf> {
        Ok(PackageRelativePathBuf::from(
            ForwardRelativePathBuf::try_from(p)?,
        ))
    }
}

impl ToOwned for PackageRelativePath {
    type Owned = PackageRelativePathBuf;

    fn to_owned(&self) -> PackageRelativePathBuf {
        PackageRelativePathBuf(self.0.to_owned())
    }
}

impl AsRef<PackageRelativePath> for PackageRelativePath {
    fn as_ref(&self) -> &PackageRelativePath {
        self
    }
}

impl AsRef<PackageRelativePath> for PackageRelativePathBuf {
    fn as_ref(&self) -> &PackageRelativePath {
        PackageRelativePath::ref_cast(&self.0)
    }
}

impl Borrow<PackageRelativePath> for PackageRelativePathBuf {
    fn borrow(&self) -> &PackageRelativePath {
        self.as_ref()
    }
}

impl Deref for PackageRelativePathBuf {
    type Target = PackageRelativePath;

    fn deref(&self) -> &PackageRelativePath {
        PackageRelativePath::ref_cast(&self.0)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::package::{PackageRelativePath, PackageRelativePathBuf};

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

pub mod testing {
    use crate::{
        cells::{paths::CellRelativePathBuf, CellName},
        package::Package,
    };

    pub trait PackageExt {
        fn testing_new(cell: &str, path: &str) -> Self;
    }

    impl PackageExt for Package {
        fn testing_new(cell: &str, path: &str) -> Self {
            Self::new(
                &CellName::unchecked_new(cell.into()),
                &CellRelativePathBuf::unchecked_new(path.into()),
            )
        }
    }
}
