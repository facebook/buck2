/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Platform-agnostic, UTF-8, relative paths.
//!
//! A `RelativePath` is a `str`-backed path that always uses `/` as a separator and is *not*
//! normalized: it may contain `.` and `..` components, empty components (consecutive `/`), or even
//! backslashes — those are not separators here and are treated as part of a normal component.
//!
//! Use [`ForwardRelativePath`](crate::paths::forward_rel_path::ForwardRelativePath) when the
//! stronger "no `.`/`..`, no leading `/`" invariant is required; `RelativePath` is the looser form
//! used for things like symlink targets and import paths that need to express `..` traversal.
//!
//! FIXME(JakobDegen): this type was lifted directly from the external `relative-path` crate as a
//! starting point and has not been polished:
//!
//! - `From<String>`/`From<&str>` impls
//! - [`Self::file_name`] returns `Option<&str>`
//! - There is no `unchecked_new_box`/`as_path` parallel to the forward type.
//! - `.` is silently stripped in some places
//! - No good story for converting towards `ForwardRelativePath`
//! - The Windows-`\` translation in [`RelativePathBuf::from_system_path`] piggybacks on
//!   [`std::path::Path::components`]. That works for the simple shape but would lose information
//!   for paths that contain literal backslashes on Unix (where `\` is a normal character).

use std::borrow::Borrow;
use std::mem;
use std::ops::Deref;
use std::path::Path;
use std::path::PathBuf;

use allocative::Allocative;
use derive_more::Display;
use gazebo::transmute;
use pagable::Pagable;
use pagable::PagableSerialize;
use ref_cast::RefCastCustom;
use ref_cast::ref_cast_custom;
use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use strong_hash::StrongHash;

const SEP: char = '/';
const CURRENT_STR: &str = ".";
const PARENT_STR: &str = "..";

/// A borrowed, immutable relative path.
///
/// See the [module docs](self) for invariants.
#[derive(
    Display,
    Debug,
    Serialize,
    RefCastCustom,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Allocative,
    StrongHash,
    PagableSerialize
)]
#[repr(transparent)]
pub struct RelativePath(
    // Note we transmute between `RelativePath` and `str`.
    str,
);

/// An owned, mutable relative path. The owned counterpart to [`RelativePath`].
#[derive(
    Default, Clone, Display, Debug, Serialize, PartialEq, Eq, PartialOrd, Ord, Hash, Allocative,
    StrongHash, Pagable
)]
#[repr(transparent)]
pub struct RelativePathBuf(#[pagable(flatten_serde)] String);

impl<'de> Deserialize<'de> for RelativePathBuf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(RelativePathBuf(s))
    }
}

impl RelativePath {
    #[ref_cast_custom]
    const fn ref_cast(s: &str) -> &RelativePath;

    /// Wraps a string slice as a `RelativePath`. Infallible — any string is a
    /// valid relative path (see the [module docs](self)).
    #[inline]
    pub fn unchecked_new(s: &str) -> &RelativePath {
        RelativePath::ref_cast(s)
    }

    /// The empty relative path.
    #[inline]
    pub const fn empty() -> &'static RelativePath {
        RelativePath::ref_cast("")
    }

    #[inline]
    pub fn as_str(&self) -> &str {
        &self.0
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Iterator over the components of this path. `.` and `..` are surfaced as
    /// [`Component::CurDir`] / [`Component::ParentDir`]; empty components
    /// (consecutive `/`s) are skipped.
    ///
    /// ```
    /// use buck2_fs::paths::RelativePath;
    /// use buck2_fs::paths::relative_path::Component;
    ///
    /// let path = RelativePath::unchecked_new("foo/../bar/./baz");
    /// let components: Vec<_> = path.components().collect();
    /// assert_eq!(
    ///     vec![
    ///         Component::Normal("foo"),
    ///         Component::ParentDir,
    ///         Component::Normal("bar"),
    ///         Component::CurDir,
    ///         Component::Normal("baz"),
    ///     ],
    ///     components,
    /// );
    /// ```
    #[inline]
    pub fn components(&self) -> Components<'_> {
        Components { source: &self.0 }
    }

    /// Iterator over the components of this path as `&str` slices. Like
    /// [`Self::components`] but [`Component::as_str`] is applied to each
    /// yielded item.
    #[inline]
    pub fn iter(&self) -> Iter<'_> {
        Iter {
            inner: self.components(),
        }
    }

    /// Returns the final component of the path, if it is a normal name.
    /// Trailing `.` components are skipped; if the final non-`.` component is
    /// `..`, returns `None`.
    pub fn file_name(&self) -> Option<&str> {
        let mut it = self.components();
        while let Some(c) = it.next_back() {
            return match c {
                Component::CurDir => continue,
                Component::Normal(name) => Some(name),
                Component::ParentDir => None,
            };
        }
        None
    }

    /// Returns the parent path, or `None` for the empty path.
    pub fn parent(&self) -> Option<&RelativePath> {
        if self.0.is_empty() {
            return None;
        }
        let mut it = self.components();
        while let Some(Component::CurDir) = it.next_back() {}
        Some(it.as_relative_path())
    }

    /// Joins `path` onto `self`, returning a new owned path. Equivalent to
    /// `self.to_owned()` followed by `RelativePathBuf::push(path)`.
    ///
    /// ```
    /// use buck2_fs::paths::RelativePath;
    ///
    /// assert_eq!(
    ///     "foo/bar/baz",
    ///     RelativePath::unchecked_new("foo/bar").join("baz").as_str()
    /// );
    /// ```
    pub fn join<P: AsRef<RelativePath>>(&self, path: P) -> RelativePathBuf {
        let mut out = self.to_owned();
        out.push(path);
        out
    }

    /// Builds an owned `RelativePathBuf` that is `self` followed by `path`,
    /// with `.` and `..` components cancelled. Leading `..` that escape the
    /// root are preserved (e.g. `"../foo".join_normalized("../bar")` →
    /// `"../../bar"`).
    ///
    /// This is a purely textual normalization — it does not consult the
    /// filesystem, so on filesystems with symlinks the result may not refer
    /// to the same location as the unnormalized join.
    ///
    /// ```
    /// use buck2_fs::paths::RelativePath;
    ///
    /// assert_eq!(
    ///     "foo/baz.txt",
    ///     RelativePath::unchecked_new("foo/bar")
    ///         .join_normalized(RelativePath::unchecked_new("../baz.txt"))
    ///         .as_str()
    /// );
    /// assert_eq!(
    ///     "../foo/baz.txt",
    ///     RelativePath::unchecked_new("../foo/bar")
    ///         .join_normalized(RelativePath::unchecked_new("../baz.txt"))
    ///         .as_str()
    /// );
    /// ```
    pub fn join_normalized<P: AsRef<RelativePath>>(&self, path: P) -> RelativePathBuf {
        let mut buf = RelativePathBuf::empty();
        relative_traversal(&mut buf, self.components());
        relative_traversal(&mut buf, path.as_ref().components());
        buf
    }

    /// Returns a normalized form of `self`, with `.` removed and `..`
    /// cancelled against the preceding component where possible. Leading
    /// `..` components that cannot be cancelled remain.
    ///
    /// ```
    /// use buck2_fs::paths::RelativePath;
    ///
    /// assert_eq!(
    ///     "../foo/baz.txt",
    ///     RelativePath::unchecked_new("../foo/./bar/../baz.txt")
    ///         .normalize()
    ///         .as_str()
    /// );
    /// assert_eq!("", RelativePath::unchecked_new(".").normalize().as_str());
    /// ```
    pub fn normalize(&self) -> RelativePathBuf {
        let mut buf = RelativePathBuf::with_capacity(self.0.len());
        relative_traversal(&mut buf, self.components());
        buf
    }

    /// Constructs a relative path from `self` to `path`. If `self` contains
    /// unnamed (`..`) components, returns an empty path because there's no way
    /// to express the path back to those unnamed positions.
    ///
    /// ```
    /// use buck2_fs::paths::RelativePath;
    ///
    /// assert_eq!(
    ///     "../../e/f",
    ///     RelativePath::unchecked_new("a/b/c/d")
    ///         .relative(RelativePath::unchecked_new("a/b/e/f"))
    ///         .as_str()
    /// );
    /// ```
    pub fn relative<P: AsRef<RelativePath>>(&self, path: P) -> RelativePathBuf {
        let mut from = RelativePathBuf::with_capacity(self.0.len());
        let mut to = RelativePathBuf::with_capacity(path.as_ref().0.len());

        relative_traversal(&mut from, self.components());
        relative_traversal(&mut to, path.as_ref().components());

        let mut it_from = from.components();
        let mut it_to = to.components();

        let (lead_from, lead_to) = loop {
            match (it_from.next(), it_to.next()) {
                (Some(f), Some(t)) if f == t => continue,
                (f, t) => break (f, t),
            }
        };

        // After `relative_traversal`, any leading `..` in `from` is a positional
        // marker we can't name our way back to.
        if lead_from == Some(Component::ParentDir) {
            return RelativePathBuf::empty();
        }

        let head = lead_from.into_iter().chain(it_from);
        let tail = lead_to.into_iter().chain(it_to);

        let mut buf = RelativePathBuf::with_capacity(usize::max(from.0.len(), to.0.len()));
        for c in head.map(|_| Component::ParentDir).chain(tail) {
            buf.push(RelativePath::unchecked_new(c.as_str()));
        }
        buf
    }

    /// Joins this relative path onto `base`, producing a [`PathBuf`] in the
    /// native path separator. Empty `base` yields just the relative
    /// components.
    ///
    /// ```
    /// use std::path::Path;
    ///
    /// use buck2_fs::paths::RelativePath;
    ///
    /// let path = RelativePath::unchecked_new("foo/bar").to_path("");
    /// assert_eq!(Path::new("foo/bar"), path);
    /// ```
    pub fn to_path<P: AsRef<Path>>(&self, base: P) -> PathBuf {
        let mut p = base.as_ref().to_path_buf().into_os_string();
        for c in self.components() {
            if !p.is_empty() {
                p.push(std::path::MAIN_SEPARATOR.encode_utf8(&mut [0u8; 4]));
            }
            p.push(c.as_str());
        }
        PathBuf::from(p)
    }
}

impl RelativePathBuf {
    /// Creates a new, empty `RelativePathBuf`.
    #[inline]
    pub fn empty() -> RelativePathBuf {
        RelativePathBuf(String::new())
    }

    #[inline]
    fn with_capacity(cap: usize) -> RelativePathBuf {
        RelativePathBuf(String::with_capacity(cap))
    }

    /// Converts a system [`Path`] into a `RelativePathBuf`.
    ///
    /// Iterates the path's native components, rejecting absolute components
    /// (Windows drive prefixes and root directories) and requiring UTF-8.
    /// On Windows, this naturally converts `\` separators to `/` because
    /// [`Path::components`] splits on the platform separator. `.` components
    /// are stripped; `..` components are preserved literally.
    ///
    /// ```
    /// use std::path::Path;
    ///
    /// use buck2_fs::paths::RelativePathBuf;
    ///
    /// assert_eq!(
    ///     "foo/bar",
    ///     RelativePathBuf::from_system_path(Path::new("foo/bar"))
    ///         .unwrap()
    ///         .as_str()
    /// );
    /// assert!(RelativePathBuf::from_system_path(Path::new("/abs")).is_err());
    /// ```
    pub fn from_system_path<P: AsRef<Path>>(
        path: P,
    ) -> Result<RelativePathBuf, FromSystemPathError> {
        use std::path::Component::CurDir;
        use std::path::Component::Normal;
        use std::path::Component::ParentDir;
        use std::path::Component::Prefix;
        use std::path::Component::RootDir;

        let path = path.as_ref();
        let mut buf = RelativePathBuf::empty();
        for c in path.components() {
            match c {
                Prefix(_) | RootDir => {
                    return Err(FromSystemPathError::NonRelative(path.display().to_string()));
                }
                CurDir => continue,
                ParentDir => buf.push(RelativePath::unchecked_new(PARENT_STR)),
                Normal(s) => {
                    let s = s
                        .to_str()
                        .ok_or_else(|| FromSystemPathError::NonUtf8(path.display().to_string()))?;
                    buf.push(RelativePath::unchecked_new(s));
                }
            }
        }
        Ok(buf)
    }

    /// Extends `self` with `path`. A separator is added between `self` and
    /// `path` when both sides are non-empty and one isn't already there.
    ///
    /// ```
    /// use buck2_fs::paths::RelativePathBuf;
    ///
    /// let mut p = RelativePathBuf::empty();
    /// p.push("foo");
    /// p.push("bar");
    /// assert_eq!("foo/bar", p.as_str());
    /// ```
    pub fn push<P: AsRef<RelativePath>>(&mut self, path: P) {
        let other = path.as_ref().as_str();
        if !self.0.is_empty() && !self.0.ends_with(SEP) && !other.starts_with(SEP) {
            self.0.push(SEP);
        }
        self.0.push_str(other);
    }

    /// Truncates `self` to its parent. Returns `false` if there was no parent
    /// (i.e. the path was empty).
    pub fn pop(&mut self) -> bool {
        match self.parent().map(|p| p.0.len()) {
            Some(len) => {
                self.0.truncate(len);
                true
            }
            None => false,
        }
    }

    /// Consumes the buffer, yielding its underlying [`String`] storage.
    #[inline]
    pub fn into_string(self) -> String {
        self.0
    }

    /// Coerce to a [`RelativePath`] slice.
    #[inline]
    pub fn as_relative_path(&self) -> &RelativePath {
        self
    }
}

/// Walks `components` applying it to `buf`, cancelling `..` against the
/// preceding component where possible.
fn relative_traversal<'a, C: IntoIterator<Item = Component<'a>>>(
    buf: &mut RelativePathBuf,
    components: C,
) {
    for c in components {
        match c {
            Component::CurDir => (),
            Component::ParentDir => match buf.components().next_back() {
                Some(Component::ParentDir) | None => {
                    buf.push(RelativePath::unchecked_new(PARENT_STR));
                }
                _ => {
                    buf.pop();
                }
            },
            Component::Normal(name) => {
                buf.push(RelativePath::unchecked_new(name));
            }
        }
    }
}

/// A single component of a [`RelativePath`].
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Component<'a> {
    /// The current directory: `.`.
    CurDir,
    /// The parent directory: `..`.
    ParentDir,
    /// A normal path component.
    Normal(&'a str),
}

impl<'a> Component<'a> {
    /// Extracts the underlying string slice.
    #[inline]
    pub fn as_str(self) -> &'a str {
        match self {
            Component::CurDir => CURRENT_STR,
            Component::ParentDir => PARENT_STR,
            Component::Normal(s) => s,
        }
    }
}

/// Iterator over the components of a [`RelativePath`] as `&str` slices.
#[derive(Clone)]
pub struct Iter<'a> {
    inner: Components<'a>,
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a str;

    #[inline]
    fn next(&mut self) -> Option<&'a str> {
        self.inner.next().map(Component::as_str)
    }
}

impl<'a> DoubleEndedIterator for Iter<'a> {
    #[inline]
    fn next_back(&mut self) -> Option<&'a str> {
        self.inner.next_back().map(Component::as_str)
    }
}

/// Iterator over the components of a [`RelativePath`].
#[derive(Clone)]
pub struct Components<'a> {
    source: &'a str,
}

impl<'a> Components<'a> {
    /// Returns the path slice yet to be iterated, as a `RelativePath`.
    ///
    /// ```
    /// use buck2_fs::paths::RelativePath;
    ///
    /// let mut it = RelativePath::unchecked_new("foo/bar/baz").components();
    /// it.next();
    /// assert_eq!("bar/baz", it.as_relative_path().as_str());
    /// ```
    #[inline]
    pub fn as_relative_path(&self) -> &'a RelativePath {
        RelativePath::unchecked_new(self.source)
    }
}

impl<'a> Iterator for Components<'a> {
    type Item = Component<'a>;

    fn next(&mut self) -> Option<Component<'a>> {
        self.source = self.source.trim_start_matches(SEP);
        let slice = match self.source.find(SEP) {
            Some(i) => {
                let (slice, rest) = self.source.split_at(i);
                self.source = rest.trim_start_matches(SEP);
                slice
            }
            None => mem::take(&mut self.source),
        };
        match slice {
            "" => None,
            CURRENT_STR => Some(Component::CurDir),
            PARENT_STR => Some(Component::ParentDir),
            slice => Some(Component::Normal(slice)),
        }
    }
}

impl<'a> DoubleEndedIterator for Components<'a> {
    fn next_back(&mut self) -> Option<Component<'a>> {
        self.source = self.source.trim_end_matches(SEP);
        let slice = match self.source.rfind(SEP) {
            Some(i) => {
                let (rest, slice) = self.source.split_at(i + 1);
                self.source = rest.trim_end_matches(SEP);
                slice
            }
            None => mem::take(&mut self.source),
        };
        match slice {
            "" => None,
            CURRENT_STR => Some(Component::CurDir),
            PARENT_STR => Some(Component::ParentDir),
            slice => Some(Component::Normal(slice)),
        }
    }
}

/// Error returned by [`RelativePathBuf::from_system_path`].
#[derive(buck2_error::Error, Debug)]
#[buck2(input)]
#[buck2(tag = RelativePath)]
pub enum FromSystemPathError {
    #[error("Path is not relative: `{0}`")]
    NonRelative(String),
    #[error("Path is not UTF-8: `{0}`")]
    NonUtf8(String),
}

impl ToOwned for RelativePath {
    type Owned = RelativePathBuf;

    #[inline]
    fn to_owned(&self) -> RelativePathBuf {
        RelativePathBuf(self.0.to_owned())
    }
}

impl Borrow<RelativePath> for RelativePathBuf {
    #[inline]
    fn borrow(&self) -> &RelativePath {
        self
    }
}

impl Deref for RelativePathBuf {
    type Target = RelativePath;

    #[inline]
    fn deref(&self) -> &RelativePath {
        RelativePath::unchecked_new(&self.0)
    }
}

impl AsRef<RelativePath> for RelativePath {
    #[inline]
    fn as_ref(&self) -> &RelativePath {
        self
    }
}

impl AsRef<RelativePath> for RelativePathBuf {
    #[inline]
    fn as_ref(&self) -> &RelativePath {
        self
    }
}

impl AsRef<RelativePath> for str {
    #[inline]
    fn as_ref(&self) -> &RelativePath {
        RelativePath::unchecked_new(self)
    }
}

impl AsRef<RelativePath> for String {
    #[inline]
    fn as_ref(&self) -> &RelativePath {
        RelativePath::unchecked_new(self)
    }
}

impl AsRef<str> for RelativePath {
    #[inline]
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl AsRef<str> for RelativePathBuf {
    #[inline]
    fn as_ref(&self) -> &str {
        &self.0
    }
}

impl From<String> for RelativePathBuf {
    #[inline]
    fn from(s: String) -> RelativePathBuf {
        RelativePathBuf(s)
    }
}

impl<'a, T: ?Sized + AsRef<str>> From<&'a T> for RelativePathBuf {
    #[inline]
    fn from(s: &'a T) -> RelativePathBuf {
        RelativePathBuf(s.as_ref().to_owned())
    }
}

impl From<RelativePathBuf> for String {
    #[inline]
    fn from(p: RelativePathBuf) -> String {
        p.0
    }
}

impl From<RelativePathBuf> for Box<RelativePath> {
    fn from(p: RelativePathBuf) -> Box<RelativePath> {
        let s: Box<str> = p.0.into_boxed_str();
        unsafe {
            // SAFETY: `RelativePath` is a `#[repr(transparent)]` wrapper around `str`.
            transmute!(Box<str>, Box<RelativePath>, s)
        }
    }
}

impl Clone for Box<RelativePath> {
    #[inline]
    fn clone(&self) -> Self {
        Box::<RelativePath>::from(RelativePathBuf(self.0.to_owned()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_system_path_unix() {
        assert_eq!(
            "foo/bar",
            RelativePathBuf::from_system_path(Path::new("foo/bar"))
                .unwrap()
                .as_str()
        );
        // `.` is stripped.
        assert_eq!(
            "foo",
            RelativePathBuf::from_system_path(Path::new("./foo"))
                .unwrap()
                .as_str()
        );
        // `..` is preserved.
        assert_eq!(
            "../foo",
            RelativePathBuf::from_system_path(Path::new("../foo"))
                .unwrap()
                .as_str()
        );
        assert_eq!(
            "",
            RelativePathBuf::from_system_path(Path::new(""))
                .unwrap()
                .as_str()
        );
    }

    #[test]
    fn from_system_path_rejects_absolute() {
        // `/abs` is rejected on every platform because `Path::components` yields
        // a `RootDir` component first.
        let err = RelativePathBuf::from_system_path(Path::new("/abs")).unwrap_err();
        assert!(
            matches!(err, FromSystemPathError::NonRelative(_)),
            "{err:?}"
        );
    }

    #[cfg(unix)]
    #[test]
    fn from_system_path_non_utf8() {
        use std::ffi::OsStr;
        use std::os::unix::ffi::OsStrExt;

        let bad = OsStr::from_bytes(&[0x80]);
        let path = Path::new(bad);
        let err = RelativePathBuf::from_system_path(path).unwrap_err();
        assert!(matches!(err, FromSystemPathError::NonUtf8(_)), "{err:?}");
    }

    #[cfg(windows)]
    #[test]
    fn from_system_path_windows_backslashes() {
        // On Windows, `Path::components` splits on `\`, so backslash inputs
        // come out using forward slashes.
        assert_eq!(
            "foo/bar",
            RelativePathBuf::from_system_path(Path::new("foo\\bar"))
                .unwrap()
                .as_str()
        );
        let err = RelativePathBuf::from_system_path(Path::new("C:\\foo")).unwrap_err();
        assert!(
            matches!(err, FromSystemPathError::NonRelative(_)),
            "{err:?}"
        );
    }

    #[test]
    fn components_iter() {
        let p = RelativePath::unchecked_new("a/../b/./c");
        let got: Vec<_> = p.components().collect();
        assert_eq!(
            vec![
                Component::Normal("a"),
                Component::ParentDir,
                Component::Normal("b"),
                Component::CurDir,
                Component::Normal("c"),
            ],
            got,
        );

        // Empty components (`//`) and trailing slashes are skipped.
        let p = RelativePath::unchecked_new("a//b/");
        let got: Vec<_> = p.components().collect();
        assert_eq!(vec![Component::Normal("a"), Component::Normal("b")], got);

        // Empty path yields no components.
        assert_eq!(0, RelativePath::unchecked_new("").components().count());
    }

    #[test]
    fn components_as_relative_path() {
        let mut it = RelativePath::unchecked_new("foo/bar/baz").components();
        assert_eq!("foo/bar/baz", it.as_relative_path().as_str());
        it.next();
        assert_eq!("bar/baz", it.as_relative_path().as_str());
        it.next();
        assert_eq!("baz", it.as_relative_path().as_str());
        it.next();
        assert_eq!("", it.as_relative_path().as_str());
    }

    #[test]
    fn join_and_normalize() {
        assert_eq!(
            "foo/baz.txt",
            RelativePath::unchecked_new("foo/bar")
                .join_normalized("../baz.txt")
                .as_str()
        );
        assert_eq!(
            "../../baz.txt",
            RelativePath::unchecked_new("..")
                .join_normalized("../baz.txt")
                .as_str()
        );
        assert_eq!(
            "",
            RelativePath::unchecked_new("foo")
                .join_normalized("..")
                .as_str()
        );
    }

    #[test]
    fn normalize() {
        assert_eq!("", RelativePath::unchecked_new(".").normalize().as_str());
        assert_eq!(
            "",
            RelativePath::unchecked_new("foo/..").normalize().as_str()
        );
        assert_eq!(
            "../foo/baz.txt",
            RelativePath::unchecked_new("../foo/./bar/../baz.txt")
                .normalize()
                .as_str()
        );
    }

    #[test]
    fn push_and_pop() {
        let mut p = RelativePathBuf::empty();
        p.push("foo");
        p.push("bar");
        assert_eq!("foo/bar", p.as_str());

        let mut p = RelativePathBuf::from("foo/bar/baz");
        assert!(p.pop());
        assert_eq!("foo/bar", p.as_str());
        assert!(p.pop());
        assert_eq!("foo", p.as_str());
        assert!(p.pop());
        assert_eq!("", p.as_str());
        assert!(!p.pop());
    }

    #[test]
    fn cross_type_eq() {
        let buf = RelativePathBuf::from("foo/bar");
        let path = RelativePath::unchecked_new("foo/bar");
        assert_eq!(buf, *path);
        assert_eq!(*path, buf);
    }

    #[test]
    fn serde_roundtrip() {
        fn roundtrip(s: &str) {
            let buf = RelativePathBuf::from(s);
            let json = serde_json::to_string(&buf).unwrap();
            assert_eq!(format!("\"{s}\""), json);
            let back: RelativePathBuf = serde_json::from_str(&json).unwrap();
            assert_eq!(buf, back);
        }
        roundtrip("");
        roundtrip("foo/bar");
        roundtrip("../baz");
    }

    #[test]
    fn to_path_with_empty_base() {
        let p = RelativePath::unchecked_new("foo/bar").to_path("");
        assert_eq!(Path::new("foo/bar"), p);
    }
}
