//! [<img alt="github" src="https://img.shields.io/badge/github-udoprog/relative-path?style=for-the-badge&logo=github" height="20">](https://github.com/udoprog/relative-path)
//! [<img alt="crates.io" src="https://img.shields.io/crates/v/relative-path.svg?style=for-the-badge&color=fc8d62&logo=rust" height="20">](https://crates.io/crates/relative-path)
//! [<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-relative-path?style=for-the-badge&logoColor=white&logo=data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDUxMiA1MTIiPjxwYXRoIGZpbGw9IiNmNWY1ZjUiIGQ9Ik00ODguNiAyNTAuMkwzOTIgMjE0VjEwNS41YzAtMTUtOS4zLTI4LjQtMjMuNC0zMy43bC0xMDAtMzcuNWMtOC4xLTMuMS0xNy4xLTMuMS0yNS4zIDBsLTEwMCAzNy41Yy0xNC4xIDUuMy0yMy40IDE4LjctMjMuNCAzMy43VjIxNGwtOTYuNiAzNi4yQzkuMyAyNTUuNSAwIDI2OC45IDAgMjgzLjlWMzk0YzAgMTMuNiA3LjcgMjYuMSAxOS45IDMyLjJsMTAwIDUwYzEwLjEgNS4xIDIyLjEgNS4xIDMyLjIgMGwxMDMuOS01MiAxMDMuOSA1MmMxMC4xIDUuMSAyMi4xIDUuMSAzMi4yIDBsMTAwLTUwYzEyLjItNi4xIDE5LjktMTguNiAxOS45LTMyLjJWMjgzLjljMC0xNS05LjMtMjguNC0yMy40LTMzLjd6TTM1OCAyMTQuOGwtODUgMzEuOXYtNjguMmw4NS0zN3Y3My4zek0xNTQgMTA0LjFsMTAyLTM4LjIgMTAyIDM4LjJ2LjZsLTEwMiA0MS40LTEwMi00MS40di0uNnptODQgMjkxLjFsLTg1IDQyLjV2LTc5LjFsODUtMzguOHY3NS40em0wLTExMmwtMTAyIDQxLjQtMTAyLTQxLjR2LS42bDEwMi0zOC4yIDEwMiAzOC4ydi42em0yNDAgMTEybC04NSA0Mi41di03OS4xbDg1LTM4Ljh2NzUuNHptMC0xMTJsLTEwMiA0MS40LTEwMi00MS40di0uNmwxMDItMzguMiAxMDIgMzguMnYuNnoiPjwvcGF0aD48L3N2Zz4K" height="20">](https://docs.rs/relative-path)
//! [<img alt="build status" src="https://img.shields.io/github/workflow/status/udoprog/relative-path/CI/main?style=for-the-badge" height="20">](https://github.com/udoprog/relative-path/actions?query=branch%3Amain)
//!
//! Portable relative UTF-8 paths for Rust.
//!
//! This crate provides a module analogous to [`std::path`], with the following
//! characteristics:
//!
//! * The path separator is set to a fixed character (`/`), regardless of
//!   platform.
//! * Relative paths cannot represent a path in the filesystem without first
//!   specifying *what they are relative to* using functions such as [`to_path`]
//!   and [`to_logical_path`].
//! * Relative paths are always guaranteed to be valid UTF-8 strings.
//!
//! On top of this we support many operations that guarantee the same behavior
//! across platforms.
//!
//! <br>
//!
//! ## Usage
//!
//! Add the following to your `Cargo.toml`:
//!
//! ```toml
//! relative-path = "1.7.2"
//! ```
//!
//! <br>
//!
//! ## Serde Support
//!
//! This library includes serde support that can be enabled with the `serde`
//! feature.
//!
//! <br>
//!
//! ## Why is `std::path` a portability hazard?
//!
//! Path representations differ across platforms.
//!
//! * Windows permits using drive volumes (multiple roots) as a prefix (e.g.
//!   `"c:\"`) and backslash (`\`) as a separator.
//! * Unix references absolute paths from a single root and uses forward slash
//!   (`/`) as a separator.
//!
//! If we use `PathBuf`, Storing paths in a manifest would allow our application
//! to build and run on one platform but potentially not others.
//!
//! Consider the following data model and corresponding toml for a manifest:
//!
//! ```rust
//! use std::path::PathBuf;
//!
//! use serde::{Serialize, Deserialize};
//!
//! #[derive(Serialize, Deserialize)]
//! struct Manifest {
//!     source: PathBuf,
//! }
//! ```
//!
//! ```toml
//! source = "C:\\Users\\udoprog\\repo\\data\\source"
//! ```
//!
//! This will run for you (assuming `source` exists). So you go ahead and check
//! the manifest into git. The next day your Linux colleague calls you and
//! wonders what they have ever done to wrong you?
//!
//! So what went wrong? Well two things. You forgot to make the `source`
//! relative, so anyone at the company which has a different username than you
//! won't be able to use it. So you go ahead and fix that:
//!
//! ```toml
//! source = "data\\source"
//! ```
//!
//! But there is still one problem! A backslash (`\`) is only a legal path
//! separator on Windows. Luckily you learn that forward slashes are supported
//! both on Windows *and* Linux. So you opt for:
//!
//! ```toml
//! source = "data/source"
//! ```
//!
//! Things are working now. So all is well... Right? Sure, but we can do better.
//!
//! This crate provides types that work with *portable relative paths* (hence
//! the name). So by using [`RelativePath`] we can systematically help avoid
//! portability issues like the one above. Avoiding issues at the source is
//! preferably over spending 5 minutes of onboarding time on a theoretical
//! problem, hoping that your new hires will remember what to do if they ever
//! encounter it.
//!
//! Using [`RelativePathBuf`] we can fix our data model like this:
//!
//! ```rust
//! use relative_path::RelativePathBuf;
//! use serde::{Serialize, Deserialize};
//!
//! #[derive(Serialize, Deserialize)]
//! pub struct Manifest {
//!     source: RelativePathBuf,
//! }
//! ```
//!
//! And where it's used:
//!
//! ```rust,no_run
//! # use relative_path::RelativePathBuf;
//! # use serde::{Serialize, Deserialize};
//! # #[derive(Serialize, Deserialize)] pub struct Manifest { source: RelativePathBuf }
//! use std::fs;
//! use std::env::current_dir;
//!
//! let manifest: Manifest = todo!();
//!
//! let root = current_dir()?;
//! let source = manifest.source.to_path(&root);
//! let content = fs::read(&source)?;
//! # Ok::<_, Box<dyn std::error::Error>>(())
//! ```
//!
//! <br>
//!
//! ## Overview
//!
//! Conversion to a platform-specific [`Path`] happens through the [`to_path`]
//! and [`to_logical_path`] functions. Where you are required to specify the
//! path that prefixes the relative path. This can come from a function such as
//! [`std::env::current_dir`].
//!
//! ```rust
//! use std::env::current_dir;
//! use std::path::Path;
//!
//! use relative_path::RelativePath;
//!
//! let root = current_dir()?;
//!
//! # if cfg!(windows) {
//! // to_path unconditionally concatenates a relative path with its base:
//! let relative_path = RelativePath::new("../foo/./bar");
//! let full_path = relative_path.to_path(&root);
//! assert_eq!(full_path, root.join("..\\foo\\.\\bar"));
//!
//! // to_logical_path tries to apply the logical operations that the relative
//! // path corresponds to:
//! let relative_path = RelativePath::new("../foo/./bar");
//! let full_path = relative_path.to_logical_path(&root);
//!
//! // Replicate the operation performed by `to_logical_path`.
//! let mut parent = root.clone();
//! parent.pop();
//! assert_eq!(full_path, parent.join("foo\\bar"));
//! # }
//! # Ok::<_, std::io::Error>(())
//! ```
//!
//! When two relative paths are compared to each other, their exact component
//! makeup determines equality.
//!
//! ```rust
//! use relative_path::RelativePath;
//!
//! assert_ne!(
//!     RelativePath::new("foo/bar/../baz"),
//!     RelativePath::new("foo/baz")
//! );
//! ```
//!
//! Using platform-specific path separators to construct relative paths is not
//! supported.
//!
//! Path separators from other platforms are simply treated as part of a
//! component:
//!
//! ```rust
//! use relative_path::RelativePath;
//!
//! assert_ne!(
//!     RelativePath::new("foo/bar"),
//!     RelativePath::new("foo\\bar")
//! );
//!
//! assert_eq!(1, RelativePath::new("foo\\bar").components().count());
//! assert_eq!(2, RelativePath::new("foo/bar").components().count());
//! ```
//!
//! To see if two relative paths are equivalent you can use [`normalize`]:
//!
//! ```rust
//! use relative_path::RelativePath;
//!
//! assert_eq!(
//!     RelativePath::new("foo/bar/../baz").normalize(),
//!     RelativePath::new("foo/baz").normalize(),
//! );
//! ```
//!
//! <br>
//!
//! ## Additional portability notes
//!
//! While relative paths avoid the most egregious portability issue, that
//! absolute paths will work equally unwell on all platforms. We cannot avoid
//! all. This section tries to document additional portability hazards that we
//! are aware of.
//!
//! [`RelativePath`], similarly to [`Path`], makes no guarantees that its
//! constituent components make up legal file names. While components are
//! strictly separated by slashes, we can still store things in them which may
//! not be used as legal paths on all platforms.
//!
//! * A `NUL` character is not permitted on unix platforms - this is a
//!   terminator in C-based filesystem APIs. Slash (`/`) is also used as a path
//!   separator.
//! * Windows has a number of [reserved characters and names][windows-reserved]
//!   (like `CON`, `PRN`, and `AUX`) which cannot legally be part of a
//!   filesystem component.
//!
//! A relative path that *accidentally* contains a platform-specific components
//! will largely result in a nonsensical paths being generated in the hope that
//! they will fail fast during development and testing.
//!
//! ```rust
//! use relative_path::RelativePath;
//! use std::path::Path;
//!
//! if cfg!(windows) {
//!     assert_eq!(
//!         Path::new("foo\\c:\\bar\\baz"),
//!         RelativePath::new("c:\\bar\\baz").to_path("foo")
//!     );
//! }
//!
//! if cfg!(unix) {
//!     assert_eq!(
//!         Path::new("foo/bar/baz"),
//!         RelativePath::new("/bar/baz").to_path("foo")
//!     );
//! }
//! ```
//!
//! [`None`]: https://doc.rust-lang.org/std/option/enum.Option.html
//! [`normalize`]: https://docs.rs/relative-path/1/relative_path/struct.RelativePath.html#method.normalize
//! [`Path`]: https://doc.rust-lang.org/std/path/struct.Path.html
//! [`RelativePath`]: https://docs.rs/relative-path/1/relative_path/struct.RelativePath.html
//! [`RelativePathBuf`]: https://docs.rs/relative-path/1/relative_path/struct.RelativePathBuf.html
//! [`std::env::current_dir`]: https://doc.rust-lang.org/std/env/fn.current_dir.html
//! [`std::path`]: https://doc.rust-lang.org/std/path/index.html
//! [`to_logical_path`]: https://docs.rs/relative-path/1/relative_path/struct.RelativePath.html#method.to_logical_path
//! [`to_path`]: https://docs.rs/relative-path/1/relative_path/struct.RelativePath.html#method.to_path
//! [windows-reserved]: https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx

// This file contains parts that are Copyright 2015 The Rust Project Developers, copied from:
// https://github.com/rust-lang/rust
// cb2a656cdfb6400ac0200c661267f91fabf237e2 src/libstd/path.rs

#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]

use std::borrow::{Borrow, Cow};
use std::cmp;
use std::error;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::iter::FromIterator;
use std::mem;
use std::ops::{self, Deref};
use std::path;
use std::rc::Rc;
use std::str;
use std::sync::Arc;

#[cfg(feature = "serde")]
extern crate serde;

const STEM_SEP: char = '.';
const CURRENT_STR: &str = ".";
const PARENT_STR: &str = "..";

const SEP: char = '/';

fn split_file_at_dot(input: &str) -> (Option<&str>, Option<&str>) {
    if input == PARENT_STR {
        return (Some(input), None);
    }

    let mut iter = input.rsplitn(2, STEM_SEP);

    let after = iter.next();
    let before = iter.next();

    if before == Some("") {
        (Some(input), None)
    } else {
        (before, after)
    }
}

// Iterate through `iter` while it matches `prefix`; return `None` if `prefix`
// is not a prefix of `iter`, otherwise return `Some(iter_after_prefix)` giving
// `iter` after having exhausted `prefix`.
fn iter_after<'a, 'b, I, J>(mut iter: I, mut prefix: J) -> Option<I>
where
    I: Iterator<Item = Component<'a>> + Clone,
    J: Iterator<Item = Component<'b>>,
{
    loop {
        let mut iter_next = iter.clone();
        match (iter_next.next(), prefix.next()) {
            (Some(ref x), Some(ref y)) if x == y => (),
            (Some(_), Some(_)) => return None,
            (Some(_), None) => return Some(iter),
            (None, None) => return Some(iter),
            (None, Some(_)) => return None,
        }
        iter = iter_next;
    }
}

/// A single path component.
///
/// Accessed using the [RelativePath::components] iterator.
///
/// # Examples
///
/// ```rust
/// use relative_path::{Component, RelativePath};
///
/// let path = RelativePath::new("foo/../bar/./baz");
/// let mut it = path.components();
///
/// assert_eq!(Some(Component::Normal("foo")), it.next());
/// assert_eq!(Some(Component::ParentDir), it.next());
/// assert_eq!(Some(Component::Normal("bar")), it.next());
/// assert_eq!(Some(Component::CurDir), it.next());
/// assert_eq!(Some(Component::Normal("baz")), it.next());
/// assert_eq!(None, it.next());
/// ```
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Component<'a> {
    /// The current directory `.`.
    CurDir,
    /// The parent directory `..`.
    ParentDir,
    /// A normal path component as a string.
    Normal(&'a str),
}

impl<'a> Component<'a> {
    /// Extracts the underlying [`str`][std::str] slice.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::{RelativePath, Component};
    ///
    /// let path = RelativePath::new("./tmp/../foo/bar.txt");
    /// let components: Vec<_> = path.components().map(Component::as_str).collect();
    /// assert_eq!(&components, &[".", "tmp", "..", "foo", "bar.txt"]);
    /// ```
    pub fn as_str(self) -> &'a str {
        use self::Component::*;

        match self {
            CurDir => CURRENT_STR,
            ParentDir => PARENT_STR,
            Normal(name) => name,
        }
    }
}

/// Traverse the given components and apply to the provided stack.
///
/// This takes '.', and '..' into account. Where '.' doesn't change the stack, and '..' pops the
/// last item or further adds parent components.
#[inline(always)]
fn relative_traversal<'a, C>(buf: &mut RelativePathBuf, components: C)
where
    C: IntoIterator<Item = Component<'a>>,
{
    use self::Component::*;

    for c in components {
        match c {
            CurDir => (),
            ParentDir => match buf.components().next_back() {
                Some(Component::ParentDir) | None => {
                    buf.push(PARENT_STR);
                }
                _ => {
                    buf.pop();
                }
            },
            Normal(name) => {
                buf.push(name);
            }
        }
    }
}

/// Iterator over all the components in a relative path.
#[derive(Clone)]
pub struct Components<'a> {
    source: &'a str,
}

impl<'a> Iterator for Components<'a> {
    type Item = Component<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.source = self.source.trim_start_matches(SEP);

        let slice = match self.source.find(SEP) {
            Some(i) => {
                let (slice, rest) = self.source.split_at(i);
                self.source = rest.trim_start_matches(SEP);
                slice
            }
            None => mem::replace(&mut self.source, ""),
        };

        match slice {
            "" => None,
            "." => Some(Component::CurDir),
            ".." => Some(Component::ParentDir),
            slice => Some(Component::Normal(slice)),
        }
    }
}

impl<'a> DoubleEndedIterator for Components<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.source = self.source.trim_end_matches(SEP);

        let slice = match self.source.rfind(SEP) {
            Some(i) => {
                let (rest, slice) = self.source.split_at(i + 1);
                self.source = rest.trim_end_matches(SEP);
                slice
            }
            None => mem::replace(&mut self.source, ""),
        };

        match slice {
            "" => None,
            "." => Some(Component::CurDir),
            ".." => Some(Component::ParentDir),
            slice => Some(Component::Normal(slice)),
        }
    }
}

impl<'a> Components<'a> {
    /// Construct a new component from the given string.
    fn new(source: &'a str) -> Components<'a> {
        Self { source }
    }

    /// Extracts a slice corresponding to the portion of the path remaining for iteration.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// let mut components = RelativePath::new("tmp/foo/bar.txt").components();
    /// components.next();
    /// components.next();
    ///
    /// assert_eq!("bar.txt", components.as_relative_path());
    /// ```
    pub fn as_relative_path(&self) -> &'a RelativePath {
        RelativePath::new(self.source)
    }
}

impl<'a> cmp::PartialEq for Components<'a> {
    fn eq(&self, other: &Components<'a>) -> bool {
        Iterator::eq(self.clone(), other.clone())
    }
}

/// An iterator over the [`Component`]s of a [`RelativePath`], as
/// [`str`][std::str] slices.
///
/// This `struct` is created by the [`iter`][RelativePath::iter] method.
#[derive(Clone)]
pub struct Iter<'a> {
    inner: Components<'a>,
}

impl<'a> Iterator for Iter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<&'a str> {
        self.inner.next().map(Component::as_str)
    }
}

impl<'a> DoubleEndedIterator for Iter<'a> {
    fn next_back(&mut self) -> Option<&'a str> {
        self.inner.next_back().map(Component::as_str)
    }
}

/// Error kind for [`FromPathError`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum FromPathErrorKind {
    /// Non-relative component in path.
    NonRelative,
    /// Non-utf8 component in path.
    NonUtf8,
    /// Trying to convert a platform-specific path which uses a platform-specific separator.
    BadSeparator,
}

/// An error raised when attempting to convert a path using [RelativePathBuf::from_path].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FromPathError {
    kind: FromPathErrorKind,
}

impl FromPathError {
    /// Gets the underlying [`FromPathErrorKind`] that provides more details on
    /// what went wrong.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::path::Path;
    /// use relative_path::{FromPathErrorKind, RelativePathBuf};
    ///
    /// let result = RelativePathBuf::from_path(Path::new("/hello/world"));
    /// let e = result.unwrap_err();
    ///
    /// assert_eq!(FromPathErrorKind::NonRelative, e.kind());
    /// ```
    pub fn kind(&self) -> FromPathErrorKind {
        self.kind
    }
}

impl From<FromPathErrorKind> for FromPathError {
    fn from(value: FromPathErrorKind) -> Self {
        Self { kind: value }
    }
}

impl fmt::Display for FromPathError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            FromPathErrorKind::NonRelative => "path contains non-relative component".fmt(fmt),
            FromPathErrorKind::NonUtf8 => "path contains non-utf8 component".fmt(fmt),
            FromPathErrorKind::BadSeparator => {
                "path contains platform-specific path separator".fmt(fmt)
            }
        }
    }
}

impl error::Error for FromPathError {}

/// An owned, mutable relative path.
///
/// This type provides methods to manipulate relative path objects.
#[derive(Clone)]
pub struct RelativePathBuf {
    inner: String,
}

impl RelativePathBuf {
    /// Create a new relative path buffer.
    pub fn new() -> RelativePathBuf {
        RelativePathBuf {
            inner: String::new(),
        }
    }

    /// Internal constructor to allocate a relative path buf with the given capacity.
    fn with_capacity(cap: usize) -> RelativePathBuf {
        RelativePathBuf {
            inner: String::with_capacity(cap),
        }
    }

    /// Try to convert a [`Path`] to a [`RelativePathBuf`].
    ///
    /// [`Path`]: https://doc.rust-lang.org/std/path/struct.Path.html
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::{RelativePath, RelativePathBuf, FromPathErrorKind};
    /// use std::path::Path;
    ///
    /// assert_eq!(
    ///     Ok(RelativePath::new("foo/bar").to_owned()),
    ///     RelativePathBuf::from_path(Path::new("foo/bar"))
    /// );
    /// ```
    pub fn from_path<P: AsRef<path::Path>>(path: P) -> Result<RelativePathBuf, FromPathError> {
        use std::path::Component::*;

        let mut buffer = RelativePathBuf::new();

        for c in path.as_ref().components() {
            match c {
                Prefix(_) | RootDir => return Err(FromPathErrorKind::NonRelative.into()),
                CurDir => continue,
                ParentDir => buffer.push(".."),
                Normal(s) => buffer.push(s.to_str().ok_or(FromPathErrorKind::NonUtf8)?),
            }
        }

        Ok(buffer)
    }

    /// Extends `self` with `path`.
    ///
    /// If `path` is absolute, it replaces the current path.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::{RelativePathBuf, RelativePath};
    ///
    /// let mut path = RelativePathBuf::new();
    /// path.push("foo");
    /// path.push("bar");
    ///
    /// assert_eq!("foo/bar", path);
    /// ```
    pub fn push<P: AsRef<RelativePath>>(&mut self, path: P) {
        let other = path.as_ref();

        let other = if other.starts_with_sep() {
            &other.inner[1..]
        } else {
            &other.inner[..]
        };

        if !self.inner.is_empty() && !self.ends_with_sep() {
            self.inner.push(SEP);
        }

        self.inner.push_str(other)
    }

    /// Updates [`file_name`] to `file_name`.
    ///
    /// If [`file_name`] was [`None`], this is equivalent to pushing
    /// `file_name`.
    ///
    /// Otherwise it is equivalent to calling [`pop`] and then pushing
    /// `file_name`. The new path will be a sibling of the original path. (That
    /// is, it will have the same parent.)
    ///
    /// [`file_name`]: RelativePath::file_name
    /// [`pop`]: RelativePathBuf::pop
    /// [`None`]: https://doc.rust-lang.org/std/option/enum.Option.html
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePathBuf;
    ///
    /// let mut buf = RelativePathBuf::from("");
    /// assert!(buf.file_name() == None);
    /// buf.set_file_name("bar");
    /// assert_eq!(RelativePathBuf::from("bar"), buf);
    ///
    /// assert!(buf.file_name().is_some());
    /// buf.set_file_name("baz.txt");
    /// assert_eq!(RelativePathBuf::from("baz.txt"), buf);
    ///
    /// buf.push("bar");
    /// assert!(buf.file_name().is_some());
    /// buf.set_file_name("bar.txt");
    /// assert_eq!(RelativePathBuf::from("baz.txt/bar.txt"), buf);
    /// ```
    pub fn set_file_name<S: AsRef<str>>(&mut self, file_name: S) {
        if self.file_name().is_some() {
            let popped = self.pop();
            debug_assert!(popped);
        }

        self.push(file_name.as_ref());
    }

    /// Updates [`extension`] to `extension`.
    ///
    /// Returns `false` and does nothing if
    /// [`file_name`][RelativePath::file_name] is [`None`], returns `true` and
    /// updates the extension otherwise.
    ///
    /// If [`extension`] is [`None`], the extension is added; otherwise it is
    /// replaced.
    ///
    /// [`extension`]: RelativePath::extension
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::{RelativePath, RelativePathBuf};
    ///
    /// let mut p = RelativePathBuf::from("feel/the");
    ///
    /// p.set_extension("force");
    /// assert_eq!(RelativePath::new("feel/the.force"), p);
    ///
    /// p.set_extension("dark_side");
    /// assert_eq!(RelativePath::new("feel/the.dark_side"), p);
    ///
    /// assert!(p.pop());
    /// p.set_extension("nothing");
    /// assert_eq!(RelativePath::new("feel.nothing"), p);
    /// ```
    pub fn set_extension<S: AsRef<str>>(&mut self, extension: S) -> bool {
        let file_stem = match self.file_stem() {
            Some(stem) => stem,
            None => return false,
        };

        let end_file_stem = file_stem[file_stem.len()..].as_ptr() as usize;
        let start = self.inner.as_ptr() as usize;
        self.inner.truncate(end_file_stem.wrapping_sub(start));

        let extension = extension.as_ref();

        if !extension.is_empty() {
            self.inner.push(STEM_SEP);
            self.inner.push_str(extension);
        }

        true
    }

    /// Truncates `self` to [`parent`][RelativePath::parent].
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::{RelativePath, RelativePathBuf};
    ///
    /// let mut p = RelativePathBuf::from("test/test.rs");
    ///
    /// assert_eq!(true, p.pop());
    /// assert_eq!(RelativePath::new("test"), p);
    /// assert_eq!(true, p.pop());
    /// assert_eq!(RelativePath::new(""), p);
    /// assert_eq!(false, p.pop());
    /// assert_eq!(RelativePath::new(""), p);
    /// ```
    pub fn pop(&mut self) -> bool {
        match self.parent().map(|p| p.inner.len()) {
            Some(len) => {
                self.inner.truncate(len);
                true
            }
            None => false,
        }
    }

    /// Coerce to a [`RelativePath`] slice.
    pub fn as_relative_path(&self) -> &RelativePath {
        self
    }

    /// Consumes the `RelativePathBuf`, yielding its internal [`String`] storage.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePathBuf;
    ///
    /// let p = RelativePathBuf::from("/the/head");
    /// let string = p.into_string();
    /// assert_eq!(string, "/the/head".to_owned());
    /// ```
    pub fn into_string(self) -> String {
        self.inner
    }

    /// Converts this `RelativePathBuf` into a [boxed][std::boxed::Box]
    /// [`RelativePath`].
    pub fn into_boxed_relative_path(self) -> Box<RelativePath> {
        let rw = Box::into_raw(self.inner.into_boxed_str()) as *mut RelativePath;
        unsafe { Box::from_raw(rw) }
    }
}

impl Default for RelativePathBuf {
    fn default() -> Self {
        RelativePathBuf::new()
    }
}

impl<'a> From<&'a RelativePath> for Cow<'a, RelativePath> {
    #[inline]
    fn from(s: &'a RelativePath) -> Cow<'a, RelativePath> {
        Cow::Borrowed(s)
    }
}

impl<'a> From<RelativePathBuf> for Cow<'a, RelativePath> {
    #[inline]
    fn from(s: RelativePathBuf) -> Cow<'a, RelativePath> {
        Cow::Owned(s)
    }
}

impl fmt::Debug for RelativePathBuf {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}", &self.inner)
    }
}

impl AsRef<RelativePath> for RelativePathBuf {
    fn as_ref(&self) -> &RelativePath {
        RelativePath::new(&self.inner)
    }
}

impl Borrow<RelativePath> for RelativePathBuf {
    fn borrow(&self) -> &RelativePath {
        self.deref()
    }
}

impl<'a, T: ?Sized + AsRef<str>> From<&'a T> for RelativePathBuf {
    fn from(path: &'a T) -> RelativePathBuf {
        RelativePathBuf {
            inner: path.as_ref().to_owned(),
        }
    }
}

impl From<String> for RelativePathBuf {
    fn from(path: String) -> RelativePathBuf {
        RelativePathBuf { inner: path }
    }
}

impl From<RelativePathBuf> for String {
    fn from(path: RelativePathBuf) -> String {
        path.into_string()
    }
}

impl ops::Deref for RelativePathBuf {
    type Target = RelativePath;

    fn deref(&self) -> &RelativePath {
        RelativePath::new(&self.inner)
    }
}

impl cmp::PartialEq for RelativePathBuf {
    fn eq(&self, other: &RelativePathBuf) -> bool {
        self.components() == other.components()
    }
}

impl cmp::Eq for RelativePathBuf {}

impl cmp::PartialOrd for RelativePathBuf {
    fn partial_cmp(&self, other: &RelativePathBuf) -> Option<cmp::Ordering> {
        self.components().partial_cmp(other.components())
    }
}

impl cmp::Ord for RelativePathBuf {
    fn cmp(&self, other: &RelativePathBuf) -> cmp::Ordering {
        self.components().cmp(other.components())
    }
}

impl Hash for RelativePathBuf {
    fn hash<H: Hasher>(&self, h: &mut H) {
        self.as_relative_path().hash(h)
    }
}

impl<P: AsRef<RelativePath>> Extend<P> for RelativePathBuf {
    fn extend<I: IntoIterator<Item = P>>(&mut self, iter: I) {
        iter.into_iter().for_each(move |p| self.push(p.as_ref()));
    }
}

impl<P: AsRef<RelativePath>> FromIterator<P> for RelativePathBuf {
    fn from_iter<I: IntoIterator<Item = P>>(iter: I) -> RelativePathBuf {
        let mut buf = RelativePathBuf::new();
        buf.extend(iter);
        buf
    }
}

/// A borrowed, immutable relative path.
#[repr(transparent)]
pub struct RelativePath {
    inner: str,
}

/// An error returned from [strip_prefix] if the prefix was not found.
///
/// [strip_prefix]: RelativePath::strip_prefix
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StripPrefixError(());

impl RelativePath {
    /// Directly wraps a string slice as a `RelativePath` slice.
    pub fn new<S: AsRef<str> + ?Sized>(s: &S) -> &RelativePath {
        unsafe { &*(s.as_ref() as *const str as *const RelativePath) }
    }

    /// Try to convert a [`Path`] to a [`RelativePath`] without allocating a buffer.
    ///
    /// [`Path`]: std::path::Path
    ///
    /// # Errors
    ///
    /// This requires the path to be a legal, platform-neutral relative path.
    /// Otherwise various forms of [`FromPathError`] will be returned as an
    /// [`Err`].
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::{RelativePath, FromPathErrorKind};
    ///
    /// assert_eq!(
    ///     Ok(RelativePath::new("foo/bar")),
    ///     RelativePath::from_path("foo/bar")
    /// );
    ///
    /// // Note: absolute paths are different depending on platform.
    /// if cfg!(windows) {
    ///     let e = RelativePath::from_path("c:\\foo\\bar").unwrap_err();
    ///     assert_eq!(FromPathErrorKind::NonRelative, e.kind());
    /// }
    ///
    /// if cfg!(unix) {
    ///     let e = RelativePath::from_path("/foo/bar").unwrap_err();
    ///     assert_eq!(FromPathErrorKind::NonRelative, e.kind());
    /// }
    /// ```
    pub fn from_path<P: ?Sized + AsRef<path::Path>>(
        path: &P,
    ) -> Result<&RelativePath, FromPathError> {
        use std::path::Component::*;

        let other = path.as_ref();

        let s = match other.to_str() {
            Some(s) => s,
            None => return Err(FromPathErrorKind::NonUtf8.into()),
        };

        let rel = RelativePath::new(s);

        // check that the component compositions are equal.
        for (a, b) in other.components().zip(rel.components()) {
            match (a, b) {
                (Prefix(_), _) | (RootDir, _) => return Err(FromPathErrorKind::NonRelative.into()),
                (CurDir, Component::CurDir) => continue,
                (ParentDir, Component::ParentDir) => continue,
                (Normal(a), Component::Normal(b)) if a == b => continue,
                _ => return Err(FromPathErrorKind::BadSeparator.into()),
            }
        }

        Ok(rel)
    }

    /// Yields the underlying [`str`][std::str] slice.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// assert_eq!(RelativePath::new("foo.txt").as_str(), "foo.txt");
    /// ```
    pub fn as_str(&self) -> &str {
        &self.inner
    }

    /// Returns an object that implements [`Display`][std::fmt::Display].
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// let path = RelativePath::new("tmp/foo.rs");
    ///
    /// println!("{}", path.display());
    /// ```
    #[deprecated(note = "RelativePath implements std::fmt::Display directly")]
    pub fn display(&self) -> Display {
        Display { path: self }
    }

    /// Creates an owned [`RelativePathBuf`] with path adjoined to self.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// let path = RelativePath::new("foo/bar");
    /// assert_eq!("foo/bar/baz", path.join("baz"));
    /// ```
    pub fn join<P: AsRef<RelativePath>>(&self, path: P) -> RelativePathBuf {
        let mut out = self.to_relative_path_buf();
        out.push(path);
        out
    }

    /// Iterate over all components in this relative path.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::{Component, RelativePath};
    ///
    /// let path = RelativePath::new("foo/bar/baz");
    /// let mut it = path.components();
    ///
    /// assert_eq!(Some(Component::Normal("foo")), it.next());
    /// assert_eq!(Some(Component::Normal("bar")), it.next());
    /// assert_eq!(Some(Component::Normal("baz")), it.next());
    /// assert_eq!(None, it.next());
    /// ```
    pub fn components(&self) -> Components {
        Components::new(&self.inner)
    }

    /// Produces an iterator over the path's components viewed as
    /// [`str`][std::str] slices.
    ///
    /// For more information about the particulars of how the path is separated
    /// into components, see [`components`][Self::components].
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// let mut it = RelativePath::new("/tmp/foo.txt").iter();
    /// assert_eq!(it.next(), Some("tmp"));
    /// assert_eq!(it.next(), Some("foo.txt"));
    /// assert_eq!(it.next(), None)
    /// ```
    pub fn iter(&self) -> Iter {
        Iter {
            inner: self.components(),
        }
    }

    /// Convert to an owned [`RelativePathBuf`].
    pub fn to_relative_path_buf(&self) -> RelativePathBuf {
        RelativePathBuf::from(self.inner.to_owned())
    }

    /// Build an owned [`PathBuf`] relative to `base` for the current relative
    /// path.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    /// use std::path::Path;
    ///
    /// let path = RelativePath::new("foo/bar").to_path(".");
    /// assert_eq!(Path::new("./foo/bar"), path);
    ///
    /// let path = RelativePath::new("foo/bar").to_path("");
    /// assert_eq!(Path::new("foo/bar"), path);
    /// ```
    ///
    /// # Encoding an absolute path
    ///
    /// Absolute paths are, in contrast to when using [`PathBuf::push`] *ignored*
    /// and will be added unchanged to the buffer.
    ///
    /// This is to preserve the probability of a path conversion failing if the
    /// relative path contains platform-specific absolute path components.
    ///
    /// ```
    /// use relative_path::RelativePath;
    /// use std::path::Path;
    ///
    /// if cfg!(windows) {
    ///     let path = RelativePath::new("/bar/baz").to_path("foo");
    ///     assert_eq!(Path::new("foo\\bar\\baz"), path);
    ///
    ///     let path = RelativePath::new("c:\\bar\\baz").to_path("foo");
    ///     assert_eq!(Path::new("foo\\c:\\bar\\baz"), path);
    /// }
    ///
    /// if cfg!(unix) {
    ///     let path = RelativePath::new("/bar/baz").to_path("foo");
    ///     assert_eq!(Path::new("foo/bar/baz"), path);
    ///
    ///     let path = RelativePath::new("c:\\bar\\baz").to_path("foo");
    ///     assert_eq!(Path::new("foo/c:\\bar\\baz"), path);
    /// }
    /// ```
    ///
    /// [`PathBuf`]: std::path::PathBuf
    /// [`PathBuf::push`]: std::path::PathBuf::push
    pub fn to_path<P: AsRef<path::Path>>(&self, base: P) -> path::PathBuf {
        let mut p = base.as_ref().to_path_buf().into_os_string();

        for c in self.components() {
            if !p.is_empty() {
                p.push(path::MAIN_SEPARATOR.encode_utf8(&mut [0u8, 0u8, 0u8, 0u8]));
            }

            p.push(c.as_str());
        }

        path::PathBuf::from(p)
    }

    /// Build an owned [`PathBuf`] relative to `base` for the current relative
    /// path.
    ///
    /// This is similar to [`to_path`][RelativePath::to_path] except that it
    /// doesn't just unconditionally append one path to the other, instead it
    /// performs the following operations depending on its own components:
    ///
    /// * [Component::CurDir] leaves the `base` unmodified.
    /// * [Component::ParentDir] removes a component from `base` using
    ///   [path::PathBuf::pop].
    /// * [Component::Normal] pushes the given path component onto `base` using
    ///   the same mechanism as [`to_path`][RelativePath::to_path].
    ///
    /// Note that the exact semantics of the path operation is determined by the
    /// corresponding [`PathBuf`] operation. E.g. popping a component off a path
    /// like `.` will result in an empty path.
    ///
    /// ```
    /// use relative_path::RelativePath;
    /// use std::path::Path;
    ///
    /// let path = RelativePath::new("..").to_logical_path(".");
    /// assert_eq!(path, Path::new(""));
    /// ```
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    /// use std::path::Path;
    ///
    /// let path = RelativePath::new("..").to_logical_path("foo/bar");
    /// assert_eq!(path, Path::new("foo"));
    /// ```
    ///
    /// # Encoding an absolute path
    ///
    /// Behaves the same as [`to_path`][RelativePath::to_path] when encoding
    /// absolute paths.
    ///
    /// Absolute paths are, in contrast to when using [`PathBuf::push`] *ignored*
    /// and will be added unchanged to the buffer.
    ///
    /// This is to preserve the probability of a path conversion failing if the
    /// relative path contains platform-specific absolute path components.
    ///
    /// ```
    /// use relative_path::RelativePath;
    /// use std::path::Path;
    ///
    /// if cfg!(windows) {
    ///     let path = RelativePath::new("/bar/baz").to_logical_path("foo");
    ///     assert_eq!(Path::new("foo\\bar\\baz"), path);
    ///
    ///     let path = RelativePath::new("c:\\bar\\baz").to_logical_path("foo");
    ///     assert_eq!(Path::new("foo\\c:\\bar\\baz"), path);
    ///
    ///     let path = RelativePath::new("foo/bar").to_logical_path("");
    ///     assert_eq!(Path::new("foo\\bar"), path);
    /// }
    ///
    /// if cfg!(unix) {
    ///     let path = RelativePath::new("/bar/baz").to_logical_path("foo");
    ///     assert_eq!(Path::new("foo/bar/baz"), path);
    ///
    ///     let path = RelativePath::new("c:\\bar\\baz").to_logical_path("foo");
    ///     assert_eq!(Path::new("foo/c:\\bar\\baz"), path);
    ///
    ///     let path = RelativePath::new("foo/bar").to_logical_path("");
    ///     assert_eq!(Path::new("foo/bar"), path);
    /// }
    /// ```
    ///
    /// [`PathBuf`]: std::path::PathBuf
    /// [`PathBuf::push`]: std::path::PathBuf::push
    pub fn to_logical_path<P: AsRef<path::Path>>(&self, base: P) -> path::PathBuf {
        use self::Component::*;

        let mut p = base.as_ref().to_path_buf().into_os_string();

        for c in self.components() {
            match c {
                CurDir => continue,
                ParentDir => {
                    let mut temp = path::PathBuf::from(std::mem::take(&mut p));
                    temp.pop();
                    p = temp.into_os_string();
                }
                Normal(c) => {
                    if !p.is_empty() {
                        p.push(path::MAIN_SEPARATOR.encode_utf8(&mut [0u8, 0u8, 0u8, 0u8]));
                    }

                    p.push(c);
                }
            }
        }

        path::PathBuf::from(p)
    }

    /// Returns a relative path, without its final [`Component`] if there is one.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// assert_eq!(Some(RelativePath::new("foo")), RelativePath::new("foo/bar").parent());
    /// assert_eq!(Some(RelativePath::new("")), RelativePath::new("foo").parent());
    /// assert_eq!(None, RelativePath::new("").parent());
    /// ```
    pub fn parent(&self) -> Option<&RelativePath> {
        use self::Component::*;

        if self.inner.is_empty() {
            return None;
        }

        let mut it = self.components();
        while let Some(CurDir) = it.next_back() {}
        Some(it.as_relative_path())
    }

    /// Returns the final component of the `RelativePath`, if there is one.
    ///
    /// If the path is a normal file, this is the file name. If it's the path of
    /// a directory, this is the directory name.
    ///
    /// Returns [`None`] If the path terminates in `..`.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// assert_eq!(Some("bin"), RelativePath::new("usr/bin/").file_name());
    /// assert_eq!(Some("foo.txt"), RelativePath::new("tmp/foo.txt").file_name());
    /// assert_eq!(Some("foo.txt"), RelativePath::new("tmp/foo.txt/").file_name());
    /// assert_eq!(Some("foo.txt"), RelativePath::new("foo.txt/.").file_name());
    /// assert_eq!(Some("foo.txt"), RelativePath::new("foo.txt/.//").file_name());
    /// assert_eq!(None, RelativePath::new("foo.txt/..").file_name());
    /// assert_eq!(None, RelativePath::new("/").file_name());
    /// ```
    pub fn file_name(&self) -> Option<&str> {
        use self::Component::*;

        let mut it = self.components();

        while let Some(c) = it.next_back() {
            return match c {
                CurDir => continue,
                Normal(name) => Some(name),
                _ => None,
            };
        }

        None
    }

    /// Returns a relative path that, when joined onto `base`, yields `self`.
    ///
    /// # Errors
    ///
    /// If `base` is not a prefix of `self` (i.e.
    /// [starts_with][Self::starts_with] returns `false`), returns [`Err`].
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// let path = RelativePath::new("test/haha/foo.txt");
    ///
    /// assert_eq!(path.strip_prefix("test"), Ok(RelativePath::new("haha/foo.txt")));
    /// assert_eq!(path.strip_prefix("test").is_ok(), true);
    /// assert_eq!(path.strip_prefix("haha").is_ok(), false);
    /// ```
    pub fn strip_prefix<P: AsRef<RelativePath>>(
        &self,
        base: P,
    ) -> Result<&RelativePath, StripPrefixError> {
        iter_after(self.components(), base.as_ref().components())
            .map(|c| c.as_relative_path())
            .ok_or(StripPrefixError(()))
    }

    /// Determines whether `base` is a prefix of `self`.
    ///
    /// Only considers whole path components to match.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// let path = RelativePath::new("etc/passwd");
    ///
    /// assert!(path.starts_with("etc"));
    ///
    /// assert!(!path.starts_with("e"));
    /// ```
    pub fn starts_with<P: AsRef<RelativePath>>(&self, base: P) -> bool {
        iter_after(self.components(), base.as_ref().components()).is_some()
    }

    /// Determines whether `child` is a suffix of `self`.
    ///
    /// Only considers whole path components to match.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// let path = RelativePath::new("etc/passwd");
    ///
    /// assert!(path.ends_with("passwd"));
    /// ```
    pub fn ends_with<P: AsRef<RelativePath>>(&self, child: P) -> bool {
        iter_after(self.components().rev(), child.as_ref().components().rev()).is_some()
    }

    /// Determines whether `self` is normalized.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// // These are normalized.
    /// assert!(RelativePath::new("").is_normalized());
    /// assert!(RelativePath::new("baz.txt").is_normalized());
    /// assert!(RelativePath::new("foo/bar/baz.txt").is_normalized());
    /// assert!(RelativePath::new("..").is_normalized());
    /// assert!(RelativePath::new("../..").is_normalized());
    /// assert!(RelativePath::new("../../foo/bar/baz.txt").is_normalized());
    ///
    /// // These are not normalized.
    /// assert!(!RelativePath::new(".").is_normalized());
    /// assert!(!RelativePath::new("./baz.txt").is_normalized());
    /// assert!(!RelativePath::new("foo/..").is_normalized());
    /// assert!(!RelativePath::new("foo/../baz.txt").is_normalized());
    /// assert!(!RelativePath::new("foo/.").is_normalized());
    /// assert!(!RelativePath::new("foo/./baz.txt").is_normalized());
    /// assert!(!RelativePath::new("../foo/./bar/../baz.txt").is_normalized());
    /// ```
    pub fn is_normalized(&self) -> bool {
        self.components()
            .skip_while(|c| matches!(c, Component::ParentDir))
            .all(|c| matches!(c, Component::Normal(_)))
    }

    /// Creates an owned [`RelativePathBuf`] like `self` but with the given file
    /// name.
    ///
    /// See [set_file_name][RelativePathBuf::set_file_name] for more details.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::{RelativePath, RelativePathBuf};
    ///
    /// let path = RelativePath::new("tmp/foo.txt");
    /// assert_eq!(path.with_file_name("bar.txt"), RelativePathBuf::from("tmp/bar.txt"));
    ///
    /// let path = RelativePath::new("tmp");
    /// assert_eq!(path.with_file_name("var"), RelativePathBuf::from("var"));
    /// ```
    pub fn with_file_name<S: AsRef<str>>(&self, file_name: S) -> RelativePathBuf {
        let mut buf = self.to_relative_path_buf();
        buf.set_file_name(file_name);
        buf
    }

    /// Extracts the stem (non-extension) portion of [`file_name`][Self::file_name].
    ///
    /// The stem is:
    ///
    /// * [`None`], if there is no file name;
    /// * The entire file name if there is no embedded `.`;
    /// * The entire file name if the file name begins with `.` and has no other `.`s within;
    /// * Otherwise, the portion of the file name before the final `.`
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// let path = RelativePath::new("foo.rs");
    ///
    /// assert_eq!("foo", path.file_stem().unwrap());
    /// ```
    pub fn file_stem(&self) -> Option<&str> {
        self.file_name()
            .map(split_file_at_dot)
            .and_then(|(before, after)| before.or(after))
    }

    /// Extracts the extension of [`file_name`][Self::file_name], if possible.
    ///
    /// The extension is:
    ///
    /// * [`None`], if there is no file name;
    /// * [`None`], if there is no embedded `.`;
    /// * [`None`], if the file name begins with `.` and has no other `.`s within;
    /// * Otherwise, the portion of the file name after the final `.`
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// assert_eq!(Some("rs"), RelativePath::new("foo.rs").extension());
    /// assert_eq!(None, RelativePath::new(".rs").extension());
    /// assert_eq!(Some("rs"), RelativePath::new("foo.rs/.").extension());
    /// ```
    pub fn extension(&self) -> Option<&str> {
        self.file_name()
            .map(split_file_at_dot)
            .and_then(|(before, after)| before.and(after))
    }

    /// Creates an owned [`RelativePathBuf`] like `self` but with the given
    /// extension.
    ///
    /// See [set_extension][RelativePathBuf::set_extension] for more details.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::{RelativePath, RelativePathBuf};
    ///
    /// let path = RelativePath::new("foo.rs");
    /// assert_eq!(path.with_extension("txt"), RelativePathBuf::from("foo.txt"));
    /// ```
    pub fn with_extension<S: AsRef<str>>(&self, extension: S) -> RelativePathBuf {
        let mut buf = self.to_relative_path_buf();
        buf.set_extension(extension);
        buf
    }

    /// Build an owned [`RelativePathBuf`], joined with the given path and
    /// normalized.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// assert_eq!(
    ///     RelativePath::new("foo/baz.txt"),
    ///     RelativePath::new("foo/bar").join_normalized("../baz.txt").as_relative_path()
    /// );
    ///
    /// assert_eq!(
    ///     RelativePath::new("../foo/baz.txt"),
    ///     RelativePath::new("../foo/bar").join_normalized("../baz.txt").as_relative_path()
    /// );
    /// ```
    pub fn join_normalized<P: AsRef<RelativePath>>(&self, path: P) -> RelativePathBuf {
        let mut buf = RelativePathBuf::new();
        relative_traversal(&mut buf, self.components());
        relative_traversal(&mut buf, path.as_ref().components());
        buf
    }

    /// Return an owned [`RelativePathBuf`], with all non-normal components
    /// moved to the beginning of the path.
    ///
    /// This permits for a normalized representation of different relative
    /// components.
    ///
    /// Normalization is a _destructive_ operation if the path references an
    /// actual filesystem path. An example of this is symlinks under unix, a
    /// path like `foo/../bar` might reference a different location other than
    /// `./bar`.
    ///
    /// Normalization is a logical operation that is only valid if the relative
    /// path is part of some context which doesn't have semantics that causes it
    /// to break, like symbolic links.
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// assert_eq!(
    ///     "../foo/baz.txt",
    ///     RelativePath::new("../foo/./bar/../baz.txt").normalize()
    /// );
    ///
    /// assert_eq!(
    ///     "",
    ///     RelativePath::new(".").normalize()
    /// );
    /// ```
    pub fn normalize(&self) -> RelativePathBuf {
        let mut buf = RelativePathBuf::new();
        relative_traversal(&mut buf, self.components());
        buf
    }

    /// Constructs a relative path from the current path, to `path`.
    ///
    /// This function will return the empty [`RelativePath`] `""` if this source
    /// contains unnamed components like `..` that would have to be traversed to
    /// reach the destination `path`. This is necessary since we have no way of
    /// knowing what the names of those components are when we're building the
    /// new relative path.
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// // Here we don't know what directories `../..` refers to, so there's no
    /// // way to construct a path back to `bar` in the current directory from
    /// // `../..`.
    /// let from = RelativePath::new("../../foo/relative-path");
    /// let to = RelativePath::new("bar");
    /// assert_eq!("", from.relative(to));
    /// ```
    ///
    /// One exception to this is when two paths contains a common prefix at
    /// which point there's no need to know what the names of those unnamed
    /// components are.
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// let from = RelativePath::new("../../foo/bar");
    /// let to = RelativePath::new("../../foo/baz");
    ///
    /// assert_eq!("../baz", from.relative(to));
    ///
    /// let from = RelativePath::new("../a/../../foo/bar");
    /// let to = RelativePath::new("../../foo/baz");
    ///
    /// assert_eq!("../baz", from.relative(to));
    /// ```
    ///
    /// # Examples
    ///
    /// ```
    /// use relative_path::RelativePath;
    ///
    /// assert_eq!(
    ///     "../../e/f",
    ///     RelativePath::new("a/b/c/d").relative(RelativePath::new("a/b/e/f"))
    /// );
    ///
    /// assert_eq!(
    ///     "../bbb",
    ///     RelativePath::new("a/../aaa").relative(RelativePath::new("b/../bbb"))
    /// );
    ///
    /// let a = RelativePath::new("git/relative-path");
    /// let b = RelativePath::new("git");
    /// assert_eq!("relative-path", b.relative(a));
    /// assert_eq!("..", a.relative(b));
    ///
    /// let a = RelativePath::new("foo/bar/bap/foo.h");
    /// let b = RelativePath::new("../arch/foo.h");
    /// assert_eq!("../../../../../arch/foo.h", a.relative(b));
    /// assert_eq!("", b.relative(a));
    /// ```
    pub fn relative<P: AsRef<RelativePath>>(&self, path: P) -> RelativePathBuf {
        let mut from = RelativePathBuf::new();
        let mut to = RelativePathBuf::new();

        relative_traversal(&mut from, self.components());
        relative_traversal(&mut to, path.as_ref().components());

        let mut it_from = from.components();
        let mut it_to = to.components();

        // Strip a common prefixes - if any.
        let (lead_from, lead_to) = loop {
            match (it_from.next(), it_to.next()) {
                (Some(f), Some(t)) if f == t => continue,
                (f, t) => {
                    break (f, t);
                }
            }
        };

        // Special case: The path we are traversing from can't contain unnamed
        // components. A relative path might be any path, like `/`, or
        // `/foo/bar/baz`, and these components cannot be named in the relative
        // traversal.
        //
        // Also note that `relative_traversal` guarantees that all ParentDir
        // components are at the head of the path being built.
        if lead_from == Some(Component::ParentDir) {
            return RelativePathBuf::new();
        }

        let head = lead_from.into_iter().chain(it_from);
        let tail = lead_to.into_iter().chain(it_to);

        let mut buf = RelativePathBuf::with_capacity(usize::max(from.inner.len(), to.inner.len()));

        for c in head.map(|_| Component::ParentDir).chain(tail) {
            buf.push(c.as_str());
        }

        buf
    }

    /// Check if path starts with a path separator.
    fn starts_with_sep(&self) -> bool {
        self.inner.starts_with(SEP)
    }

    /// Check if path ends with a path separator.
    fn ends_with_sep(&self) -> bool {
        self.inner.ends_with(SEP)
    }
}

impl From<&RelativePath> for Box<RelativePath> {
    #[inline]
    fn from(path: &RelativePath) -> Box<RelativePath> {
        let boxed: Box<str> = path.inner.into();
        let rw = Box::into_raw(boxed) as *mut RelativePath;
        unsafe { Box::from_raw(rw) }
    }
}

impl From<RelativePathBuf> for Box<RelativePath> {
    #[inline]
    fn from(path: RelativePathBuf) -> Box<RelativePath> {
        let boxed: Box<str> = path.inner.into();
        let rw = Box::into_raw(boxed) as *mut RelativePath;
        unsafe { Box::from_raw(rw) }
    }
}

impl Clone for Box<RelativePath> {
    fn clone(&self) -> Self {
        self.to_relative_path_buf().into_boxed_relative_path()
    }
}

impl From<&RelativePath> for Arc<RelativePath> {
    #[inline]
    fn from(path: &RelativePath) -> Arc<RelativePath> {
        let arc: Arc<str> = path.inner.into();
        let rw = Arc::into_raw(arc) as *const RelativePath;
        unsafe { Arc::from_raw(rw) }
    }
}

impl From<RelativePathBuf> for Arc<RelativePath> {
    #[inline]
    fn from(path: RelativePathBuf) -> Arc<RelativePath> {
        let arc: Arc<str> = path.inner.into();
        let rw = Arc::into_raw(arc) as *const RelativePath;
        unsafe { Arc::from_raw(rw) }
    }
}

impl From<&RelativePath> for Rc<RelativePath> {
    #[inline]
    fn from(path: &RelativePath) -> Rc<RelativePath> {
        let rc: Rc<str> = path.inner.into();
        let rw = Rc::into_raw(rc) as *const RelativePath;
        unsafe { Rc::from_raw(rw) }
    }
}

impl From<RelativePathBuf> for Rc<RelativePath> {
    #[inline]
    fn from(path: RelativePathBuf) -> Rc<RelativePath> {
        let rc: Rc<str> = path.inner.into();
        let rw = Rc::into_raw(rc) as *const RelativePath;
        unsafe { Rc::from_raw(rw) }
    }
}

impl ToOwned for RelativePath {
    type Owned = RelativePathBuf;

    fn to_owned(&self) -> RelativePathBuf {
        self.to_relative_path_buf()
    }
}

impl fmt::Debug for RelativePath {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}", &self.inner)
    }
}

impl AsRef<str> for RelativePathBuf {
    fn as_ref(&self) -> &str {
        &self.inner
    }
}

impl AsRef<RelativePath> for String {
    fn as_ref(&self) -> &RelativePath {
        RelativePath::new(self)
    }
}

impl AsRef<RelativePath> for str {
    fn as_ref(&self) -> &RelativePath {
        RelativePath::new(self)
    }
}

impl AsRef<RelativePath> for RelativePath {
    fn as_ref(&self) -> &RelativePath {
        self
    }
}

impl cmp::PartialEq for RelativePath {
    fn eq(&self, other: &RelativePath) -> bool {
        self.components() == other.components()
    }
}

impl cmp::Eq for RelativePath {}

impl cmp::PartialOrd for RelativePath {
    fn partial_cmp(&self, other: &RelativePath) -> Option<cmp::Ordering> {
        self.components().partial_cmp(other.components())
    }
}

impl cmp::Ord for RelativePath {
    fn cmp(&self, other: &RelativePath) -> cmp::Ordering {
        self.components().cmp(other.components())
    }
}

impl Hash for RelativePath {
    fn hash<H: Hasher>(&self, h: &mut H) {
        for c in self.components() {
            c.hash(h);
        }
    }
}

impl fmt::Display for RelativePath {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

impl fmt::Display for RelativePathBuf {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

/// Helper struct for printing relative paths.
///
/// This is not strictly necessary in the same sense as it is for [`Display`],
/// because relative paths are guaranteed to be valid UTF-8. But the behavior is
/// preserved to simplify the transition between [`Path`] and [`RelativePath`].
///
/// [`Path`]: std::path::Path
/// [`Display`]: std::fmt::Display
pub struct Display<'a> {
    path: &'a RelativePath,
}

impl<'a> fmt::Debug for Display<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(&self.path, f)
    }
}

impl<'a> fmt::Display for Display<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.path, f)
    }
}

#[cfg(feature = "serde")]
impl serde::ser::Serialize for RelativePathBuf {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        serializer.serialize_str(&self.inner)
    }
}

#[cfg(feature = "serde")]
impl<'de> serde::de::Deserialize<'de> for RelativePathBuf {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        struct RelativePathBufVisitor;

        impl<'de> serde::de::Visitor<'de> for RelativePathBufVisitor {
            type Value = RelativePathBuf;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a relative path")
            }

            fn visit_string<E>(self, input: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(RelativePathBuf::from(input))
            }

            fn visit_str<E>(self, input: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(RelativePathBuf::from(input.to_owned()))
            }
        }

        deserializer.deserialize_any(RelativePathBufVisitor)
    }
}

#[cfg(feature = "serde")]
impl serde::ser::Serialize for RelativePath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        serializer.serialize_str(&self.inner)
    }
}

macro_rules! impl_cmp {
    ($lhs:ty, $rhs:ty) => {
        impl<'a, 'b> PartialEq<$rhs> for $lhs {
            #[inline]
            fn eq(&self, other: &$rhs) -> bool {
                <RelativePath as PartialEq>::eq(self, other)
            }
        }

        impl<'a, 'b> PartialEq<$lhs> for $rhs {
            #[inline]
            fn eq(&self, other: &$lhs) -> bool {
                <RelativePath as PartialEq>::eq(self, other)
            }
        }

        impl<'a, 'b> PartialOrd<$rhs> for $lhs {
            #[inline]
            fn partial_cmp(&self, other: &$rhs) -> Option<cmp::Ordering> {
                <RelativePath as PartialOrd>::partial_cmp(self, other)
            }
        }

        impl<'a, 'b> PartialOrd<$lhs> for $rhs {
            #[inline]
            fn partial_cmp(&self, other: &$lhs) -> Option<cmp::Ordering> {
                <RelativePath as PartialOrd>::partial_cmp(self, other)
            }
        }
    };
}

impl_cmp!(RelativePathBuf, RelativePath);
impl_cmp!(RelativePathBuf, &'a RelativePath);
impl_cmp!(Cow<'a, RelativePath>, RelativePath);
impl_cmp!(Cow<'a, RelativePath>, &'b RelativePath);
impl_cmp!(Cow<'a, RelativePath>, RelativePathBuf);

macro_rules! impl_cmp_str {
    ($lhs:ty, $rhs:ty) => {
        impl<'a, 'b> PartialEq<$rhs> for $lhs {
            #[inline]
            fn eq(&self, other: &$rhs) -> bool {
                <RelativePath as PartialEq>::eq(self, other.as_ref())
            }
        }

        impl<'a, 'b> PartialEq<$lhs> for $rhs {
            #[inline]
            fn eq(&self, other: &$lhs) -> bool {
                <RelativePath as PartialEq>::eq(self.as_ref(), other)
            }
        }

        impl<'a, 'b> PartialOrd<$rhs> for $lhs {
            #[inline]
            fn partial_cmp(&self, other: &$rhs) -> Option<cmp::Ordering> {
                <RelativePath as PartialOrd>::partial_cmp(self, other.as_ref())
            }
        }

        impl<'a, 'b> PartialOrd<$lhs> for $rhs {
            #[inline]
            fn partial_cmp(&self, other: &$lhs) -> Option<cmp::Ordering> {
                <RelativePath as PartialOrd>::partial_cmp(self.as_ref(), other)
            }
        }
    };
}

impl_cmp_str!(RelativePathBuf, str);
impl_cmp_str!(RelativePathBuf, &'a str);
impl_cmp_str!(RelativePathBuf, String);
impl_cmp_str!(RelativePath, str);
impl_cmp_str!(RelativePath, &'a str);
impl_cmp_str!(RelativePath, String);
impl_cmp_str!(&'a RelativePath, str);
impl_cmp_str!(&'a RelativePath, String);

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;
    use std::rc::Rc;
    use std::sync::Arc;

    macro_rules! t(
        ($path:expr, iter: $iter:expr) => (
            {
                let path = RelativePath::new($path);

                // Forward iteration
                let comps = path.iter().map(str::to_string).collect::<Vec<String>>();
                let exp: &[&str] = &$iter;
                let exps = exp.iter().map(|s| s.to_string()).collect::<Vec<String>>();
                assert!(comps == exps, "iter: Expected {:?}, found {:?}",
                        exps, comps);

                // Reverse iteration
                let comps = RelativePath::new($path).iter().rev().map(str::to_string)
                    .collect::<Vec<String>>();
                let exps = exps.into_iter().rev().collect::<Vec<String>>();
                assert!(comps == exps, "iter().rev(): Expected {:?}, found {:?}",
                        exps, comps);
            }
        );

        ($path:expr, parent: $parent:expr, file_name: $file:expr) => (
            {
                let path = RelativePath::new($path);

                let parent = path.parent().map(|p| p.as_str());
                let exp_parent: Option<&str> = $parent;
                assert!(parent == exp_parent, "parent: Expected {:?}, found {:?}",
                        exp_parent, parent);

                let file = path.file_name();
                let exp_file: Option<&str> = $file;
                assert!(file == exp_file, "file_name: Expected {:?}, found {:?}",
                        exp_file, file);
            }
        );

        ($path:expr, file_stem: $file_stem:expr, extension: $extension:expr) => (
            {
                let path = RelativePath::new($path);

                let stem = path.file_stem();
                let exp_stem: Option<&str> = $file_stem;
                assert!(stem == exp_stem, "file_stem: Expected {:?}, found {:?}",
                        exp_stem, stem);

                let ext = path.extension();
                let exp_ext: Option<&str> = $extension;
                assert!(ext == exp_ext, "extension: Expected {:?}, found {:?}",
                        exp_ext, ext);
            }
        );

        ($path:expr, iter: $iter:expr,
                     parent: $parent:expr, file_name: $file:expr,
                     file_stem: $file_stem:expr, extension: $extension:expr) => (
            {
                t!($path, iter: $iter);
                t!($path, parent: $parent, file_name: $file);
                t!($path, file_stem: $file_stem, extension: $extension);
            }
        );
    );

    fn assert_components(components: &[&str], path: &RelativePath) {
        let components = components
            .iter()
            .cloned()
            .map(Component::Normal)
            .collect::<Vec<_>>();
        let result: Vec<_> = path.components().collect();
        assert_eq!(&components[..], &result[..]);
    }

    fn rp(input: &str) -> &RelativePath {
        RelativePath::new(input)
    }

    #[test]
    pub fn test_decompositions() {
        t!("",
        iter: [],
        parent: None,
        file_name: None,
        file_stem: None,
        extension: None
        );

        t!("foo",
        iter: ["foo"],
        parent: Some(""),
        file_name: Some("foo"),
        file_stem: Some("foo"),
        extension: None
        );

        t!("/",
        iter: [],
        parent: Some(""),
        file_name: None,
        file_stem: None,
        extension: None
        );

        t!("/foo",
        iter: ["foo"],
        parent: Some(""),
        file_name: Some("foo"),
        file_stem: Some("foo"),
        extension: None
        );

        t!("foo/",
        iter: ["foo"],
        parent: Some(""),
        file_name: Some("foo"),
        file_stem: Some("foo"),
        extension: None
        );

        t!("/foo/",
        iter: ["foo"],
        parent: Some(""),
        file_name: Some("foo"),
        file_stem: Some("foo"),
        extension: None
        );

        t!("foo/bar",
        iter: ["foo", "bar"],
        parent: Some("foo"),
        file_name: Some("bar"),
        file_stem: Some("bar"),
        extension: None
        );

        t!("/foo/bar",
        iter: ["foo", "bar"],
        parent: Some("/foo"),
        file_name: Some("bar"),
        file_stem: Some("bar"),
        extension: None
        );

        t!("///foo///",
        iter: ["foo"],
        parent: Some(""),
        file_name: Some("foo"),
        file_stem: Some("foo"),
        extension: None
        );

        t!("///foo///bar",
        iter: ["foo", "bar"],
        parent: Some("///foo"),
        file_name: Some("bar"),
        file_stem: Some("bar"),
        extension: None
        );

        t!("./.",
        iter: [".", "."],
        parent: Some(""),
        file_name: None,
        file_stem: None,
        extension: None
        );

        t!("/..",
        iter: [".."],
        parent: Some(""),
        file_name: None,
        file_stem: None,
        extension: None
        );

        t!("../",
        iter: [".."],
        parent: Some(""),
        file_name: None,
        file_stem: None,
        extension: None
        );

        t!("foo/.",
        iter: ["foo", "."],
        parent: Some(""),
        file_name: Some("foo"),
        file_stem: Some("foo"),
        extension: None
        );

        t!("foo/..",
        iter: ["foo", ".."],
        parent: Some("foo"),
        file_name: None,
        file_stem: None,
        extension: None
        );

        t!("foo/./",
        iter: ["foo", "."],
        parent: Some(""),
        file_name: Some("foo"),
        file_stem: Some("foo"),
        extension: None
        );

        t!("foo/./bar",
        iter: ["foo", ".", "bar"],
        parent: Some("foo/."),
        file_name: Some("bar"),
        file_stem: Some("bar"),
        extension: None
        );

        t!("foo/../",
        iter: ["foo", ".."],
        parent: Some("foo"),
        file_name: None,
        file_stem: None,
        extension: None
        );

        t!("foo/../bar",
        iter: ["foo", "..", "bar"],
        parent: Some("foo/.."),
        file_name: Some("bar"),
        file_stem: Some("bar"),
        extension: None
        );

        t!("./a",
        iter: [".", "a"],
        parent: Some("."),
        file_name: Some("a"),
        file_stem: Some("a"),
        extension: None
        );

        t!(".",
        iter: ["."],
        parent: Some(""),
        file_name: None,
        file_stem: None,
        extension: None
        );

        t!("./",
        iter: ["."],
        parent: Some(""),
        file_name: None,
        file_stem: None,
        extension: None
        );

        t!("a/b",
        iter: ["a", "b"],
        parent: Some("a"),
        file_name: Some("b"),
        file_stem: Some("b"),
        extension: None
        );

        t!("a//b",
        iter: ["a", "b"],
        parent: Some("a"),
        file_name: Some("b"),
        file_stem: Some("b"),
        extension: None
        );

        t!("a/./b",
        iter: ["a", ".", "b"],
        parent: Some("a/."),
        file_name: Some("b"),
        file_stem: Some("b"),
        extension: None
        );

        t!("a/b/c",
        iter: ["a", "b", "c"],
        parent: Some("a/b"),
        file_name: Some("c"),
        file_stem: Some("c"),
        extension: None
        );

        t!(".foo",
        iter: [".foo"],
        parent: Some(""),
        file_name: Some(".foo"),
        file_stem: Some(".foo"),
        extension: None
        );
    }

    #[test]
    pub fn test_stem_ext() {
        t!("foo",
        file_stem: Some("foo"),
        extension: None
        );

        t!("foo.",
        file_stem: Some("foo"),
        extension: Some("")
        );

        t!(".foo",
        file_stem: Some(".foo"),
        extension: None
        );

        t!("foo.txt",
        file_stem: Some("foo"),
        extension: Some("txt")
        );

        t!("foo.bar.txt",
        file_stem: Some("foo.bar"),
        extension: Some("txt")
        );

        t!("foo.bar.",
        file_stem: Some("foo.bar"),
        extension: Some("")
        );

        t!(".", file_stem: None, extension: None);

        t!("..", file_stem: None, extension: None);

        t!("", file_stem: None, extension: None);
    }

    #[test]
    pub fn test_set_file_name() {
        macro_rules! tfn(
                ($path:expr, $file:expr, $expected:expr) => ( {
                let mut p = RelativePathBuf::from($path);
                p.set_file_name($file);
                assert!(p.as_str() == $expected,
                        "setting file name of {:?} to {:?}: Expected {:?}, got {:?}",
                        $path, $file, $expected,
                        p.as_str());
            });
        );

        tfn!("foo", "foo", "foo");
        tfn!("foo", "bar", "bar");
        tfn!("foo", "", "");
        tfn!("", "foo", "foo");

        tfn!(".", "foo", "./foo");
        tfn!("foo/", "bar", "bar");
        tfn!("foo/.", "bar", "bar");
        tfn!("..", "foo", "../foo");
        tfn!("foo/..", "bar", "foo/../bar");
        tfn!("/", "foo", "/foo");
    }

    #[test]
    pub fn test_set_extension() {
        macro_rules! tse(
                ($path:expr, $ext:expr, $expected:expr, $output:expr) => ( {
                let mut p = RelativePathBuf::from($path);
                let output = p.set_extension($ext);
                assert!(p.as_str() == $expected && output == $output,
                        "setting extension of {:?} to {:?}: Expected {:?}/{:?}, got {:?}/{:?}",
                        $path, $ext, $expected, $output,
                        p.as_str(), output);
            });
        );

        tse!("foo", "txt", "foo.txt", true);
        tse!("foo.bar", "txt", "foo.txt", true);
        tse!("foo.bar.baz", "txt", "foo.bar.txt", true);
        tse!(".test", "txt", ".test.txt", true);
        tse!("foo.txt", "", "foo", true);
        tse!("foo", "", "foo", true);
        tse!("", "foo", "", false);
        tse!(".", "foo", ".", false);
        tse!("foo/", "bar", "foo.bar", true);
        tse!("foo/.", "bar", "foo.bar", true);
        tse!("..", "foo", "..", false);
        tse!("foo/..", "bar", "foo/..", false);
        tse!("/", "foo", "/", false);
    }

    #[test]
    fn test_eq_recievers() {
        use std::borrow::Cow;

        let borrowed: &RelativePath = RelativePath::new("foo/bar");
        let mut owned: RelativePathBuf = RelativePathBuf::new();
        owned.push("foo");
        owned.push("bar");
        let borrowed_cow: Cow<RelativePath> = borrowed.into();
        let owned_cow: Cow<RelativePath> = owned.clone().into();

        macro_rules! t {
            ($($current:expr),+) => {
                $(
                    assert_eq!($current, borrowed);
                    assert_eq!($current, owned);
                    assert_eq!($current, borrowed_cow);
                    assert_eq!($current, owned_cow);
                )+
            }
        }

        t!(borrowed, owned, borrowed_cow, owned_cow);
    }

    #[test]
    pub fn test_compare() {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        fn hash<T: Hash>(t: T) -> u64 {
            let mut s = DefaultHasher::new();
            t.hash(&mut s);
            s.finish()
        }

        macro_rules! tc(
            ($path1:expr, $path2:expr, eq: $eq:expr,
             starts_with: $starts_with:expr, ends_with: $ends_with:expr,
             relative_from: $relative_from:expr) => ({
                 let path1 = RelativePath::new($path1);
                 let path2 = RelativePath::new($path2);

                 let eq = path1 == path2;
                 assert!(eq == $eq, "{:?} == {:?}, expected {:?}, got {:?}",
                         $path1, $path2, $eq, eq);
                 assert!($eq == (hash(path1) == hash(path2)),
                         "{:?} == {:?}, expected {:?}, got {} and {}",
                         $path1, $path2, $eq, hash(path1), hash(path2));

                 let starts_with = path1.starts_with(path2);
                 assert!(starts_with == $starts_with,
                         "{:?}.starts_with({:?}), expected {:?}, got {:?}", $path1, $path2,
                         $starts_with, starts_with);

                 let ends_with = path1.ends_with(path2);
                 assert!(ends_with == $ends_with,
                         "{:?}.ends_with({:?}), expected {:?}, got {:?}", $path1, $path2,
                         $ends_with, ends_with);

                 let relative_from = path1.strip_prefix(path2)
                                          .map(|p| p.as_str())
                                          .ok();
                 let exp: Option<&str> = $relative_from;
                 assert!(relative_from == exp,
                         "{:?}.strip_prefix({:?}), expected {:?}, got {:?}",
                         $path1, $path2, exp, relative_from);
            });
        );

        tc!("", "",
        eq: true,
        starts_with: true,
        ends_with: true,
        relative_from: Some("")
        );

        tc!("foo", "",
        eq: false,
        starts_with: true,
        ends_with: true,
        relative_from: Some("foo")
        );

        tc!("", "foo",
        eq: false,
        starts_with: false,
        ends_with: false,
        relative_from: None
        );

        tc!("foo", "foo",
        eq: true,
        starts_with: true,
        ends_with: true,
        relative_from: Some("")
        );

        tc!("foo/", "foo",
        eq: true,
        starts_with: true,
        ends_with: true,
        relative_from: Some("")
        );

        tc!("foo/bar", "foo",
        eq: false,
        starts_with: true,
        ends_with: false,
        relative_from: Some("bar")
        );

        tc!("foo/bar/baz", "foo/bar",
        eq: false,
        starts_with: true,
        ends_with: false,
        relative_from: Some("baz")
        );

        tc!("foo/bar", "foo/bar/baz",
        eq: false,
        starts_with: false,
        ends_with: false,
        relative_from: None
        );
    }

    #[test]
    fn test_join() {
        assert_components(&["foo", "bar", "baz"], &rp("foo/bar").join("baz///"));
        assert_components(
            &["hello", "world", "foo", "bar", "baz"],
            &rp("hello/world").join("///foo/bar/baz"),
        );
        assert_components(&["foo", "bar", "baz"], &rp("").join("foo/bar/baz"));
    }

    #[test]
    fn test_components_iterator() {
        use self::Component::*;

        assert_eq!(
            vec![Normal("hello"), Normal("world")],
            rp("/hello///world//").components().collect::<Vec<_>>()
        );
    }

    #[test]
    fn test_to_path_buf() {
        let path = rp("/hello///world//");
        let path_buf = path.to_path(".");
        let expected = Path::new(".").join("hello").join("world");
        assert_eq!(expected, path_buf);
    }

    #[test]
    fn test_eq() {
        assert_eq!(rp("//foo///bar"), rp("/foo/bar"));
        assert_eq!(rp("foo///bar"), rp("foo/bar"));
        assert_eq!(rp("foo"), rp("foo"));
        assert_eq!(rp("foo"), rp("foo").to_relative_path_buf());
    }

    #[test]
    fn test_next_back() {
        use self::Component::*;

        let mut it = rp("baz/bar///foo").components();
        assert_eq!(Some(Normal("foo")), it.next_back());
        assert_eq!(Some(Normal("bar")), it.next_back());
        assert_eq!(Some(Normal("baz")), it.next_back());
        assert_eq!(None, it.next_back());
    }

    #[test]
    fn test_parent() {
        let path = rp("baz/./bar/foo//./.");

        assert_eq!(Some(rp("baz/./bar")), path.parent());
        assert_eq!(
            Some(rp("baz/.")),
            path.parent().and_then(RelativePath::parent)
        );
        assert_eq!(
            Some(rp("")),
            path.parent()
                .and_then(RelativePath::parent)
                .and_then(RelativePath::parent)
        );
        assert_eq!(
            None,
            path.parent()
                .and_then(RelativePath::parent)
                .and_then(RelativePath::parent)
                .and_then(RelativePath::parent)
        );
    }

    #[test]
    fn test_relative_path_buf() {
        assert_eq!(
            rp("hello/world/."),
            rp("/hello///world//").to_owned().join(".")
        );
    }

    #[test]
    fn test_normalize() {
        assert_eq!(rp("c/d"), rp("a/.././b/../c/d").normalize());
    }

    #[test]
    fn test_relative_to() {
        assert_eq!(
            rp("foo/foo/bar"),
            rp("foo/bar").join_normalized("../foo/bar")
        );

        assert_eq!(
            rp("../c/e"),
            rp("x/y").join_normalized("../../a/b/../../../c/d/../e")
        );
    }

    #[test]
    fn test_from() {
        assert_eq!(
            rp("foo/bar").to_owned(),
            RelativePathBuf::from(String::from("foo/bar")),
        );

        assert_eq!(rp("foo/bar").to_owned(), RelativePathBuf::from("foo/bar"),);

        assert_eq!(&*Box::<RelativePath>::from(rp("foo/bar")), rp("foo/bar"));
        assert_eq!(
            &*Box::<RelativePath>::from(RelativePathBuf::from("foo/bar")),
            rp("foo/bar")
        );

        assert_eq!(&*Arc::<RelativePath>::from(rp("foo/bar")), rp("foo/bar"));
        assert_eq!(
            &*Arc::<RelativePath>::from(RelativePathBuf::from("foo/bar")),
            rp("foo/bar")
        );

        assert_eq!(&*Rc::<RelativePath>::from(rp("foo/bar")), rp("foo/bar"));
        assert_eq!(
            &*Rc::<RelativePath>::from(RelativePathBuf::from("foo/bar")),
            rp("foo/bar")
        );
    }

    #[test]
    fn test_default() {
        assert_eq!(RelativePathBuf::new(), RelativePathBuf::default(),);
    }

    #[test]
    pub fn test_push() {
        macro_rules! tp(
            ($path:expr, $push:expr, $expected:expr) => ( {
                let mut actual = RelativePathBuf::from($path);
                actual.push($push);
                assert!(actual.as_str() == $expected,
                        "pushing {:?} onto {:?}: Expected {:?}, got {:?}",
                        $push, $path, $expected, actual.as_str());
            });
        );

        tp!("", "foo", "foo");
        tp!("foo", "bar", "foo/bar");
        tp!("foo/", "bar", "foo/bar");
        tp!("foo//", "bar", "foo//bar");
        tp!("foo/.", "bar", "foo/./bar");
        tp!("foo./.", "bar", "foo././bar");
        tp!("foo", "", "foo/");
        tp!("foo", ".", "foo/.");
        tp!("foo", "..", "foo/..");
    }

    #[test]
    pub fn test_pop() {
        macro_rules! tp(
            ($path:expr, $expected:expr, $output:expr) => ( {
                let mut actual = RelativePathBuf::from($path);
                let output = actual.pop();
                assert!(actual.as_str() == $expected && output == $output,
                        "popping from {:?}: Expected {:?}/{:?}, got {:?}/{:?}",
                        $path, $expected, $output,
                        actual.as_str(), output);
            });
        );

        tp!("", "", false);
        tp!("/", "", true);
        tp!("foo", "", true);
        tp!(".", "", true);
        tp!("/foo", "", true);
        tp!("/foo/bar", "/foo", true);
        tp!("/foo/bar/.", "/foo", true);
        tp!("foo/bar", "foo", true);
        tp!("foo/.", "", true);
        tp!("foo//bar", "foo", true);
    }

    #[test]
    pub fn test_display() {
        // NB: display delegated to the underlying string.
        assert_eq!(RelativePathBuf::from("foo/bar").to_string(), "foo/bar");
        assert_eq!(RelativePath::new("foo/bar").to_string(), "foo/bar");

        assert_eq!(format!("{}", RelativePathBuf::from("foo/bar")), "foo/bar");
        assert_eq!(format!("{}", RelativePath::new("foo/bar")), "foo/bar");
    }

    #[cfg(unix)]
    #[test]
    pub fn test_unix_from_path() {
        use std::ffi::OsStr;
        use std::os::unix::ffi::OsStrExt;

        assert_eq!(
            Err(FromPathErrorKind::NonRelative.into()),
            RelativePath::from_path("/foo/bar")
        );

        // Continuation byte without continuation.
        let non_utf8 = OsStr::from_bytes(&[0x80u8]);

        assert_eq!(
            Err(FromPathErrorKind::NonUtf8.into()),
            RelativePath::from_path(non_utf8)
        );
    }

    #[cfg(windows)]
    #[test]
    pub fn test_windows_from_path() {
        assert_eq!(
            Err(FromPathErrorKind::NonRelative.into()),
            RelativePath::from_path("c:\\foo\\bar")
        );

        assert_eq!(
            Err(FromPathErrorKind::BadSeparator.into()),
            RelativePath::from_path("foo\\bar")
        );
    }

    #[cfg(unix)]
    #[test]
    pub fn test_unix_owned_from_path() {
        use std::ffi::OsStr;
        use std::os::unix::ffi::OsStrExt;

        assert_eq!(
            Err(FromPathErrorKind::NonRelative.into()),
            RelativePathBuf::from_path(Path::new("/foo/bar"))
        );

        // Continuation byte without continuation.
        let non_utf8 = OsStr::from_bytes(&[0x80u8]);

        assert_eq!(
            Err(FromPathErrorKind::NonUtf8.into()),
            RelativePathBuf::from_path(Path::new(non_utf8))
        );
    }

    #[cfg(windows)]
    #[test]
    pub fn test_windows_owned_from_path() {
        assert_eq!(
            Err(FromPathErrorKind::NonRelative.into()),
            RelativePathBuf::from_path(Path::new("c:\\foo\\bar"))
        );
    }
}
