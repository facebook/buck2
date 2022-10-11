/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Borrow;

use crate::dupe::Dupe;
use crate::types::TEq;

/// Optimised collect iterator into Vec, which might be a Result.
///
/// If we do a standard .collect() on the iterator it will never have a good size hint,
/// as the lower bound will always be zero, so might reallocate several times.
/// We know the Vec will either be thrown away, or exactly `len`, so aim if we do allocate,
/// make sure it is at `len`. However, if the first element throws an error, we don't need
/// to allocate at all, so special case that.
fn collect_result<T, E>(mut it: impl ExactSizeIterator<Item = Result<T, E>>) -> Result<Vec<T>, E> {
    match it.next() {
        None => Ok(Vec::new()),
        Some(Err(e)) => Err(e),
        Some(Ok(x)) => {
            // +1 for the element we have already consumed
            let mut res = Vec::with_capacity(it.len() + 1);
            res.push(x);
            for x in it {
                res.push(x?);
            }
            Ok(res)
        }
    }
}

/// Extension traits on slices/[`Vec`](Vec).
pub trait SliceExt {
    type Item;

    /// A shorthand for `iter().map(f).collect::<Vec<_>>()`. For example:
    ///
    /// ```
    /// use gazebo::prelude::*;
    /// assert_eq!([1,2,3][..].map(|x| x*x), vec![1,4,9]);
    /// assert_eq!(vec![1,2,3].map(|x| x*x), vec![1,4,9]);
    /// ```
    ///
    /// Note that from Rust 1.55.0 there is a `map` method on
    /// arrays (e.g. `[T; N]`) so you'll need to explicitly convert
    /// arrays to slices with the `[..]` operation.
    fn map<'a, B, F>(&'a self, f: F) -> Vec<B>
    where
        F: FnMut(&'a Self::Item) -> B;

    /// A shorthand for `iter().map(f).collect::<Result<Vec<_>, _>>()`. For example:
    ///
    /// ```
    /// use gazebo::prelude::*;
    /// assert_eq!([1,2,3].try_map(|x| Ok(x*x)), Ok::<_, bool>(vec![1,4,9]));
    /// assert_eq!([1,2,-3].try_map(|x| if *x > 0 { Ok(x*x) } else { Err(false) }), Err(false));
    /// ```
    ///
    /// This function will be generalised to [`Try`](std::ops::Try) once it has been
    /// standardised.
    fn try_map<'a, B, E, F>(&'a self, f: F) -> Result<Vec<B>, E>
    where
        F: FnMut(&'a Self::Item) -> Result<B, E>;

    /// Take ownership of each item in the vector using `to_owned`. For example:
    ///
    /// ```
    /// use gazebo::prelude::*;
    /// let xs: &[&str] = &["hello", "world"];
    /// let ys: Vec<String> = xs.owned();
    /// ```
    fn owned<'a, T, R>(&'a self) -> Vec<R>
    where
        // Important constraints are:
        // * Self::Item == &'a T
        // * Borrow<T> == R
        Self::Item: TEq<&'a T>,
        R: Borrow<T>,
        T: ToOwned<Owned = R>,
        T: 'a,
        T: ?Sized,
    {
        self.map(|x| (*x.teq_ref()).to_owned())
    }

    /// If the size of vector is 1, returns the first element
    /// Otherwise, returns None
    /// ```
    /// use gazebo::prelude::*;
    /// assert_eq!(*vec![1].as_singleton().unwrap(), 1);
    /// assert_eq!(vec!['a', 'b', 'c'].as_singleton(), None);
    fn as_singleton(&self) -> Option<&Self::Item>;

    /// Copies the elements from `src` into `self`, analogous to `clone_from_slice` but for
    /// elements that are `dupe`.
    ///
    /// The length of `src` must be the same as `self`.
    ///
    /// If `T` implements `Copy`, it can be more performant to use [`std::slice::[T]::copy_from_slice`].
    ///
    /// ```
    /// use gazebo::prelude::*;
    ///
    /// let src = [1, 2, 3, 4];
    /// let mut dst = [0, 0];
    ///
    /// dst.dupe_from_slice(&src[2..]);
    /// assert_eq!(src, [1, 2, 3, 4]);
    /// assert_eq!(dst, [3, 4]);
    /// ```
    fn dupe_from_slice(&mut self, src: &[Self::Item])
    where
        Self::Item: Dupe;
}

impl<T> SliceExt for [T] {
    type Item = T;

    fn map<'a, B, F>(&'a self, f: F) -> Vec<B>
    where
        F: FnMut(&'a Self::Item) -> B,
    {
        self.iter().map(f).collect()
    }

    fn try_map<'a, B, E, F>(&'a self, f: F) -> Result<Vec<B>, E>
    where
        F: FnMut(&'a Self::Item) -> Result<B, E>,
    {
        collect_result(self.iter().map(f))
    }

    fn as_singleton(&self) -> Option<&T> {
        match self {
            [x] => Some(x),
            _ => None,
        }
    }

    fn dupe_from_slice(&mut self, src: &[Self::Item])
    where
        Self::Item: Dupe,
    {
        self.clone_from_slice(src)
    }
}

// TODO(bobyf) merge these traits if we can properly implement linters without requiring separate traits
/// Short hand analogous to `Iter::cloned`, where items of `&T` are converted to `T` via clone.
///
/// ```
/// use gazebo::prelude::*;
///
/// #[derive(Clone, Debug, PartialEq)]
/// struct X;
///
/// let x = [&X];
/// let y : Vec<X> = x.cloned();
///
/// assert_eq!(y, vec![X]);
///
/// let x = vec![&X];
/// let y : Vec<X> = x.cloned();
///
/// assert_eq!(y, vec![X]);
/// ```
pub trait SliceClonedExt {
    type Item;

    fn cloned(&self) -> Vec<Self::Item>;
}

impl<T> SliceClonedExt for [&T]
where
    T: Clone,
{
    type Item = T;

    fn cloned(&self) -> Vec<Self::Item> {
        self.map(|x| (*x).clone())
    }
}

/// Short hand analogous to `Iter::duped`, where items of `&T` are converted to `T` via `dupe`.
///
/// ```
/// use gazebo::prelude::*;
///
/// #[derive(Clone, Dupe, Debug, PartialEq)]
/// struct X;
///
/// let x = [&X];
/// let y : Vec<X> = x.duped();
///
/// assert_eq!(y, vec![X]);
///
/// let x = vec![&X];
/// let y : Vec<X> = x.duped();
///
/// assert_eq!(y, vec![X]);
/// ```
pub trait SliceDupedExt {
    type Item;

    fn duped(&self) -> Vec<Self::Item>;
}

impl<T> SliceDupedExt for [&T]
where
    T: Dupe,
{
    type Item = T;

    fn duped(&self) -> Vec<Self::Item> {
        self.map(|x| (*x).dupe())
    }
}

/// Short hand analogous to `Iter::copied`, where items of `&T` are converted to `T` via `copy`.
///
/// ```
/// use gazebo::prelude::*;
///
/// #[derive(Copy, Clone, Debug, PartialEq)]
/// struct X;
///
/// let x = [&X];
/// let y : Vec<X> = x.copied();
///
/// assert_eq!(y, vec![X]);
///
/// let x = vec![&X];
/// let y : Vec<X> = x.copied();
///
/// assert_eq!(y, vec![X]);
/// ```
pub trait SliceCopiedExt {
    type Item;

    fn copied(&self) -> Vec<Self::Item>;
}

impl<T> SliceCopiedExt for [&T]
where
    T: Copy,
{
    type Item = T;

    fn copied(&self) -> Vec<Self::Item> {
        self.map(|x| **x)
    }
}

/// Extension traits on [`Vec`](Vec).
pub trait VecExt {
    type Item;

    /// A shorthand for `into_iter().map(f).collect::<Vec<_>>()`. For example:
    ///
    /// ```
    /// use gazebo::prelude::*;
    /// assert_eq!(vec![1,2,3].into_map(|x| x*x), vec![1,4,9]);
    /// ```
    fn into_map<B, F>(self, f: F) -> Vec<B>
    where
        F: FnMut(Self::Item) -> B;

    /// A shorthand for `into_iter().map(f).collect::<Result<Vec<_>, _>>()`. For example:
    ///
    /// ```
    /// use gazebo::prelude::*;
    /// assert_eq!(vec![1,2,3].into_try_map(|x| Ok(x*x)), Ok::<_, bool>(vec![1,4,9]));
    /// assert_eq!(vec![1,2,-3].into_try_map(|x| if x > 0 { Ok(x*x) } else { Err(false) }), Err(false));
    /// ```
    ///
    /// This function will be generalised to [`Try`](std::ops::Try) once it has been
    /// standardised.
    fn into_try_map<B, E, F>(self, f: F) -> Result<Vec<B>, E>
    where
        F: FnMut(Self::Item) -> Result<B, E>;
}

impl<T> VecExt for Vec<T> {
    type Item = T;

    fn into_map<B, F>(self, f: F) -> Vec<B>
    where
        F: FnMut(Self::Item) -> B,
    {
        self.into_iter().map(f).collect()
    }

    fn into_try_map<B, E, F>(self, f: F) -> Result<Vec<B>, E>
    where
        F: FnMut(Self::Item) -> Result<B, E>,
    {
        collect_result(self.into_iter().map(f))
    }
}
