/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

/// Extension traits on [`Iterator`](Iterator).
pub trait IterExt {
    type Item;

    /// If this iterator contains a single element, return it. Otherwise, return `None`.
    ///
    /// ```
    /// use gazebo::prelude::*;
    ///
    /// let i = vec![1];
    /// assert_eq!(i.into_iter().into_singleton(), Some(1));
    ///
    /// let i = Vec::<i64>::new();
    /// assert_eq!(i.into_iter().into_singleton(), None);
    ///
    /// let i = vec![1, 2];
    /// assert_eq!(i.into_iter().into_singleton(), None);
    /// ```
    fn into_singleton(self) -> Option<Self::Item>
    where
        Self: Sized;
}

pub trait IterOwned: Sized {
    /// Calls `to_owned()` on all the items provided by the inner Iterator.
    ///
    /// ```
    /// use gazebo::prelude::*;
    ///
    /// let inputs = vec!["a", "b", "c"];
    /// let outputs = inputs.into_iter().owned().collect::<Vec<_>>();
    /// assert_eq!(
    ///     outputs,
    ///     vec!["a".to_owned(), "b".to_owned(), "c".to_owned()]
    /// )
    /// ```
    fn owned(self) -> Owned<Self>;
}

impl<I> IterExt for I
where
    I: Iterator,
{
    type Item = I::Item;

    fn into_singleton(mut self) -> Option<Self::Item>
    where
        Self: Sized,
    {
        let ret = self.next()?;
        if self.next().is_some() {
            return None;
        }
        Some(ret)
    }
}

impl<'a, I, T> IterOwned for I
where
    I: Iterator<Item = &'a T> + Sized,
    T: 'a + ToOwned + ?Sized,
{
    fn owned(self) -> Owned<Self> {
        Owned { inner: self }
    }
}

/// An Iterator that yields the Owned variants of the inner iterator's items.
pub struct Owned<I> {
    inner: I,
}

impl<'a, I, T> Iterator for Owned<I>
where
    I: Iterator<Item = &'a T>,
    T: 'a + ToOwned + ?Sized,
{
    type Item = <T as ToOwned>::Owned;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.inner.next()?.to_owned())
    }
}
