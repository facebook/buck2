/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cmp::Ordering;
use std::iter::Cloned;

use crate::dupe::Dupe;

/// Extension traits on [`Iterator`](Iterator).
pub trait IterExt {
    type Item;

    /// Like `any`, except allow the function supplied to return a `Result` type, where we `Err`
    /// on the first encounter of `Err`.
    ///
    /// ```
    /// use gazebo::prelude::*;
    ///
    /// fn true_if_even_throw_on_zero(x: &usize) -> Result<bool, ()> {
    ///     if *x == 0 {
    ///         Err(())
    ///     } else {
    ///         Ok(x % 2 == 0)
    ///     }
    /// }
    ///
    /// let x = [1, 3, 2];
    /// assert_eq!(x.iter().try_any(true_if_even_throw_on_zero), Ok(true));
    ///
    /// let x = [1, 3, 5];
    /// assert_eq!(x.iter().try_any(true_if_even_throw_on_zero), Ok(false));
    ///
    /// let x = [1, 0, 2];
    /// assert_eq!(x.iter().try_any(true_if_even_throw_on_zero), Err(()));
    ///
    /// ```
    fn try_any<F, E>(self, any: F) -> Result<bool, E>
    where
        Self: Sized,
        F: FnMut(Self::Item) -> Result<bool, E>;

    /// Like `all`, except allow the function supplied to return a `Result` type, where we `Err`
    /// on the first encounter of `Err`.
    ///
    /// ```
    /// use gazebo::prelude::*;
    ///
    /// fn true_if_even_throw_on_zero(x: &usize) -> Result<bool, ()> {
    ///     if *x == 0 {
    ///         Err(())
    ///     } else {
    ///         Ok(x % 2 == 0)
    ///     }
    /// }
    ///
    /// let x = [2, 4, 2];
    /// assert_eq!(x.iter().try_all(true_if_even_throw_on_zero), Ok(true));
    ///
    /// let x = [1, 3, 5];
    /// assert_eq!(x.iter().try_all(true_if_even_throw_on_zero), Ok(false));
    ///
    /// let x = [2, 0, 2];
    /// assert_eq!(x.iter().try_all(true_if_even_throw_on_zero), Err(()));
    ///
    /// ```
    fn try_all<F, E>(self, any: F) -> Result<bool, E>
    where
        Self: Sized,
        F: FnMut(Self::Item) -> Result<bool, E>;

    /// Like `eq_by`, except allow the function supplied to return a `Result` type, where we `Err`
    /// on the first encounter of `Err`.
    ///
    /// ```
    /// use gazebo::prelude::*;
    ///
    /// fn double_eq_throw_on_zero(x: &usize, y: &usize) -> Result<bool, ()> {
    ///     if *x == 0 || *y == 0 {
    ///         Err(())
    ///     } else {
    ///         Ok(x * 2 == *y)
    ///     }
    /// }
    ///
    /// let x = [1, 4, 2];
    /// let y = [2, 8, 4];
    ///
    /// assert_eq!(x.iter().try_eq_by(&y, double_eq_throw_on_zero), Ok(true));
    ///
    /// let x = [1, 4, 2];
    /// let y = [2, 0, 4];
    ///
    /// assert_eq!(x.iter().try_eq_by(&y, double_eq_throw_on_zero), Err(()));
    /// ```
    fn try_eq_by<I, F, E>(self, other: I, eq: F) -> Result<bool, E>
    where
        Self: Sized,
        I: IntoIterator,
        F: FnMut(Self::Item, I::Item) -> Result<bool, E>;

    /// Like `cmp_by`, except allow the function supplied to return a `Result` type, where we `Err`
    /// on the first encounter of `Err`.
    ///
    /// ```
    /// use gazebo::prelude::*;
    /// use std::cmp::Ordering;
    ///
    /// fn double_cmp_throw_on_zero(x: &usize, y: &usize) -> Result<Ordering, ()> {
    ///     if *x == 0 || *y == 0 {
    ///         Err(())
    ///     } else {
    ///         Ok((x * 2).cmp(y))
    ///     }
    /// }
    ///
    /// let x = [1, 4, 2];
    /// let y = [2, 8, 4];
    ///
    /// assert_eq!(x.iter().try_cmp_by(&y, double_cmp_throw_on_zero), Ok(Ordering::Equal));
    ///
    /// let x = [1, 2, 2];
    /// let y = [2, 8, 4];
    ///
    /// assert_eq!(x.iter().try_cmp_by(&y, double_cmp_throw_on_zero), Ok(Ordering::Less));
    ///
    /// let x = [1, 4];
    /// let y = [2, 8, 4];
    ///
    /// assert_eq!(x.iter().try_cmp_by(&y, double_cmp_throw_on_zero), Ok(Ordering::Less));
    ///
    /// let x = [1, 4, 4];
    /// let y = [2, 8, 4];
    ///
    /// assert_eq!(x.iter().try_cmp_by(&y, double_cmp_throw_on_zero), Ok(Ordering::Greater));
    ///
    /// let x = [1, 4, 2, 3];
    /// let y = [2, 8, 4];
    ///
    /// assert_eq!(x.iter().try_cmp_by(&y, double_cmp_throw_on_zero), Ok(Ordering::Greater));
    ///
    /// let x = [1, 4, 2];
    /// let y = [2, 0, 4];
    ///
    /// assert_eq!(x.iter().try_cmp_by(&y, double_cmp_throw_on_zero), Err(()));
    /// ```
    fn try_cmp_by<I, F, E>(self, other: I, cmp: F) -> Result<Ordering, E>
    where
        Self: Sized,
        I: IntoIterator,
        F: FnMut(Self::Item, I::Item) -> Result<Ordering, E>;

    /// Like `unzip`, except allowing the current `Iterator` to contain `Result` type, where we `Err`
    /// on the first encounter of `Err`.
    ///
    /// ```
    /// use gazebo::prelude::*;
    ///
    /// let i = vec![Ok::<_, ()>((1, "a")), Ok((2, "b"))];
    ///
    /// assert_eq!(i.into_iter().try_unzip(), Ok((vec![1, 2], vec!["a", "b"])));
    ///
    /// let i = vec![Ok((1, "a")), Err(()), Ok((2, "b"))];
    ///
    /// assert_eq!(i.into_iter().try_unzip::<_, _, Vec<_>, Vec<_>, _>(), Err(()));
    /// ```
    fn try_unzip<A, B, FromA, FromB, E>(self) -> Result<(FromA, FromB), E>
    where
        FromA: Default + Extend<A>,
        FromB: Default + Extend<B>,
        Self: Iterator<Item = Result<(A, B), E>>;

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

pub trait IterDuped: Sized {
    /// Like `duped()`, but only works for types that implement `Dupe`.
    /// Note that the return type is deliberately `Cloned`, as that behaves
    /// the same as a `Duped` would be, but can take advantage of standard library
    /// optimisations.
    ///
    /// ```
    /// use gazebo::prelude::*;
    /// use std::rc::Rc;
    /// let inputs = vec![Rc::new("Hello"), Rc::new("World")];
    /// let outputs = inputs.iter().duped().collect::<Vec<_>>();
    /// assert_eq!(inputs, outputs);
    /// ```
    /// use gazebo::prelude::*;
    /// use std::cmp::Ordering;
    fn duped(self) -> Cloned<Self>;
}

pub trait IterOwned: Sized {
    /// Calls `to_owned()` on all the items provided by the inner Iterator.
    ///
    /// ```
    /// use gazebo::prelude::*;
    ///
    /// let inputs = vec!["a", "b", "c"];
    /// let outputs = inputs.into_iter().owned().collect::<Vec<_>>();
    /// assert_eq!(outputs, vec!["a".to_owned(), "b".to_owned(), "c".to_owned()])
    /// ```
    fn owned(self) -> Owned<Self>;
}

impl<I> IterExt for I
where
    I: Iterator,
{
    type Item = I::Item;

    fn try_any<F, E>(mut self, f: F) -> Result<bool, E>
    where
        Self: Sized,
        F: FnMut(Self::Item) -> Result<bool, E>,
    {
        // TODO migrate use of Result<(), Option<E>> to ControlFlow when it's no longer unstable
        fn check<T, E>(
            mut f: impl FnMut(T) -> Result<bool, E>,
        ) -> impl FnMut((), T) -> Result<(), Option<E>> {
            move |(), x| match f(x) {
                Ok(true) => Err(None),
                Ok(false) => Ok(()),
                Err(e) => Err(Some(e)),
            }
        }

        match self.try_fold((), check(f)) {
            Ok(()) => Ok(false),
            Err(None) => Ok(true),
            Err(Some(e)) => Err(e),
        }
    }

    fn try_all<F, E>(mut self, f: F) -> Result<bool, E>
    where
        Self: Sized,
        F: FnMut(Self::Item) -> Result<bool, E>,
    {
        // TODO migrate use of Result<(), Option<E>> to ControlFlow when it's no longer unstable
        fn check<T, E>(
            mut f: impl FnMut(T) -> Result<bool, E>,
        ) -> impl FnMut((), T) -> Result<(), Option<E>> {
            move |(), x| match f(x) {
                Ok(true) => Ok(()),
                Ok(false) => Err(None),
                Err(e) => Err(Some(e)),
            }
        }

        match self.try_fold((), check(f)) {
            Ok(()) => Ok(true),
            Err(None) => Ok(false),
            Err(Some(e)) => Err(e),
        }
    }

    fn try_eq_by<O, F, E>(mut self, other: O, mut eq: F) -> Result<bool, E>
    where
        Self: Sized,
        O: IntoIterator,
        F: FnMut(Self::Item, O::Item) -> Result<bool, E>,
    {
        let mut other = other.into_iter();

        loop {
            let x = match self.next() {
                None => return Ok(other.next().is_none()),
                Some(val) => val,
            };

            let y = match other.next() {
                None => return Ok(false),
                Some(val) => val,
            };

            if !eq(x, y)? {
                return Ok(false);
            }
        }
    }

    fn try_cmp_by<O, F, E>(mut self, other: O, mut cmp: F) -> Result<Ordering, E>
    where
        Self: Sized,
        O: IntoIterator,
        F: FnMut(Self::Item, O::Item) -> Result<Ordering, E>,
    {
        let mut other = other.into_iter();

        loop {
            let x = match self.next() {
                None => {
                    if other.next().is_none() {
                        return Ok(Ordering::Equal);
                    } else {
                        return Ok(Ordering::Less);
                    }
                }
                Some(val) => val,
            };

            let y = match other.next() {
                None => return Ok(Ordering::Greater),
                Some(val) => val,
            };

            match cmp(x, y)? {
                Ordering::Equal => {}
                non_eq => return Ok(non_eq),
            }
        }
    }

    fn try_unzip<A, B, FromA, FromB, E>(self) -> Result<(FromA, FromB), E>
    where
        FromA: Default + Extend<A>,
        FromB: Default + Extend<B>,
        Self: Iterator<Item = Result<(A, B), E>>,
    {
        let mut ts: FromA = Default::default();
        let mut us: FromB = Default::default();

        for e in self {
            let (t, u) = e?;
            ts.extend(Some(t));
            us.extend(Some(u));
        }

        Ok((ts, us))
    }

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

impl<'a, I, T> IterDuped for I
where
    I: Sized,
    I: Iterator<Item = &'a T>,
    T: 'a + Dupe,
{
    fn duped(self) -> Cloned<Self> {
        self.cloned()
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
