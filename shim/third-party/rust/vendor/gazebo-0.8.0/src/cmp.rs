/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Traits to help implementing dynamic comparisons.

use std::any::Any;

/// A comparable "token" that can be returned to wrap a reference to an [`Any`
/// type](Any) for [`PartialEq`](PartialEq).
///
/// This lets dyn traits be comparable by having all implementations return some
/// "token" that can be considered [`PartialEq`](PartialEq).
pub struct PartialEqAny<'a> {
    cmp: fn(&'a dyn Any, &'a dyn Any) -> bool,
    val: &'a dyn Any,
}

impl<'a> PartialEqAny<'a> {
    pub fn new<A: PartialEq + 'static>(a: &'a A) -> Self {
        Self {
            cmp: |this, other| {
                // SAFETY: We only call `cmp` with `this === a`.
                debug_assert!(this.downcast_ref::<A>().is_some());
                Some(unsafe { &*(this as *const dyn Any as *const A) }) == other.downcast_ref::<A>()
            },
            val: a,
        }
    }

    /// gets an instance that always compares to false
    pub fn always_false() -> Self {
        struct AlwaysFalse;

        impl PartialEq for AlwaysFalse {
            fn eq(&self, _other: &Self) -> bool {
                false
            }
        }

        PartialEqAny::new(&AlwaysFalse)
    }
}

impl<'a> PartialEq for PartialEqAny<'a> {
    fn eq(&self, other: &PartialEqAny<'a>) -> bool {
        (self.cmp)(self.val, other.val)
    }
}

/// Performs a chain of comparison operation expressions yielding `std::cmp::Ordering`, supporting
/// early exit upon hitting the first expressions that doesn't yield `std::cmp::Ordering::Equal`
/// and returning the result of that. This is useful for easily writing a sequence of expressions
/// necessary to yield a comparison result.
/// The macro is expanded inplace, so any expressions dealing with `Result` types are allowed
/// provided that the larger scope allows returning result.
///
/// ```
/// use std::cmp::Ordering;
/// use gazebo::cmp_chain;
///
/// assert_eq!(
///     cmp_chain! {
///         1.cmp(&1),
///         Ok::<_, ()>(2.cmp(&2))?,
///         3.cmp(&4),
///         panic!("won't reach this"),
///     },
///     Ordering::Less,
/// );
///
/// # Ok::<_, ()>(())
/// ```
#[macro_export]
macro_rules! cmp_chain {
    ($e:expr) => {
        $e
    };
    ($e:expr, $($x:expr),+ $(,)?) => {
        match $e {
            std::cmp::Ordering::Equal => {
                cmp_chain!($($x),+)
            },
            c => {
                c
            }
        }
    };
}

/// Performs a chain of equals operation expressions yielding `bool`, supporting
/// early exit upon hitting the first expressions that returns `false` and returning `false`.
/// This is useful for easily writing a sequence of equals expressions necessary to yield a `bool`
/// The macro is expanded inplace, so any expressions dealing with `Result` types are allowed
/// provided that the larger scope allows returning result.
///
/// ```
/// use gazebo::eq_chain;
///
/// assert_eq!(
///     eq_chain! {
///         1 == 1,
///         Ok::<_, ()>(2 == 2)?,
///         3 == 4,
///         panic!("won't reach this"),
///     },
///     false,
/// );
///
/// # Ok::<_, ()>(())
/// ```
#[macro_export]
macro_rules! eq_chain {
    ($e:expr) => {
        $e
    };
    ($e:expr, $($x:expr),+ $(,)?) => {
        if $e {
            eq_chain!($($x),+)
        } else {
            false
        }
    };
}

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;

    use crate::cmp::PartialEqAny;

    struct Wrap<T>(T);

    impl<T: PartialEq + 'static> Wrap<T> {
        fn token(&self) -> PartialEqAny {
            PartialEqAny::new(&self.0)
        }
    }

    #[test]
    fn test_cmp_any() {
        let w1 = Wrap(1);
        let w2 = Wrap(1);
        let w3 = Wrap(2);

        assert_eq!(w1.token() == w2.token(), true);
        assert_eq!(w1.token() == w3.token(), false);

        let w4 = Wrap("foo");
        let w5 = Wrap("foo");
        let w6 = Wrap("bar");

        assert_eq!(w4.token() == w5.token(), true);
        assert_eq!(w4.token() == w6.token(), false);

        assert_eq!(w1.token() == w6.token(), false);
    }

    #[test]
    #[allow(clippy::eq_op)]
    fn always_false_cmp() {
        let w = Wrap(1);
        let f = PartialEqAny::always_false();

        assert_eq!(f == f, false);
        assert_eq!(f == w.token(), false);
    }

    #[test]
    fn cmp_eq_chain() {
        struct FakeComparable(
            Box<dyn Fn() -> Ordering>,
            Box<dyn Fn() -> Ordering>,
            Box<dyn Fn() -> Ordering>,
        );
        impl PartialEq for FakeComparable {
            fn eq(&self, _other: &Self) -> bool {
                eq_chain!(
                    (self.0)() == Ordering::Equal,
                    (self.1)() == Ordering::Equal,
                    (self.2)() == Ordering::Equal,
                )
            }
        }
        impl Eq for FakeComparable {}
        impl PartialOrd for FakeComparable {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }
        impl Ord for FakeComparable {
            fn cmp(&self, _other: &Self) -> Ordering {
                cmp_chain! {
                    (self.0)(),
                    (self.1)(),
                    (self.2)(),
                }
            }
        }

        let fake = FakeComparable(
            Box::new(|| Ordering::Less),
            Box::new(|| unreachable!("should return less")),
            Box::new(|| unreachable!("should return less")),
        );
        assert_eq!(fake.cmp(&fake), Ordering::Less);
        assert_eq!(fake.eq(&fake), false);

        let fake = FakeComparable(
            Box::new(|| Ordering::Greater),
            Box::new(|| unreachable!("should return less")),
            Box::new(|| unreachable!("should return less")),
        );
        assert_eq!(fake.cmp(&fake), Ordering::Greater);
        assert_eq!(fake.eq(&fake), false);

        let fake = FakeComparable(
            Box::new(|| Ordering::Equal),
            Box::new(|| Ordering::Less),
            Box::new(|| unreachable!("should return less")),
        );
        assert_eq!(fake.cmp(&fake), Ordering::Less);
        assert_eq!(fake.eq(&fake), false);

        let fake = FakeComparable(
            Box::new(|| Ordering::Equal),
            Box::new(|| Ordering::Equal),
            Box::new(|| Ordering::Greater),
        );
        assert_eq!(fake.cmp(&fake), Ordering::Greater);
        assert_eq!(fake.eq(&fake), false);

        let fake = FakeComparable(
            Box::new(|| Ordering::Equal),
            Box::new(|| Ordering::Equal),
            Box::new(|| Ordering::Equal),
        );
        assert_eq!(fake.cmp(&fake), Ordering::Equal);
        assert_eq!(fake.eq(&fake), true);
    }
}
