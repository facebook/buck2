//! Provides a macro, `assert_matches!`, which tests whether a value
//! matches a given pattern, causing a panic if the match fails.
//!
//! See the macro [`assert_matches!`] documentation for more information.
//!
//! Also provides a debug-only counterpart, [`debug_assert_matches!`].
//!
//! See the macro [`debug_assert_matches!`] documentation for more information
//! about this macro.
//!
//! [`assert_matches!`]: macro.assert_matches.html
//! [`debug_assert_matches!`]: macro.debug_assert_matches.html

#![deny(missing_docs)]
#![cfg_attr(not(test), no_std)]

/// Asserts that an expression matches a given pattern.
///
/// A guard expression may be supplied to add further restrictions to the
/// expected value of the expression.
///
/// A `match` arm may be supplied to perform additional assertions or to yield
/// a value from the macro invocation.
///
/// # Examples
///
/// ```
/// #[macro_use] extern crate assert_matches;
///
/// #[derive(Debug)]
/// enum Foo {
///     A(i32),
///     B(&'static str),
/// }
///
/// # fn main() {
/// let a = Foo::A(1);
///
/// // Assert that `a` matches the pattern `Foo::A(_)`.
/// assert_matches!(a, Foo::A(_));
///
/// // Assert that `a` matches the pattern and
/// // that the contained value meets the condition `i > 0`.
/// assert_matches!(a, Foo::A(i) if i > 0);
///
/// let b = Foo::B("foobar");
/// 
/// // Assert that `b` matches the pattern `Foo::B(_)`.
/// assert_matches!(b, Foo::B(s) => {
///     // Perform additional assertions on the variable binding `s`.
///     assert!(s.starts_with("foo"));
///     assert!(s.ends_with("bar"));
/// });
///
/// // Assert that `b` matches the pattern and yield the string `s`.
/// let s = assert_matches!(b, Foo::B(s) => s);
///
/// // Perform an assertion on the value `s`.
/// assert_eq!(s, "foobar");
/// # }
/// ```
#[macro_export]
macro_rules! assert_matches {
    ( $e:expr , $($pat:pat)|+ ) => {
        match $e {
            $($pat)|+ => (),
            ref e => panic!("assertion failed: `{:?}` does not match `{}`",
                e, stringify!($($pat)|+))
        }
    };
    ( $e:expr , $($pat:pat)|+ if $cond:expr ) => {
        match $e {
            $($pat)|+ if $cond => (),
            ref e => panic!("assertion failed: `{:?}` does not match `{}`",
                e, stringify!($($pat)|+ if $cond))
        }
    };
    ( $e:expr , $($pat:pat)|+ => $arm:expr ) => {
        match $e {
            $($pat)|+ => $arm,
            ref e => panic!("assertion failed: `{:?}` does not match `{}`",
                e, stringify!($($pat)|+))
        }
    };
    ( $e:expr , $($pat:pat)|+ if $cond:expr => $arm:expr ) => {
        match $e {
            $($pat)|+ if $cond => $arm,
            ref e => panic!("assertion failed: `{:?}` does not match `{}`",
                e, stringify!($($pat)|+ if $cond))
        }
    };
    ( $e:expr , $($pat:pat)|+ , $($arg:tt)* ) => {
        match $e {
            $($pat)|+ => (),
            ref e => panic!("assertion failed: `{:?}` does not match `{}`: {}",
                e, stringify!($($pat)|+), format_args!($($arg)*))
        }
    };
    ( $e:expr , $($pat:pat)|+ if $cond:expr , $($arg:tt)* ) => {
        match $e {
            $($pat)|+ if $cond => (),
            ref e => panic!("assertion failed: `{:?}` does not match `{}`: {}",
                e, stringify!($($pat)|+ if $cond), format_args!($($arg)*))
        }
    };
    ( $e:expr , $($pat:pat)|+ => $arm:expr , $($arg:tt)* ) => {
        match $e {
            $($pat)|+ => $arm,
            ref e => panic!("assertion failed: `{:?}` does not match `{}`: {}",
                e, stringify!($($pat)|+), format_args!($($arg)*))
        }
    };
    ( $e:expr , $($pat:pat)|+ if $cond:expr => $arm:expr , $($arg:tt)* ) => {
        match $e {
            $($pat)|+ if $cond => $arm,
            ref e => panic!("assertion failed: `{:?}` does not match `{}`: {}",
                e, stringify!($($pat)|+ if $cond), format_args!($($arg)*))
        }
    };
}

/// Asserts that an expression matches a given pattern.
///
/// Unlike [`assert_matches!`], `debug_assert_matches!` statements are only enabled
/// in non-optimized builds by default. An optimized build will omit all
/// `debug_assert_matches!` statements unless `-C debug-assertions` is passed
/// to the compiler.
///
/// See the macro [`assert_matches!`] documentation for more information.
///
/// [`assert_matches!`]: macro.assert_matches.html
#[macro_export(local_inner_macros)]
macro_rules! debug_assert_matches {
    ( $($tt:tt)* ) => { {
        if _assert_matches_cfg!(debug_assertions) {
            assert_matches!($($tt)*);
        }
    } }
}

#[doc(hidden)]
#[macro_export]
macro_rules! _assert_matches_cfg {
    ( $($tt:tt)* ) => { cfg!($($tt)*) }
}

#[cfg(test)]
mod test {
    use std::panic::{catch_unwind, UnwindSafe};

    #[derive(Debug)]
    enum Foo {
        A(i32),
        B(&'static str),
        C(&'static str),
    }

    #[test]
    fn test_assert_succeed() {
        let a = Foo::A(123);

        assert_matches!(a, Foo::A(_));
        assert_matches!(a, Foo::A(123));
        assert_matches!(a, Foo::A(i) if i == 123);
        assert_matches!(a, Foo::A(42) | Foo::A(123));

        let b = Foo::B("foo");

        assert_matches!(b, Foo::B(_));
        assert_matches!(b, Foo::B("foo"));
        assert_matches!(b, Foo::B(s) if s == "foo");
        assert_matches!(b, Foo::B(s) => assert_eq!(s, "foo"));
        assert_matches!(b, Foo::B(s) => { assert_eq!(s, "foo"); assert!(true) });
        assert_matches!(b, Foo::B(s) if s == "foo" => assert_eq!(s, "foo"));
        assert_matches!(b, Foo::B(s) if s == "foo" => { assert_eq!(s, "foo"); assert!(true) });

        let c = Foo::C("foo");

        assert_matches!(c, Foo::B(_) | Foo::C(_));
        assert_matches!(c, Foo::B("foo") | Foo::C("foo"));
        assert_matches!(c, Foo::B(s) | Foo::C(s) if s == "foo");
        assert_matches!(c, Foo::B(s) | Foo::C(s) => assert_eq!(s, "foo"));
        assert_matches!(c, Foo::B(s) | Foo::C(s) => { assert_eq!(s, "foo"); assert!(true) });
        assert_matches!(c, Foo::B(s) | Foo::C(s) if s == "foo" => assert_eq!(s, "foo"));
        assert_matches!(c, Foo::B(s) | Foo::C(s) if s == "foo" => { assert_eq!(s, "foo"); assert!(true) });
    }

    #[test]
    #[should_panic]
    fn test_assert_panic_0() {
        let a = Foo::A(123);

        assert_matches!(a, Foo::B(_));
    }

    #[test]
    #[should_panic]
    fn test_assert_panic_1() {
        let b = Foo::B("foo");

        assert_matches!(b, Foo::B("bar"));
    }

    #[test]
    #[should_panic]
    fn test_assert_panic_2() {
        let b = Foo::B("foo");

        assert_matches!(b, Foo::B(s) if s == "bar");
    }

    #[test]
    #[should_panic]
    fn test_assert_panic_3() {
        let b = Foo::B("foo");

        assert_matches!(b, Foo::B(s) => assert_eq!(s, "bar"));
    }

    #[test]
    #[should_panic]
    fn test_assert_panic_4() {
        let b = Foo::B("foo");

        assert_matches!(b, Foo::B(s) if s == "bar" => assert_eq!(s, "foo"));
    }

    #[test]
    #[should_panic]
    fn test_assert_panic_5() {
        let b = Foo::B("foo");

        assert_matches!(b, Foo::B(s) if s == "foo" => assert_eq!(s, "bar"));
    }

    #[test]
    #[should_panic]
    fn test_assert_panic_6() {
        let b = Foo::B("foo");

        assert_matches!(b, Foo::B(s) if s == "foo" => { assert_eq!(s, "foo"); assert!(false) });
    }

    #[test]
    fn test_assert_no_move() {
        let b = &mut Foo::A(0);
        assert_matches!(*b, Foo::A(0));
    }

    #[test]
    fn assert_with_message() {
        let a = Foo::A(0);

        assert_matches!(a, Foo::A(_), "o noes");
        assert_matches!(a, Foo::A(n) if n == 0, "o noes");
        assert_matches!(a, Foo::A(n) => assert_eq!(n, 0), "o noes");
        assert_matches!(a, Foo::A(n) => { assert_eq!(n, 0); assert!(n < 1) }, "o noes");
        assert_matches!(a, Foo::A(n) if n == 0 => assert_eq!(n, 0), "o noes");
        assert_matches!(a, Foo::A(n) if n == 0 => { assert_eq!(n, 0); assert!(n < 1) }, "o noes");
        assert_matches!(a, Foo::A(_), "o noes {:?}", a);
        assert_matches!(a, Foo::A(n) if n == 0, "o noes {:?}", a);
        assert_matches!(a, Foo::A(n) => assert_eq!(n, 0), "o noes {:?}", a);
        assert_matches!(a, Foo::A(n) => { assert_eq!(n, 0); assert!(n < 1) }, "o noes {:?}", a);
        assert_matches!(a, Foo::A(_), "o noes {value:?}", value=a);
        assert_matches!(a, Foo::A(n) if n == 0, "o noes {value:?}", value=a);
        assert_matches!(a, Foo::A(n) => assert_eq!(n, 0), "o noes {value:?}", value=a);
        assert_matches!(a, Foo::A(n) => { assert_eq!(n, 0); assert!(n < 1) }, "o noes {value:?}", value=a);
        assert_matches!(a, Foo::A(n) if n == 0 => assert_eq!(n, 0), "o noes {value:?}", value=a);
    }

    fn panic_message<F>(f: F) -> String
            where F: FnOnce() + UnwindSafe {
        let err = catch_unwind(f)
            .expect_err("function did not panic");

        *err.downcast::<String>()
            .expect("function panicked with non-String value")
    }

    #[test]
    fn test_panic_message() {
        let a = Foo::A(1);

        // expr, pat
        assert_eq!(panic_message(|| {
            assert_matches!(a, Foo::B(_));
        }), r#"assertion failed: `A(1)` does not match `Foo::B(_)`"#);

        // expr, pat if cond
        assert_eq!(panic_message(|| {
            assert_matches!(a, Foo::B(s) if s == "foo");
        }), r#"assertion failed: `A(1)` does not match `Foo::B(s) if s == "foo"`"#);

        // expr, pat => arm
        assert_eq!(panic_message(|| {
            assert_matches!(a, Foo::B(_) => {});
        }), r#"assertion failed: `A(1)` does not match `Foo::B(_)`"#);

        // expr, pat if cond => arm
        assert_eq!(panic_message(|| {
            assert_matches!(a, Foo::B(s) if s == "foo" => {});
        }), r#"assertion failed: `A(1)` does not match `Foo::B(s) if s == "foo"`"#);

        // expr, pat, args
        assert_eq!(panic_message(|| {
            assert_matches!(a, Foo::B(_), "msg");
        }), r#"assertion failed: `A(1)` does not match `Foo::B(_)`: msg"#);

        // expr, pat if cond, args
        assert_eq!(panic_message(|| {
            assert_matches!(a, Foo::B(s) if s == "foo", "msg");
        }), r#"assertion failed: `A(1)` does not match `Foo::B(s) if s == "foo"`: msg"#);

        // expr, pat => arm, args
        assert_eq!(panic_message(|| {
            assert_matches!(a, Foo::B(_) => {}, "msg");
        }), r#"assertion failed: `A(1)` does not match `Foo::B(_)`: msg"#);

        // expr, pat if cond => arm, args
        assert_eq!(panic_message(|| {
            assert_matches!(a, Foo::B(s) if s == "foo" => {}, "msg");
        }), r#"assertion failed: `A(1)` does not match `Foo::B(s) if s == "foo"`: msg"#);
    }
}
