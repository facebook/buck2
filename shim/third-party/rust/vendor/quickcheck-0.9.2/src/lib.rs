//! This crate is a port of
//! [Haskell's QuickCheck](https://hackage.haskell.org/package/QuickCheck).
//!
//! For detailed examples, please see the
//! [README](https://github.com/BurntSushi/quickcheck).

#![cfg_attr(feature = "i128", feature(i128_type, i128))]

#[cfg(feature = "use_logging")]
extern crate env_logger;
#[cfg(feature = "use_logging")]
#[macro_use]
extern crate log;
extern crate rand;
extern crate rand_core;

pub use crate::arbitrary::{
    empty_shrinker, single_shrinker, Arbitrary, Gen, StdGen, StdThreadGen,
};
pub use crate::tester::{quickcheck, QuickCheck, TestResult, Testable};
pub use rand_core::RngCore;

/// A macro for writing quickcheck tests.
///
/// This macro takes as input one or more property functions to test, and
/// produces a proper `#[test]` function for each property. If the property
/// fails, the behavior is as if `quickcheck` were called on the property
/// (i.e., it panics and fails the test).
///
/// Note that this macro doesn't support `mut` or patterns in parameters.
///
/// # Example
///
/// ```rust
/// # #[macro_use] extern crate quickcheck; fn main() {
/// quickcheck! {
///     fn prop_reverse_reverse(xs: Vec<usize>) -> bool {
///         let rev: Vec<_> = xs.clone().into_iter().rev().collect();
///         let revrev: Vec<_> = rev.into_iter().rev().collect();
///         xs == revrev
///     }
/// };
/// # }
/// ```
#[macro_export]
macro_rules! quickcheck {
    (@as_items $($i:item)*) => ($($i)*);
    {
        $(
            $(#[$m:meta])*
            fn $fn_name:ident($($arg_name:ident : $arg_ty:ty),*) -> $ret:ty {
                $($code:tt)*
            }
        )*
    } => (
        $crate::quickcheck! {
            @as_items
            $(
                #[test]
                $(#[$m])*
                fn $fn_name() {
                    fn prop($($arg_name: $arg_ty),*) -> $ret {
                        $($code)*
                    }
                    $crate::quickcheck(prop as fn($($arg_ty),*) -> $ret);
                }
            )*
        }
    )
}

#[cfg(feature = "use_logging")]
fn env_logger_init() -> Result<(), log::SetLoggerError> {
    env_logger::try_init()
}

#[cfg(not(feature = "use_logging"))]
fn env_logger_init() {}
#[cfg(not(feature = "use_logging"))]
macro_rules! info {
    ($($_ignore:tt)*) => {
        ()
    };
}

mod arbitrary;
mod tester;

#[cfg(test)]
mod tests;
