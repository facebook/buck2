/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::{self, Debug};

use quickcheck::{Arbitrary, Gen};

/// This type is a token that allows code to prove initFacebook has been called.
/// A function can require this proof by taking `_fb: FacebookInit` as an
/// argument.
///
/// The basic assumption of initFacebook is that it happens at the very
/// beginning of main before there are additional threads. It must be allowed to
/// modify process-global state like env vars or gflags without the risk of
/// undefined behavior from other code concurrently reading those things.
///
/// The only way to obtain a proof token is through a pair of attribute macros
/// exported by this crate. `#\[fbinit::main\]` is placed on your main function
/// and will call initFacebook and provide the resulting proof:
///
///     #[fbinit::main]
///     fn main(fb: fbinit::FacebookInit) {
///         /* ... */
///     }
///
/// The proof argument is optional. If you don't need it, this is fine too:
///
///     #[fbinit::main]
///     fn main() {
///         /* ... */
///     }
///
/// If main is async, the attribute behaves like `#\[tokio::main\].
///
///     #[fbinit::main]
///     async fn main(fb: fbinit::FacebookInit) {
///         yay().await;
///     }
///
///     async fn yay() {}
///
/// There is also `#\[fbinit::test\]` which behaves like `#\[test\]` or
/// `#\[tokio::test\]`.
///
///     #[fbinit::test]
///     fn test_my_service(fb: fbinit::FacebookInit) {
///         /* ... */
///     }
///
///     #[fbinit::test]
///     async fn async_test(fb: fbinit::FacebookInit) {
///         /* ... */
///     }
///
#[derive(Copy, Clone)]
pub struct FacebookInit {
    // Prevent code outside of this crate from constructing.
    _private: (),
}

/// Produces a proof that initFacebook has been called, without actually calling
/// initFacebook.
///
/// # Safety
///
/// This is unsafe! You must know somehow that fbinit::main has been used or the
/// init was performed already by C++.
pub const unsafe fn assume_init() -> FacebookInit {
    FacebookInit { _private: () }
}

/// Produces proof that initFacebook has been called, or panics otherwise.
///
/// # Panics
///
/// This call always panics for non fbcode builds. For fbcode builds it panics
/// if `perform_init` was not called before.
pub fn expect_init() -> FacebookInit {
    panic!("fbinit::expect_init was called, but this is not an fbcode build!");
}

impl Debug for FacebookInit {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("FacebookInit")
    }
}

impl Arbitrary for FacebookInit {
    fn arbitrary<G: Gen>(_: &mut G) -> Self {
        unsafe { r#impl::perform_init() }
    }
}

// Not public API. These are used by the attribute macros.
// The non fbcode_build version is not performing any Facebook
// initializations.
#[doc(hidden)]
pub mod r#impl {
    pub use super::assume_init as perform_init;

    pub struct DestroyGuard;

    impl DestroyGuard {
        pub fn new() -> Self {
            DestroyGuard
        }
    }
}
