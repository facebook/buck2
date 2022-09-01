/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::panic;

use tokio::task::JoinError;

pub mod guarded_rc;

/// Extension to the 'Result' type to have convenient handling of tokio
/// JoinHandles
pub trait TokioResultExt<T> {
    /// Returns the contained [`Ok`] value, consuming the `self` value.
    ///
    /// # Panics
    ///
    /// Panics if the result is a [`Err`] of any kind.
    /// This will take care of propagating the original panic, if any, and
    /// repanic on the current thread with a good stack trace.
    /// [`Ok`]: enum.Result.html#variant.Ok
    /// [`Err`]: enum.Result.html#variant.Err
    fn expect_completion(self) -> T;
}

impl<T> TokioResultExt<T> for Result<T, JoinError> {
    fn expect_completion(self) -> T {
        match self {
            Ok(t) => t,
            Err(e) => {
                if e.is_panic() {
                    let e = e.into_panic();
                    panic::resume_unwind(e);
                } else {
                    panic!("{}. spawned thread cancelled", e)
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    #[should_panic(expected = "this panic message")]
    async fn test_panic_recovers_original_message() {
        tokio::spawn(async {
            panic!("this panic message");
        })
        .await
        .expect_completion();
    }

    #[tokio::test]
    #[should_panic(expected = "this panic message with fmt 2")]
    async fn test_panic_recovers_original_fmt_message() {
        tokio::spawn(async {
            panic!("this panic message with fmt {}", 2);
        })
        .await
        .expect_completion();
    }

    #[tokio::test]
    #[should_panic(expected = "this panic message")]
    async fn test_panic_recovers_original_message_recursive_spawns() {
        tokio::spawn(async {
            tokio::spawn(async {
                panic!("this panic message");
            })
            .await
            .expect_completion();
        })
        .await
        .expect_completion();
    }

    #[tokio::test]
    async fn test_ok_is_ok() {
        let res = tokio::spawn(async { 1 }).await.expect_completion();

        assert_eq!(res, 1);
    }
}
