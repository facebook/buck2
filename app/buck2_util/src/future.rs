/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::iter;
use std::mem;

use futures::stream::FuturesUnordered;
use futures::FutureExt;
use futures::StreamExt;

// patternlint-disable-next-line buck2-no-futures-try-join-all
/// Semantically the same as `futures::future::try_join_all`, but bug free.
///
/// <https://github.com/rust-lang/futures-rs/issues/2866>
pub fn try_join_all<F, R, E, I>(i: I) -> impl Future<Output = Result<Vec<R>, E>>
where
    F: Future<Output = Result<R, E>>,
    I: IntoIterator<Item = F>,
{
    let mut i = i.into_iter().fuse();
    let f1 = i.next();
    let f2 = i.next();
    let f3 = i.next();

    let (f1, f2, f3) = match (f1, f2, f3) {
        (None, _, _) => {
            return futures::future::ready(Ok(Vec::new()))
                .left_future()
                .left_future();
        }
        (Some(f1), None, _) => {
            return async move { Ok(vec![f1.await?]) }
                .left_future()
                .right_future();
        }
        (Some(f1), Some(f2), None) => {
            return async move {
                let (v1, v2) = futures::future::try_join(f1, f2).await?;
                Ok(vec![v1, v2])
            }
            .right_future()
            .left_future();
        }
        (Some(f1), Some(f2), Some(f3)) => (f1, f2, f3),
    };

    #[allow(clippy::tuple_array_conversions)]
    let mut futs: FuturesUnordered<_> = [f1, f2, f3]
        .into_iter()
        .chain(i)
        .enumerate()
        .map(|(i, f)| async move { (i, f.await) })
        .collect();

    async move {
        let mut outputs: Vec<Option<_>> = iter::repeat_with(|| None).take(futs.len()).collect();
        while let Some((i, res)) = futs.next().await {
            match res {
                Ok(v) => {
                    let prev = mem::replace(&mut outputs[i], Some(v));
                    assert!(prev.is_none());
                }
                Err(e) => return Err(e),
            }
        }
        Ok(outputs.into_iter().map(|v| v.unwrap()).collect())
    }
    .right_future()
    .right_future()
}

#[cfg(test)]
mod tests {
    async fn slow_ok_or_err(ok: bool) -> Result<(), ()> {
        if ok {
            futures::future::pending().await
        } else {
            Err(())
        }
    }

    /// Test that the futures are eagerly cancelled when the first one fails. This test fails with
    /// the `futures` version of the function.
    #[tokio::test]
    async fn test_returns_eagerly() {
        for size in [0, 1, 2, 10, 1000] {
            let futs = std::iter::repeat_with(|| slow_ok_or_err(true))
                .take(size)
                .chain([slow_ok_or_err(false)]);
            super::try_join_all(futs).await.unwrap_err();
        }
    }
}
