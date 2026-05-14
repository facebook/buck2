/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::time::Duration;

use futures::future::Either;
use futures::future::Future;
use futures::stream::Stream;
use futures::stream::StreamExt;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;

/// This function is used for commands where the client wants to stream `stdin`
/// to the server and return back responses. Usually this is for commands that
/// act as API endpoints over stdin/stdout like language servers, etc.
///
/// We need to provide a 'static stream for Tonic to send to the Buck2 daemon,
/// but we don't want to borrow stdin statically (though in practice that
/// doesn't really matter because the way the command ends is when stdin is
/// empty). So, what we do instead is that we forward stdin only while the
/// command is ongoing.
///
/// The `hangup` parameter allows producing a message to the server when stdin
/// ends. The `hangup_timeout` parameter allows returning a fallback result
/// after stdin ends and the optional hangup message has been sent. If it
/// returns `None`, this helper stays pending after EOF and relies on the work
/// future to finish independently.
pub async fn reborrow_stream_for_static<'a, T, R, F>(
    stream: impl Stream<Item = T> + 'a,
    f: impl FnOnce(ReceiverStream<T>) -> F,
    hangup: impl FnOnce() -> Option<T>,
    hangup_timeout: impl FnOnce() -> Option<(Duration, R)>,
) -> R
where
    F: Future<Output = R> + 'a,
    T: 'static,
{
    let (tx, rx) = mpsc::channel(1);

    let forward = async move {
        futures::pin_mut!(stream);

        while let Ok(permit) = tx.reserve().await {
            match stream.next().await {
                Some(e) => permit.send(e),
                None => break,
            }
        }

        if let Some(hangup) = hangup() {
            let _ignored = tx.send(hangup).await;
        }

        let out = if let Some((timeout, out)) = hangup_timeout() {
            tokio::time::sleep(timeout).await;
            out
        } else {
            // Some server-side streaming commands do not handle hangups. Until they do, we must
            // keep this side alive and rely on the RPC finishing independently.
            futures::future::pending().await
        };

        // We only get here when hangup_timeout() provides a fallback result. Without one, this
        // branch stays pending forever and relies on the work future to finish independently.
        drop(tx);

        out
    };

    let rx = ReceiverStream::new(rx);
    let work = f(rx);

    futures::pin_mut!(work);
    futures::pin_mut!(forward);

    Either::factor_first(futures::future::select(work, forward).await).0
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use tokio_stream::StreamExt;

    use super::*;

    #[tokio::test]
    async fn test_reborrow_stream_for_static_prefers_work_result_after_hangup() {
        let result = reborrow_stream_for_static(
            futures::stream::empty::<u8>(),
            |mut stream| async move {
                assert_eq!(stream.next().await, Some(7));
                "work"
            },
            || Some(7),
            || Some((Duration::from_secs(1), "timeout")),
        )
        .await;

        assert_eq!(result, "work");
    }

    #[tokio::test]
    async fn test_reborrow_stream_for_static_times_out_after_hangup() {
        let result = reborrow_stream_for_static(
            futures::stream::empty::<u8>(),
            |mut stream| async move {
                assert_eq!(stream.next().await, Some(7));
                futures::future::pending::<&'static str>().await
            },
            || Some(7),
            || Some((Duration::from_millis(10), "timeout")),
        )
        .await;

        assert_eq!(result, "timeout");
    }
}
