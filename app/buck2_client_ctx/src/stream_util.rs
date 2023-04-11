/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use futures::future::Either;
use futures::future::Future;
use futures::stream::Stream;
use futures::stream::StreamExt;
use tokio::sync::mpsc;
use tokio_stream::wrappers::ReceiverStream;

/// We need to provide a 'static stream for Tonic to send to the Buck2 daemon, but we don't want to
/// borrow stdin statically (though in practice that doesn't really matter because the way the
/// command ends is when stdin is empty). So, what we do instead is that we forward stdin only
/// while the command is ongoing.
///
/// The `hangup` parameter allows producing a message when stdin ends.
pub async fn reborrow_stream_for_static<'a, T, R, F>(
    stream: impl Stream<Item = T> + 'a,
    f: impl FnOnce(ReceiverStream<T>) -> F,
    hangup: impl FnOnce() -> Option<T>,
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

        // The LSP server side does not handle hangups. So, until it does... we never hang up:
        // Err(Status { code: FailedPrecondition, message: "received a message that is not a `StreamingRequest`", source: None })
        let out = futures::future::pending().await;

        // We actually can't get here; Note that we actually get his function to return a "R"
        // because pending() can return whatever we want it to, but that's because it can't actually
        // return :)
        drop(tx);

        out
    };

    let rx = ReceiverStream::new(rx);
    let work = f(rx);

    futures::pin_mut!(work);
    futures::pin_mut!(forward);

    Either::factor_first(futures::future::select(work, forward).await).0
}
