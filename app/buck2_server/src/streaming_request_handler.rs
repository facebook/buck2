/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::marker::PhantomData;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use buck2_cli_proto::StreamingRequest;
use futures::Stream;
use pin_project::pin_project;
use tonic::Status;

/// Simple container that holds onto a stream of incoming client requests.
///
/// The primary use for this is pulling messages of a specific type from
/// the client via [`StreamingRequestHandler::message`]
#[pin_project]
pub struct StreamingRequestHandler<T: TryFrom<StreamingRequest, Error = Status>> {
    #[pin]
    client_stream: tonic::Streaming<StreamingRequest>,
    _phantom: PhantomData<T>,
}

impl<T: TryFrom<StreamingRequest, Error = Status>> StreamingRequestHandler<T> {
    pub fn new(client_stream: tonic::Streaming<StreamingRequest>) -> Self {
        Self {
            client_stream,
            _phantom: PhantomData::default(),
        }
    }

    /// Get a message of type `T` from inside of a [`StreamingRequest`] envelope.
    ///
    /// Returns an error if the message is of the wrong type.
    pub async fn message(&mut self) -> Result<T, Status> {
        let request = match self.client_stream.message().await? {
            Some(m) => Ok(m),
            None => Err(Status::failed_precondition(
                "received a message that is not a `StreamingRequest`",
            )),
        }?;
        request.try_into()
    }

    fn map_poll(v: Option<Result<StreamingRequest, Status>>) -> Option<Result<T, Status>> {
        v.map(|v| v.and_then(|v| v.try_into()))
    }
}

impl<T: TryFrom<StreamingRequest, Error = Status>> Stream for StreamingRequestHandler<T> {
    type Item = Result<T, Status>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let this = self.project();
        let inner: Pin<&mut _> = this.client_stream;

        match inner.poll_next(cx) {
            Poll::Ready(v) => Poll::Ready(Self::map_poll(v)),
            Poll::Pending => Poll::Pending,
        }
    }
}
