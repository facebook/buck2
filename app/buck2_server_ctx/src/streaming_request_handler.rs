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

#[derive(Debug, thiserror::Error)]
enum StreamingRequestError {
    #[error("Request returned error status: {0}")]
    GrpcStatus(Status),
    #[error("Unexpected EOF reading gRPC request")]
    UnexpectedEof,
}

/// Simple container that holds onto a stream of incoming client requests.
///
/// The primary use for this is pulling messages of a specific type from
/// the client via [`StreamingRequestHandler::message`]
#[pin_project]
pub struct StreamingRequestHandler<T: TryFrom<StreamingRequest, Error = anyhow::Error>> {
    #[pin]
    client_stream: tonic::Streaming<StreamingRequest>,
    _phantom: PhantomData<T>,
}

impl<T: TryFrom<StreamingRequest, Error = anyhow::Error>> StreamingRequestHandler<T> {
    pub fn new(client_stream: tonic::Streaming<StreamingRequest>) -> Self {
        Self {
            client_stream,
            _phantom: PhantomData,
        }
    }

    /// Get a message of type `T` from inside of a [`StreamingRequest`] envelope.
    ///
    /// Returns an error if the message is of the wrong type.
    pub async fn message(&mut self) -> anyhow::Result<T> {
        let request = match self.client_stream.message().await {
            Err(e) => return Err(StreamingRequestError::GrpcStatus(e).into()),
            Ok(Some(m)) => m,
            Ok(None) => return Err(StreamingRequestError::UnexpectedEof.into()),
        };
        request.try_into()
    }

    fn map_poll(v: Option<Result<StreamingRequest, Status>>) -> Option<anyhow::Result<T>> {
        match v {
            None => None,
            Some(Err(e)) => Some(Err(StreamingRequestError::GrpcStatus(e).into())),
            Some(Ok(v)) => match v.try_into() {
                Ok(v) => Some(Ok(v)),
                Err(e) => Some(Err(e)),
            },
        }
    }
}

impl<T: TryFrom<StreamingRequest, Error = anyhow::Error>> Stream for StreamingRequestHandler<T> {
    type Item = anyhow::Result<T>;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        let this = self.project();
        let inner: Pin<&mut _> = this.client_stream;

        match inner.poll_next(cx) {
            Poll::Ready(v) => Poll::Ready(Self::map_poll(v)),
            Poll::Pending => Poll::Pending,
        }
    }
}
