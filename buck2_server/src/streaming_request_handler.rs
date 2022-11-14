/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::marker::PhantomData;

use cli_proto::StreamingRequest;
use tonic::Status;

/// Simple container that holds onto a stream of incoming client requests.
///
/// The primary use for this is pulling messages of a specific type from
/// the client via [`StreamingRequestHandler::message`]
pub struct StreamingRequestHandler<T: TryFrom<StreamingRequest, Error = Status>> {
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
}
