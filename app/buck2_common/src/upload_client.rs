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

use buck2_http::HttpClient;
use buck2_http::HttpClientBuilder;
use buck2_http::retries::HttpError;
use buck2_http::retries::HttpErrorForRetry;
use buck2_http::retries::IntoBuck2Error;
use buck2_http::retries::http_retry;
use bytes::Bytes;
use futures::stream::BoxStream;
use futures::stream::StreamExt;
use hyper::Response;

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Http)]
enum HttpWriteError {
    #[error(transparent)]
    Client(HttpError),
}

impl HttpErrorForRetry for HttpWriteError {
    fn is_retryable(&self) -> bool {
        match self {
            Self::Client(e) => e.is_retryable(),
        }
    }
}

impl IntoBuck2Error for HttpWriteError {
    fn into_buck2_error(self) -> buck2_error::Error {
        buck2_error::Error::from(self)
    }
}

pub struct HttpUploadClient {
    client: HttpClient,
    base_url: String,
}

impl HttpUploadClient {
    pub async fn new(base_url: String) -> buck2_error::Result<Self> {
        let client = HttpClientBuilder::oss().await?.build();
        Ok(Self { client, base_url })
    }

    pub async fn write(&self, path: &str, buf: Bytes) -> buck2_error::Result<()> {
        let url = format!("{}/{}", self.base_url, path);
        let res = http_retry(
            || async {
                self.client
                    .put(&url, buf.clone(), vec![])
                    .await
                    .map_err(|e| HttpWriteError::Client(HttpError::Client(e)))
            },
            vec![Duration::from_secs(1), Duration::from_secs(2)],
        )
        .await?;
        consume_response(res).await;
        Ok(())
    }
}

async fn consume_response<'a>(mut res: Response<BoxStream<'a, hyper::Result<Bytes>>>) {
    while let Some(_chunk) = res.body_mut().next().await {}
}
