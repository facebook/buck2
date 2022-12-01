/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use futures::future::Future;

pub async fn to_tonic<F, T>(fut: F) -> Result<tonic::Response<T>, tonic::Status>
where
    F: Future<Output = anyhow::Result<T>>,
{
    match fut.await {
        Ok(r) => Ok(tonic::Response::new(r)),
        Err(e) => Err(tonic::Status::unknown(format!("{:#}", e))),
    }
}
