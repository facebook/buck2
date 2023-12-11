/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_http::HttpClient;
use dice::UserComputationData;
use dupe::Dupe;

/// Dice implementations so we can pass along the HttpClient to various subsystems
/// that need to use it (Materializer, RunActions, etc).
pub trait HasHttpClient {
    fn get_http_client(&self) -> HttpClient;
}

pub trait SetHttpClient {
    fn set_http_client(&mut self, client: HttpClient);
}

impl HasHttpClient for UserComputationData {
    fn get_http_client(&self) -> HttpClient {
        self.data
            .get::<HttpClient>()
            .expect("HttpClient should be set")
            .dupe()
    }
}

impl SetHttpClient for UserComputationData {
    fn set_http_client(&mut self, client: HttpClient) {
        self.data.set(client);
    }
}
