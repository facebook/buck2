/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use dice::DiceData;
use dice::DiceDataBuilder;
use dupe::Dupe;

/// This configuration describes how to interpret digests received from a RE backend.
#[derive(Copy, Clone, Dupe, Debug, Allocative, Hash, Eq, PartialEq)]
pub struct DigestConfig {}

impl DigestConfig {
    pub fn compat() -> Self {
        Self {}
    }
}

pub trait HasDigestConfig {
    fn get_digest_config(&self) -> DigestConfig;
}

pub trait SetDigestConfig {
    fn set_digest_config(&mut self, digest_config: DigestConfig);
}

impl HasDigestConfig for DiceData {
    fn get_digest_config(&self) -> DigestConfig {
        self.get::<DigestConfig>()
            .expect("digest config should be set")
            .dupe()
    }
}

impl SetDigestConfig for DiceDataBuilder {
    fn set_digest_config(&mut self, digest_config: DigestConfig) {
        self.set(digest_config)
    }
}
