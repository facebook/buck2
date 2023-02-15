/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Core data on dice

use std::sync::Arc;

use dice::DiceData;
use dice::DiceDataBuilder;
use dupe::Dupe;

use crate::digest_config::DigestConfig;
use crate::io::IoProvider;

pub trait HasIoProvider {
    // TODO(bobyf) can we make this not an arc
    fn get_io_provider(&self) -> Arc<dyn IoProvider>;
}

pub trait SetIoProvider {
    fn set_io_provider(&mut self, fs: Arc<dyn IoProvider>);
}

impl HasIoProvider for DiceData {
    fn get_io_provider(&self) -> Arc<dyn IoProvider> {
        self.get::<Arc<dyn IoProvider>>()
            .expect("project filesystem should be set")
            .dupe()
    }
}

impl SetIoProvider for DiceDataBuilder {
    fn set_io_provider(&mut self, fs: Arc<dyn IoProvider>) {
        self.set(fs)
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

pub mod testing {
    use buck2_core::fs::project::ProjectRootTemp;

    use super::*;
    use crate::io::fs::FsIoProvider;

    pub trait SetTestingIoProvider {
        fn set_testing_io_provider(&mut self, fs: &ProjectRootTemp);
    }

    impl SetTestingIoProvider for DiceDataBuilder {
        fn set_testing_io_provider(&mut self, fs: &ProjectRootTemp) {
            self.set_io_provider(Arc::new(FsIoProvider::new(fs.path().dupe())))
        }
    }
}
