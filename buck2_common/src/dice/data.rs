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

use dice::data::DiceData;
use dice::DiceDataBuilder;
use gazebo::prelude::*;

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

pub mod testing {
    use buck2_core::fs::project::ProjectFilesystemTemp;

    use super::*;
    use crate::io::fs::FsIoProvider;

    pub trait SetTestingIoProvider {
        fn set_testing_io_provider(&mut self, fs: &ProjectFilesystemTemp);
    }

    impl SetTestingIoProvider for DiceDataBuilder {
        fn set_testing_io_provider(&mut self, fs: &ProjectFilesystemTemp) {
            self.set_io_provider(Arc::new(FsIoProvider::new(Arc::new(fs.path().clone()))))
        }
    }
}
