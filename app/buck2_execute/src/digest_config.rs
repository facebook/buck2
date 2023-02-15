/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_common::file_ops::FileMetadata;
use dice::DiceData;
use dice::DiceDataBuilder;
use dupe::Dupe;
use once_cell::sync::OnceCell;

use crate::directory::ActionDirectoryBuilder;
use crate::directory::ActionSharedDirectory;
use crate::directory::ReDirectorySerializer;
use crate::directory::INTERNER;

/// This configuration describes how to interpret digests received from a RE backend.
#[derive(Copy, Clone, Dupe, Debug, Allocative, Hash, Eq, PartialEq)]
pub struct DigestConfig {}

impl DigestConfig {
    pub fn compat() -> Self {
        Self {}
    }

    pub fn empty_file(&self) -> FileMetadata {
        // TODO: This should be a field on the DigestConfig, obviously.
        FileMetadata::empty()
    }

    pub fn empty_directory(&self) -> ActionSharedDirectory {
        // TODO: This should be a field on the DigestConfig, obviously.
        static EMPTY_DIRECTORY: OnceCell<ActionSharedDirectory> = OnceCell::new();
        EMPTY_DIRECTORY
            .get_or_init(|| {
                ActionDirectoryBuilder::empty()
                    .fingerprint(&ReDirectorySerializer {
                        digest_config: *self,
                    })
                    .shared(&*INTERNER)
            })
            .dupe()
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
