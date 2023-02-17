/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_common::cas_digest::CasDigestConfig;
use buck2_common::cas_digest::CasDigestConfigError;
use buck2_common::cas_digest::DigestAlgorithm;
use buck2_common::file_ops::FileMetadata;
use derivative::Derivative;
use dice::DiceData;
use dice::DiceDataBuilder;
use dupe::Dupe;
use once_cell::sync::Lazy;
use ref_cast::RefCast;

use crate::directory::ActionDirectoryBuilder;
use crate::directory::ActionSharedDirectory;
use crate::directory::ReDirectorySerializer;
use crate::directory::INTERNER;

/// This configuration describes how to interpret digests received from a RE backend.
#[derive(Copy, Clone, Dupe, Debug, Allocative, Hash, Eq, PartialEq)]
pub struct DigestConfig {
    inner: &'static DigestConfigInner,
}

impl DigestConfig {
    pub fn compat() -> Self {
        static COMPAT: Lazy<DigestConfigInner> =
            Lazy::new(|| DigestConfigInner::new(CasDigestConfig::compat()));

        Self { inner: &COMPAT }
    }

    /// We just Box::leak this since we create one per daemon and as a result just use
    /// CasDigestConfig as a pointer.
    pub fn leak_new(algorithms: Vec<DigestAlgorithm>) -> Result<Self, CasDigestConfigError> {
        let inner = Box::leak(box DigestConfigInner::new(CasDigestConfig::leak_new(
            algorithms,
        )?));
        Ok(Self { inner })
    }

    pub fn cas_digest_config(&self) -> CasDigestConfig {
        self.inner.cas_digest_config
    }

    pub fn empty_file(&self) -> FileMetadata {
        // TODO: This should be a field on the DigestConfig, obviously.
        FileMetadata::empty(self.cas_digest_config())
    }

    pub fn as_directory_serializer(&self) -> &ReDirectorySerializer {
        ReDirectorySerializer::ref_cast(&self.inner.cas_digest_config)
    }

    pub fn empty_directory(&self) -> ActionSharedDirectory {
        self.inner.empty_directory.dupe()
    }
}

#[derive(Debug, Allocative, Derivative, Eq)]
#[derivative(Hash, PartialEq)]
struct DigestConfigInner {
    cas_digest_config: CasDigestConfig,
    #[derivative(PartialEq = "ignore", Hash = "ignore")]
    empty_directory: ActionSharedDirectory,
}

impl DigestConfigInner {
    fn new(cas_digest_config: CasDigestConfig) -> Self {
        let empty_directory = ActionDirectoryBuilder::empty()
            .fingerprint(&ReDirectorySerializer { cas_digest_config })
            .shared(&*INTERNER);

        Self {
            cas_digest_config,
            empty_directory,
        }
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
