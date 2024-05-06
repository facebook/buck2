/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::file_ops::TrackedFileDigest;
use dupe::Dupe;
use remote_execution as RE;

use crate::digest_config::DigestConfig;
use crate::execute::action_digest::ActionDigest;
use crate::execute::blobs::ActionBlobs;
use crate::execute::paths_with_digest::PathsWithDigestBlobData;

#[derive(Clone)]
pub struct ActionDigestAndBlobs {
    pub action: ActionDigest,
    /// The encoded action and other messages referenced from it by digest (e.g. RE::Command).
    /// Does not include the files referenced in inputs.
    pub blobs: ActionBlobs,
}

pub struct ActionDigestAndBlobsBuilder {
    digest_config: DigestConfig,
    blobs: ActionBlobs,
}

impl ActionDigestAndBlobsBuilder {
    pub fn new(digest_config: DigestConfig) -> ActionDigestAndBlobsBuilder {
        ActionDigestAndBlobsBuilder {
            digest_config,
            blobs: ActionBlobs::new(digest_config),
        }
    }

    pub fn add_paths(&mut self, digest: TrackedFileDigest, paths: PathsWithDigestBlobData) {
        self.blobs.add_blob(digest, paths.0);
    }

    pub fn add_command(&mut self, command: &RE::Command) -> TrackedFileDigest {
        self.blobs.add_protobuf_message(command, self.digest_config)
    }

    pub fn build(mut self, action: &RE::Action) -> ActionDigestAndBlobs {
        let action = self.blobs.add_protobuf_message(action, self.digest_config);
        ActionDigestAndBlobs {
            action: action.data().dupe().coerce(),
            blobs: self.blobs,
        }
    }
}
