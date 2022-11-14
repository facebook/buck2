/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use buck2_common::file_ops::FileDigest;
use buck2_common::file_ops::TrackedFileDigest;
use gazebo::dupe::Dupe;
use prost::Message;

/// Contains small blobs referenced from action messages (does not include any file contents blobs).
pub struct ActionBlobs(HashMap<TrackedFileDigest, Vec<u8>>);

impl ActionBlobs {
    pub fn new() -> Self {
        // We add empty files to the input that don't exist in disk; so add
        // the empty digest to blobs, as going to disk would fail.
        let mut blobs = HashMap::new();
        blobs.insert(TrackedFileDigest::empty(), Vec::new());
        Self(blobs)
    }

    pub fn add_blob(&mut self, digest: TrackedFileDigest, data: Vec<u8>) {
        self.0.insert(digest, data);
    }

    pub fn add_protobuf_message(&mut self, m: &impl Message) -> TrackedFileDigest {
        let mut blob = Vec::new();
        // Unwrap is safe because it only fails in OOM conditions, which we pretend don't happen
        m.encode(&mut blob).unwrap();
        let digest = TrackedFileDigest::new(FileDigest::from_bytes_sha1(&blob));
        self.0.insert(digest.dupe(), blob);
        digest
    }

    pub fn keys(&self) -> impl Iterator<Item = &TrackedFileDigest> {
        self.0.keys()
    }

    pub fn get(&self, digest: &TrackedFileDigest) -> Option<&Vec<u8>> {
        self.0.get(digest)
    }
}
