/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Vocabulary types for persisting the local dep-file (local action) cache to disk.
//!
//! The in-memory cache (`DEP_FILES`) lives in `buck2_action_impl`; the SQLite database lives in
//! `buck2_execute_impl`; and the daemon assembles them in `buck2_server`. None of those three
//! crates depend on each other, so the shared types and the late-bound handles that bridge them
//! live here in `buck2_execute`, which they all depend on.

use std::sync::Arc;

use allocative::Allocative;
use buck2_common::file_ops::metadata::FileDigest;
use buck2_common::file_ops::metadata::TrackedFileDigest;
use buck2_fs::paths::forward_rel_path::ForwardRelativePathBuf;
use buck2_util::late_binding::LateBinding;

use crate::artifact_value::ArtifactValue;

/// A single dep-file cache entry in the configuration-independent form that is persisted to disk and
/// reloaded across daemon restarts. This carries only what the identical-action (`LocalActionCache`)
/// lookup needs: the command digests, the config-independent declared dep-file identities, and the
/// action's outputs keyed by their target-relative path (the configuration-dependent
/// `BuildArtifactPath` is reconstructed from the live action's declared outputs at lookup time).
#[derive(Clone, Debug)]
pub struct StoredDepFileState {
    /// Raw bytes of the expanded command line digest. Stored as bytes (rather than
    /// `ExpandedCommandLineDigest`) so this type need not depend on `buck2_build_api`.
    pub cli_digest: Vec<u8>,
    /// Digest of the action's input directory.
    pub directory_digest: FileDigest,
    /// Digest of the local worker's input directory, if any.
    pub local_worker_directory_digest: Option<TrackedFileDigest>,
    pub was_produced_locally: bool,
    /// Config-independent identities of the dep files this action declared. Empty if the action
    /// declared no dep files.
    pub declared: Vec<StoredDepFileIdentity>,
    /// The action's outputs, keyed by target-relative path.
    pub outputs: Vec<StoredOutput>,
}

/// One persisted output of a dep-file cache entry, keyed by its target-relative path.
#[derive(Clone, Debug, Allocative)]
pub struct StoredOutput {
    pub path: ForwardRelativePathBuf,
    pub value: StoredOutputValue,
}

/// The persisted form of an output's value.
#[derive(Clone, Debug, Allocative)]
pub enum StoredOutputValue {
    /// A leaf output (file/symlink): the value is small and stored in full, so it is reconstructed
    /// directly from the row.
    Leaf(ArtifactValue),
    /// A directory output: only its fingerprint is persisted. The full tree is rehydrated from the
    /// materializer (which already persists+reloads it) at lookup time and verified against this
    /// fingerprint, avoiding a second serialization of the tree here.
    Directory(FileDigest),
}

/// The configuration-independent identity of a declared dep file, mirroring the tuple compared by
/// `DeclaredDepFiles::declares_same_dep_files` in `buck2_action_impl`.
#[derive(Clone, Debug)]
pub struct StoredDepFileIdentity {
    pub label: String,
    /// Target-relative output path of the dep file.
    pub path: String,
    /// Projected path within the output artifact.
    pub projected: String,
    pub is_content_based: bool,
}

/// Write-through handle to the persisted dep-file database. Implemented in `buck2_execute_impl` and
/// installed by `buck2_server` once the database is opened. All methods must be crash-safe and must
/// never fail the build: implementations downgrade database errors to soft errors.
pub trait DepFileStore: Send + Sync + 'static {
    /// Persist (or replace) the entry for `(logical_key, config_key)`. Keys are opaque digests
    /// (stored as `BLOB`), not decoded.
    fn insert(&self, logical_key: Vec<u8>, config_key: Vec<u8>, state: StoredDepFileState);
    /// Remove the entry for `(logical_key, config_key)`, if present.
    fn delete(&self, logical_key: Vec<u8>, config_key: Vec<u8>);
    /// Every persisted entry for `logical_key`, one per configuration it was built under (empty on a
    /// miss or on any database error). The in-memory cache calls this on demand for a logical action
    /// it has no live entry for, rather than loading the whole database at startup.
    fn get(&self, logical_key: &[u8]) -> Vec<StoredDepFileState>;
    /// Remove all entries.
    fn clear(&self);
}

/// The persisted dep-file store, installed by `buck2_server` at daemon startup. Absent (`get()`
/// returns `Err`) when persistence is disabled or in tests, in which case the cache stays purely
/// in-memory.
pub static DEP_FILE_STORE: LateBinding<Arc<dyn DepFileStore>> = LateBinding::new("DEP_FILE_STORE");
