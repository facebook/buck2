/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use anyhow::Context;
use buck2_core::fs::fs_util;
use buck2_core::fs::paths::AbsPathBuf;
use buck2_core::fs::paths::FileNameBuf;
use buck2_core::fs::project::ProjectRoot;

// Once we start storing disk state in the cache directory, we need to make sure
// buck2 always deletes the cache directory if the cache is disabled.
// Otherwise, buck-out state can diverge from the state of on-disk cache when
// cache is disabled, causing buck2 to use stale cache when reading from the
// cache is re-enabled. One way this can happen is that someone can build on
// an older revision with a buck2 that doesn't understand the cache directory
// in between 2 builds on newer revisions with buck2 that reads from the cache
// (for ex., as a part of a bisect), then the state can become stale.
// There are 2 (not foolproof) mitigations planned:
// 1) Read from the logs what the last buck2 invocation was and check that the
// last buck2 supported on-disk state. If not, delete the disk state.
// 2) Start always deleting the cache directory now until we add support for disk
// state in buck2.
// The following implements mitigation #2 by always deleting disk state.
pub(crate) fn delete_unknown_disk_state(
    cache_dir_path: AbsPathBuf,
    fs: ProjectRoot,
) -> anyhow::Result<()> {
    let res: anyhow::Result<()> = try {
        if cache_dir_path.exists() {
            for file in fs_util::read_dir(&cache_dir_path)? {
                let filename = file?
                    .file_name()
                    .to_str()
                    .context("Filename is not UTF-8")
                    .and_then(|f| FileNameBuf::try_from(f.to_owned()))?;
                fs.remove_path_recursive(&cache_dir_path.join(filename))?;
            }
        }
    };

    res.with_context(|| {
        format!(
            "deleting unrecognized caches in {} to prevent them from going stale",
            &cache_dir_path
        )
    })
}
