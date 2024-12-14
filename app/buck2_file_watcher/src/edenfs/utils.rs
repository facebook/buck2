/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::str;

use buck2_data::FileWatcherKind;
use edenfs::Dtype;

pub fn dtype_into_file_watcher_kind(dtype: Dtype) -> FileWatcherKind {
    match dtype {
        Dtype::DIR => FileWatcherKind::Directory,
        Dtype::LINK => FileWatcherKind::Symlink,
        _ => FileWatcherKind::File,
    }
}

pub fn bytes_to_string_or_unknown(bytes: &[u8]) -> &str {
    str::from_utf8(bytes).ok().unwrap_or("unknown")
}
