/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::path::Path;

use buck2_core::fs::project_rel_path::ProjectRelativePath;

pub(crate) fn find_first_valid_parent(mut path: &Path) -> Option<&ProjectRelativePath> {
    loop {
        path = path.parent()?;

        match ProjectRelativePath::new(path) {
            Ok(path) => return Some(path),
            Err(_) => {}
        }
    }
}
