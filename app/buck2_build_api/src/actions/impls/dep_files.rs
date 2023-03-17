/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_util::late_binding::LateBinding;

pub static FLUSH_DEP_FILES: LateBinding<fn()> = LateBinding::new("FLUSH_DEP_FILES");

/// Forget about all dep files. This isn't really meant to be commonly used, but if an invalid dep
/// file was produced and the user wants unblocking, this will provide it.
pub fn flush_dep_files() {
    (FLUSH_DEP_FILES.get().unwrap())();
}
