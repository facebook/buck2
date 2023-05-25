/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_util::late_binding::LateBinding;
use starlark::environment::GlobalsBuilder;

/// Default version used in the `buck2` binary.
pub static CONFIGURE_BXL_FILE_GLOBALS: LateBinding<fn(&mut GlobalsBuilder)> =
    LateBinding::new("CONFIGURE_BXL_FILE_GLOBALS");
