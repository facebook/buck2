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

/// Globals defined in `buck2_bxl` crate,
/// which are used to create the context for `.bxl` evaluation.
pub static BXL_SPECIFIC_GLOBALS: LateBinding<fn(&mut GlobalsBuilder)> =
    LateBinding::new("BXL_SPECIFIC_GLOBALS");
