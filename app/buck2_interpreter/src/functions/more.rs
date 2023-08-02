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

/// Globals defined in `buck2_build_api`.
pub static REGISTER_BUCK2_BUILD_API_GLOBALS: LateBinding<fn(&mut GlobalsBuilder)> =
    LateBinding::new("REGISTER_BUCK2_BUILD_API_GLOBALS");
