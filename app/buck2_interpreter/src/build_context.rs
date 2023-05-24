/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_util::late_binding::LateBinding;
use starlark::eval::Evaluator;

use crate::path::StarlarkPath;

pub static STARLARK_PATH_FROM_BUILD_CONTEXT: LateBinding<
    for<'a> fn(&Evaluator<'_, 'a>) -> anyhow::Result<StarlarkPath<'a>>,
> = LateBinding::new("STARLARK_PATH_FROM_BUILD_CONTEXT");
