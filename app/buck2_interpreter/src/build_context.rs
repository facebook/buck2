/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_util::late_binding::LateBinding;
use starlark::eval::Evaluator;

use crate::paths::path::StarlarkPath;

pub static STARLARK_PATH_FROM_BUILD_CONTEXT: LateBinding<
    for<'a> fn(&Evaluator<'_, 'a, '_>) -> buck2_error::Result<StarlarkPath<'a>>,
> = LateBinding::new("STARLARK_PATH_FROM_BUILD_CONTEXT");

pub fn starlark_path_from_build_context<'a>(
    eval: &Evaluator<'_, 'a, '_>,
) -> buck2_error::Result<StarlarkPath<'a>> {
    (STARLARK_PATH_FROM_BUILD_CONTEXT.get()?)(eval)
}
