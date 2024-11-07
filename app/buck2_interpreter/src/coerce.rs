/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::target::label::label::TargetLabel;
use buck2_util::late_binding::LateBinding;
use starlark::eval::Evaluator;

pub static COERCE_TARGET_LABEL_FOR_BZL: LateBinding<
    fn(&mut Evaluator, &str) -> buck2_error::Result<TargetLabel>,
> = LateBinding::new("COERCE_TARGET_LABEL");
