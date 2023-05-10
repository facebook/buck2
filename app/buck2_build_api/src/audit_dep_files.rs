/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::io::Write;
use std::pin::Pin;

use buck2_core::category::Category;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_util::late_binding::LateBinding;
use dice::DiceTransaction;

/// Implementation of `audit dep-files`.
pub static AUDIT_DEP_FILES: LateBinding<
    for<'a> fn(
        ctx: &'a DiceTransaction,
        ConfiguredTargetLabel,
        Category,
        Option<String>,
        &'a mut (dyn Write + Send),
    ) -> Pin<Box<dyn Future<Output = anyhow::Result<()>> + Send + 'a>>,
> = LateBinding::new("AUDIT_DEP_FILES");
