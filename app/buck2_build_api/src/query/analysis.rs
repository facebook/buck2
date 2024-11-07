/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::future::Future;
use std::pin::Pin;

use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_util::late_binding::LateBinding;
use dice::DiceComputations;
use indexmap::IndexMap;

/// Used by `audit classpath`.
pub static CLASSPATH_FOR_TARGETS: LateBinding<
    for<'c> fn(
        &'c mut DiceComputations,
        Vec<ConfiguredTargetLabel>,
    ) -> Pin<
        Box<
            dyn Future<Output = buck2_error::Result<IndexMap<ConfiguredTargetLabel, Artifact>>>
                + Send
                + 'c,
        >,
    >,
> = LateBinding::new("CLASSPATH_FOR_TARGETS");
