/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::result::{SharedError, SharedResult};
use gazebo::variants::UnpackVariants;
use starlark::values::ProvidesStaticType;

use crate::{
    build::ProviderArtifacts, interpreter::rule_defs::provider::FrozenProviderCollectionValue,
};

#[derive(Clone, Debug, derive_more::Display, ProvidesStaticType, UnpackVariants)]
pub enum StarlarkBuildResult {
    Error(SharedError),
    None,
    #[display(fmt = "successful build result")]
    Built {
        providers: FrozenProviderCollectionValue,
        run_args: Option<Vec<String>>,
        built: Vec<SharedResult<ProviderArtifacts>>,
    },
}

impl StarlarkBuildResult {
    pub(crate) fn new(
        result: SharedResult<
            Option<(
                FrozenProviderCollectionValue,
                Option<Vec<String>>,
                Vec<SharedResult<ProviderArtifacts>>,
            )>,
        >,
    ) -> Self {
        match result {
            Ok(Some((providers, run_args, built))) => Self::Built {
                providers,
                run_args,
                built,
            },
            Ok(None) => Self::None,
            Err(e) => Self::Error(e),
        }
    }
}
