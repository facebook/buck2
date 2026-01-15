/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Injects data used for build onto dice

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_core::fs::buck_out_path::BuckOutPathResolver;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use derive_more::Display;
use dice::DiceComputations;
use dice::DiceTransactionUpdater;
use dice::InjectedKey;
use dupe::Dupe;
use pagable::Pagable;

#[async_trait]
pub trait HasBuildContextData {
    async fn get_buck_out_path(&mut self) -> buck2_error::Result<BuckOutPathResolver>;
}

pub trait SetBuildContextData {
    fn set_buck_out_path(
        &mut self,
        path: Option<ProjectRelativePathBuf>,
    ) -> buck2_error::Result<()>;
}

#[derive(PartialEq, Eq, Allocative, Pagable)]
pub struct BuildData {
    buck_out_path: ProjectRelativePathBuf,
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display("{:?}", self)]
struct BuildDataKey;

impl InjectedKey for BuildDataKey {
    type Value = Arc<BuildData>;

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[async_trait]
impl HasBuildContextData for DiceComputations<'_> {
    async fn get_buck_out_path(&mut self) -> buck2_error::Result<BuckOutPathResolver> {
        let data = self.compute(&BuildDataKey).await?;
        Ok(BuckOutPathResolver::new(data.buck_out_path.to_buf()))
    }
}

impl SetBuildContextData for DiceTransactionUpdater {
    fn set_buck_out_path(
        &mut self,
        path: Option<ProjectRelativePathBuf>,
    ) -> buck2_error::Result<()> {
        Ok(self.changed_to(vec![(
            BuildDataKey,
            Arc::new(BuildData {
                buck_out_path: path.unwrap_or_else(|| {
                    ProjectRelativePathBuf::unchecked_new("buck-out/v2".to_owned())
                }),
            }),
        )])?)
    }
}
