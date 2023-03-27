/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Injects data used for build onto dice

use std::sync::Arc;

use allocative::Allocative;
use async_trait::async_trait;
use buck2_common::result::SharedResult;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use derive_more::Display;
use dice::DiceComputations;
use dice::DiceTransactionUpdater;
use dice::InjectedKey;
use dupe::Dupe;

#[async_trait]
pub trait HasBuildContextData {
    async fn get_buck_out_path(&self) -> SharedResult<ProjectRelativePathBuf>;
}

pub trait SetBuildContextData {
    fn set_buck_out_path(&mut self, path: Option<ProjectRelativePathBuf>) -> anyhow::Result<()>;
}

#[derive(PartialEq, Eq, Allocative)]
pub struct BuildData {
    buck_out_path: ProjectRelativePathBuf,
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display(fmt = "{:?}", self)]
struct BuildDataKey;

impl InjectedKey for BuildDataKey {
    type Value = Arc<BuildData>;

    fn equality(x: &Self::Value, y: &Self::Value) -> bool {
        x == y
    }
}

#[async_trait]
impl HasBuildContextData for DiceComputations {
    async fn get_buck_out_path(&self) -> SharedResult<ProjectRelativePathBuf> {
        let data = self.compute(&BuildDataKey).await?;
        Ok(data.buck_out_path.to_buf())
    }
}

impl SetBuildContextData for DiceTransactionUpdater {
    fn set_buck_out_path(&mut self, path: Option<ProjectRelativePathBuf>) -> anyhow::Result<()> {
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
