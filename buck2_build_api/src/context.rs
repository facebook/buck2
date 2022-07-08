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

use async_trait::async_trait;
use buck2_common::result::SharedResult;
use buck2_core::fs::paths::ForwardRelativePath;
use buck2_core::fs::paths::ForwardRelativePathBuf;
use derive_more::Display;
use dice::DiceComputations;
use dice::InjectedKey;
use gazebo::prelude::*;
use owning_ref::ArcRef;

#[async_trait]
pub trait HasBuildContextData {
    async fn get_buck_out_path(&self) -> SharedResult<ArcRef<BuildData, ForwardRelativePath>>;
}

pub trait SetBuildContextData {
    fn set_buck_out_path(&self, path: Option<ForwardRelativePathBuf>);
}

#[derive(PartialEq, Eq)]
pub struct BuildData {
    buck_out_path: ForwardRelativePathBuf,
}

#[derive(Clone, Dupe, Display, Debug, Eq, Hash, PartialEq)]
#[display(fmt = "{:?}", self)]
struct BuildDataKey;

impl InjectedKey for BuildDataKey {
    type Value = SharedResult<Arc<BuildData>>;

    fn compare(x: &Self::Value, y: &Self::Value) -> bool {
        match (x, y) {
            (Ok(x), Ok(y)) => x == y,
            _ => false,
        }
    }
}

#[async_trait]
impl HasBuildContextData for DiceComputations {
    async fn get_buck_out_path(&self) -> SharedResult<ArcRef<BuildData, ForwardRelativePath>> {
        let data = self.compute(&BuildDataKey).await?;
        Ok(ArcRef::new(data).map(|d| AsRef::<ForwardRelativePath>::as_ref(&d.buck_out_path)))
    }
}

impl SetBuildContextData for DiceComputations {
    fn set_buck_out_path(&self, path: Option<ForwardRelativePathBuf>) {
        self.changed_to(vec![(
            BuildDataKey,
            Ok(Arc::new(BuildData {
                buck_out_path: path.unwrap_or_else(|| {
                    ForwardRelativePathBuf::unchecked_new("buck-out/v2".to_owned())
                }),
            })),
        )])
    }
}
