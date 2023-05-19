/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_build_api::bxl::types::BxlFunctionLabel;
use buck2_build_api::bxl::types::CliArgValue;
use buck2_core::base_deferred_key::BaseDeferredKeyDyn;
use buck2_core::collections::ordered_map::OrderedMap;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_core::target::label::TargetLabel;
use buck2_data::action_key_owner::BaseDeferredKeyProto;
use buck2_data::ToProtoMessage;
use dupe::Dupe;
use gazebo::cmp::PartialEqAny;

#[derive(
    Clone,
    Dupe,
    derive_more::Display,
    Debug,
    Eq,
    Hash,
    PartialEq,
    Ord,
    PartialOrd,
    Allocative
)]
pub(crate) struct BxlKey(Arc<BxlKeyData>);

impl BxlKey {
    pub(crate) fn new(
        spec: BxlFunctionLabel,
        bxl_args: Arc<OrderedMap<String, CliArgValue>>,
        global_target_platform: Option<TargetLabel>,
    ) -> Self {
        Self(Arc::new(BxlKeyData {
            spec,
            bxl_args,
            global_target_platform,
        }))
    }

    pub(crate) fn label(&self) -> &BxlFunctionLabel {
        &self.0.spec
    }

    pub(crate) fn cli_args(&self) -> &Arc<OrderedMap<String, CliArgValue>> {
        &self.0.bxl_args
    }

    pub(crate) fn into_base_deferred_key_dyn_impl(self) -> Arc<dyn BaseDeferredKeyDyn> {
        self.0
    }

    pub(crate) fn from_base_deferred_key_dyn_impl(
        key: Arc<dyn BaseDeferredKeyDyn>,
    ) -> Option<Self> {
        key.into_any().downcast().ok().map(BxlKey)
    }

    pub(crate) fn from_base_deferred_key_dyn_impl_err(
        key: Arc<dyn BaseDeferredKeyDyn>,
    ) -> anyhow::Result<Self> {
        Self::from_base_deferred_key_dyn_impl(key).context("Not BxlKey (internal error)")
    }

    pub(crate) fn global_target_platform(&self) -> &Option<TargetLabel> {
        &self.0.global_target_platform
    }
}

#[derive(
    Clone,
    derive_more::Display,
    Debug,
    Eq,
    Hash,
    PartialEq,
    Ord,
    PartialOrd,
    Allocative
)]
#[display(fmt = "{}", "spec")]
struct BxlKeyData {
    spec: BxlFunctionLabel,
    bxl_args: Arc<OrderedMap<String, CliArgValue>>,
    global_target_platform: Option<TargetLabel>,
}

impl BxlKeyData {
    fn as_proto(&self) -> buck2_data::BxlFunctionKey {
        buck2_data::BxlFunctionKey {
            label: Some(self.spec.as_proto()),
        }
    }
}

impl BaseDeferredKeyDyn for BxlKeyData {
    fn eq_token(&self) -> PartialEqAny {
        PartialEqAny::new(self)
    }

    fn hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        Hash::hash(self, &mut hasher);
        hasher.finish()
    }

    fn make_hashed_path(
        &self,
        base: &ProjectRelativePath,
        prefix: &ForwardRelativePath,
        action_key: Option<&str>,
        path: &ForwardRelativePath,
    ) -> ProjectRelativePathBuf {
        let label = &self.spec;
        let cell_relative_path = label.bxl_path.path().path().as_str();

        let output_hash = {
            let mut hasher = DefaultHasher::new();
            self.bxl_args.hash(&mut hasher);
            let output_hash = hasher.finish();
            format!("{:x}", output_hash)
        };

        // It is performance critical that we use slices and allocate via `join` instead of
        // repeated calls to `join` on the path object because `join` allocates on each call,
        // which has a significant impact.
        let parts = [
            base.as_str(),
            "/",
            prefix.as_str(),
            "-bxl/",
            label.bxl_path.cell().as_str(),
            "/",
            output_hash.as_str(),
            "/",
            cell_relative_path,
            if cell_relative_path.is_empty() {
                ""
            } else {
                "/"
            },
            "__",
            label.name.as_str(),
            "__",
            action_key.unwrap_or_default(),
            if action_key.is_none() { "" } else { "__" },
            "/",
            path.as_str(),
        ];

        ProjectRelativePathBuf::unchecked_new(parts.concat())
    }

    fn configured_label(&self) -> Option<ConfiguredTargetLabel> {
        None
    }

    fn to_proto(&self) -> BaseDeferredKeyProto {
        BaseDeferredKeyProto::BxlKey(self.as_proto())
    }

    fn into_any(self: Arc<Self>) -> Arc<dyn Any + Send + Sync> {
        self
    }
}
