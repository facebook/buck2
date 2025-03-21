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
use buck2_build_api::bxl::types::BxlFunctionLabel;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::deferred::base_deferred_key::BaseDeferredKeyBxl;
use buck2_core::deferred::base_deferred_key::BaseDeferredKeyDyn;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_data::action_key_owner::BaseDeferredKeyProto;
use buck2_data::ToProtoMessage;
use buck2_error::BuckErrorContext;
use buck2_interpreter::dice::starlark_provider::StarlarkEvalKind;
use cmp_any::PartialEqAny;
use dupe::Dupe;
use starlark_map::ordered_map::OrderedMap;

use crate::bxl::starlark_defs::cli_args::CliArgValue;
use crate::bxl::starlark_defs::context::actions::BxlExecutionResolution;

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
        force_print_stacktrace: bool,
        global_cfg_options: GlobalCfgOptions,
    ) -> Self {
        Self(Arc::new(BxlKeyData {
            spec,
            bxl_args,
            force_print_stacktrace,
            global_cfg_options,
        }))
    }

    pub(crate) fn label(&self) -> &BxlFunctionLabel {
        &self.0.spec
    }

    pub(crate) fn cli_args(&self) -> &Arc<OrderedMap<String, CliArgValue>> {
        &self.0.bxl_args
    }

    fn into_base_deferred_key_dyn_impl(
        self,
        execution_resolution: BxlExecutionResolution,
    ) -> Arc<dyn BaseDeferredKeyDyn> {
        Arc::new(BxlDynamicKeyData {
            key: self.0,
            execution_resolution,
        })
    }

    pub(crate) fn into_base_deferred_key(
        self,
        execution_resolution: BxlExecutionResolution,
    ) -> BaseDeferredKey {
        BaseDeferredKey::BxlLabel(BaseDeferredKeyBxl(
            self.into_base_deferred_key_dyn_impl(execution_resolution),
        ))
    }

    pub(crate) fn from_base_deferred_key_dyn_impl_err(
        key: BaseDeferredKeyBxl,
    ) -> buck2_error::Result<Self> {
        BxlDynamicKey::from_base_deferred_key_dyn_impl(key)
            .map(|k| BxlKey(k.0.key.dupe()))
            .internal_error("Not BxlKey")
    }

    pub(crate) fn global_cfg_options(&self) -> &GlobalCfgOptions {
        &self.0.global_cfg_options
    }

    pub(crate) fn force_print_stacktrace(&self) -> bool {
        self.0.force_print_stacktrace
    }

    pub(crate) fn as_starlark_eval_kind(&self) -> StarlarkEvalKind {
        StarlarkEvalKind::Bxl(self.0.dupe())
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
#[display("{}", spec)]
struct BxlKeyData {
    spec: BxlFunctionLabel,
    bxl_args: Arc<OrderedMap<String, CliArgValue>>,
    /// Overrides `fail_no_stacktrace` to print a stacktrace anyway. FIXME(JakobDegen): Might be
    /// better to put this on the `UserComputationData` instead, to keep this from invalidating the
    /// dice node. A bit hard to wire up though, so just leave it here for now.
    force_print_stacktrace: bool,
    global_cfg_options: GlobalCfgOptions,
}

impl BxlKeyData {
    fn as_proto(&self) -> buck2_data::BxlFunctionKey {
        buck2_data::BxlFunctionKey {
            label: Some(self.spec.as_proto()),
        }
    }
}

// Note that exec_deps and toolchains are not used as a part of the hashed path directly. During normal BXL actions
// instantiation, these are used to resolve the execution platform resolution, which is also present in
// BxlDynamicKeyData, and _is_ used to make the hashed path. Thus, exec_deps and toolchains are indirectly used to
// construct the hashed path. However, we still need to include them in the BxlDynamicKeyData so that we can pass
// them from the root BXL to the dynamic BXL context, and then access them on the dynamic BXL context's actions factory.
#[derive(Clone, derive_more::Display, Debug, Eq, Hash, PartialEq, Allocative)]
#[display("{}", key)]
pub(crate) struct BxlDynamicKeyData {
    key: Arc<BxlKeyData>,
    pub(crate) execution_resolution: BxlExecutionResolution,
}

pub(crate) struct BxlDynamicKey(pub(crate) Arc<BxlDynamicKeyData>);

impl BxlDynamicKey {
    pub(crate) fn key(&self) -> BxlKey {
        BxlKey(self.0.key.dupe())
    }

    fn from_base_deferred_key_dyn_impl(key: BaseDeferredKeyBxl) -> Option<Self> {
        key.0.into_any().downcast().ok().map(BxlDynamicKey)
    }

    pub(crate) fn from_base_deferred_key_dyn_impl_err(
        key: BaseDeferredKeyBxl,
    ) -> buck2_error::Result<Self> {
        Self::from_base_deferred_key_dyn_impl(key).internal_error("Not BxlDynamicKey")
    }
}

impl BaseDeferredKeyDyn for BxlDynamicKeyData {
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
        let label = &self.key.spec;
        let cell_relative_path = label.bxl_path.path().path().as_str();

        let output_hash = {
            let mut hasher = DefaultHasher::new();
            self.key.bxl_args.hash(&mut hasher);
            self.key.global_cfg_options.hash(&mut hasher);
            let output_hash = hasher.finish();
            format!("{:x}", output_hash)
        };

        let exec_platform = {
            let mut hasher = DefaultHasher::new();
            self.execution_resolution
                .resolved_execution
                .hash(&mut hasher);
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
            exec_platform.as_str(),
            "__",
            "/",
            path.as_str(),
        ];

        ProjectRelativePathBuf::unchecked_new(parts.concat())
    }

    fn configured_label(&self) -> Option<ConfiguredTargetLabel> {
        None
    }

    fn to_proto(&self) -> BaseDeferredKeyProto {
        BaseDeferredKeyProto::BxlKey(self.key.as_proto())
    }

    fn into_any(self: Arc<Self>) -> Arc<dyn Any + Send + Sync> {
        self
    }

    fn execution_platform_resolution(&self) -> &ExecutionPlatformResolution {
        &self.execution_resolution.resolved_execution
    }

    fn global_cfg_options(&self) -> Option<GlobalCfgOptions> {
        Some(self.key.global_cfg_options.dupe())
    }
}
