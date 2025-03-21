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
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::hash::Hasher;
use std::sync::Arc;

use allocative::Allocative;
use buck2_analysis::analysis::env::get_deps_from_analysis_results;
use buck2_analysis::analysis::env::RuleAnalysisAttrResolutionContext;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_build_api::anon_target::AnonTargetDependentAnalysisResults;
use buck2_build_api::anon_target::AnonTargetDyn;
use buck2_build_api::artifact_groups::promise::PromiseArtifactId;
use buck2_build_api::artifact_groups::promise::PromiseArtifactResolveError;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLike;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::pair::ConfigurationNoExec;
use buck2_core::deferred::base_deferred_key::BaseDeferredKey;
use buck2_core::deferred::base_deferred_key::BaseDeferredKeyDyn;
use buck2_core::execution_types::execution::ExecutionPlatformResolution;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_core::target::label::label::TargetLabel;
use buck2_data::action_key_owner::BaseDeferredKeyProto;
use buck2_data::ToProtoMessage;
use buck2_interpreter::dice::starlark_provider::StarlarkEvalKind;
use buck2_node::rule_type::StarlarkRuleType;
use cmp_any::PartialEqAny;
use dupe::Dupe;
use starlark::collections::SmallMap;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::structs::AllocStruct;
use starlark::values::structs::StructRef;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueOfUncheckedGeneric;
use starlark_map::sorted_map::SortedMap;

use crate::anon_target_attr::AnonTargetAttr;
use crate::anon_target_attr_resolve::AnonTargetAttrResolution;
use crate::anon_target_attr_resolve::AnonTargetAttrResolutionContext;

#[derive(Hash, Eq, PartialEq, Clone, Debug, Allocative)]
pub(crate) struct AnonTarget {
    /// Not necessarily a "real" target label that actually exists, but could be.
    name: TargetLabel,
    /// The type of the rule we are running.
    rule_type: Arc<StarlarkRuleType>,
    /// The attributes the target was defined with.
    /// We use a sorted map since we want to iterate in a defined order.
    attrs: SortedMap<String, AnonTargetAttr>,
    /// The execution configuration - same as the parent.
    exec_cfg: ConfigurationNoExec,
    /// The hash of the `rule_type` and `attrs` for bzl anon targets.
    /// Or
    /// The hash of the `rule_type`, `attrs` and `global_cfg_options` for bxl anon targets.
    hash: String,
    /// Variant of the anon target, either bxl or bzl.
    variant: AnonTargetVariant,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug, Allocative)]
pub(crate) enum AnonTargetVariant {
    Bzl,
    Bxl(GlobalCfgOptions),
}

impl fmt::Display for AnonTarget {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} (anon: {}) ({})",
            self.name(),
            self.partial_hash(),
            self.exec_cfg()
        )
    }
}

impl AnonTarget {
    fn mk_hash_bzl(
        rule_type: &StarlarkRuleType,
        attrs: &SortedMap<String, AnonTargetAttr>,
    ) -> String {
        // This is the same hasher as we use for Configuration, so is probably fine.
        // But quite possibly should be a crypto hasher in future.
        let mut hasher = DefaultHasher::new();
        rule_type.hash(&mut hasher);
        attrs.hash(&mut hasher);
        format!("{:x}", hasher.finish())
    }

    fn mk_hash_bxl(
        rule_type: &StarlarkRuleType,
        attrs: &SortedMap<String, AnonTargetAttr>,
        global_cfg_options: &GlobalCfgOptions,
    ) -> String {
        let mut hasher = DefaultHasher::new();
        rule_type.hash(&mut hasher);
        attrs.hash(&mut hasher);
        global_cfg_options.hash(&mut hasher);
        format!("{:x}", hasher.finish())
    }

    pub(crate) fn as_proto(&self) -> buck2_data::AnonTarget {
        buck2_data::AnonTarget {
            name: Some(self.name().as_proto()),
            execution_configuration: Some(self.exec_cfg().cfg().as_proto()),
            hash: self.partial_hash().to_owned(),
        }
    }

    pub(crate) fn new(
        rule_type: Arc<StarlarkRuleType>,
        name: TargetLabel,
        attrs: SortedMap<String, AnonTargetAttr>,
        exec_cfg: ConfigurationNoExec,
    ) -> Self {
        let hash = Self::mk_hash_bzl(&rule_type, &attrs);
        AnonTarget {
            name,
            rule_type,
            attrs,
            exec_cfg,
            hash,
            variant: AnonTargetVariant::Bzl,
        }
    }

    pub(crate) fn new_bxl(
        rule_type: Arc<StarlarkRuleType>,
        name: TargetLabel,
        attrs: SortedMap<String, AnonTargetAttr>,
        exec_cfg: ConfigurationNoExec,
        global_cfg_options: GlobalCfgOptions,
    ) -> Self {
        let hash = Self::mk_hash_bxl(&rule_type, &attrs, &global_cfg_options);
        AnonTarget {
            name,
            rule_type,
            attrs,
            exec_cfg,
            hash,
            variant: AnonTargetVariant::Bxl(global_cfg_options),
        }
    }

    pub(crate) fn name(&self) -> &TargetLabel {
        &self.name
    }

    pub(crate) fn attrs(&self) -> &SortedMap<String, AnonTargetAttr> {
        &self.attrs
    }

    /// The hash of the rule type and attributes for bzl anon targets.
    /// Or
    /// The hash of the rule type, attributes and global_cfg_options for bxl anon targets.
    fn partial_hash(&self) -> &str {
        &self.hash
    }

    pub(crate) fn exec_cfg(&self) -> &ConfigurationNoExec {
        &self.exec_cfg
    }

    pub(crate) fn configured_label(&self) -> ConfiguredTargetLabel {
        // We need a configured label, but we don't have a real configuration (because it doesn't make sense),
        // so create a dummy version
        self.name().configure(ConfigurationData::unspecified())
    }

    pub(crate) fn anon_target_type(&self) -> &AnonTargetVariant {
        &self.variant
    }
}

impl AnonTargetDyn for AnonTarget {
    fn rule_type(&self) -> &Arc<StarlarkRuleType> {
        &self.rule_type
    }

    fn base_deferred_key(self: Arc<Self>) -> BaseDeferredKey {
        BaseDeferredKey::AnonTarget(self)
    }

    fn resolve_attrs<'v>(
        &self,
        env: &'v Module,
        dependents_analyses: AnonTargetDependentAnalysisResults<'v>,
        exec_resolution: ExecutionPlatformResolution,
    ) -> buck2_error::Result<ValueOfUncheckedGeneric<Value<'v>, StructRef<'static>>> {
        let dep_analysis_results =
            get_deps_from_analysis_results(dependents_analyses.dep_analysis_results)?;

        // No attributes are allowed to contain macros or other stuff, so an empty resolution context works
        let rule_analysis_attr_resolution_ctx = RuleAnalysisAttrResolutionContext {
            module: &env,
            dep_analysis_results,
            query_results: HashMap::new(),
            execution_platform_resolution: exec_resolution,
        };

        let resolution_ctx = AnonTargetAttrResolutionContext {
            promised_artifacts_map: dependents_analyses.promised_artifacts,
            rule_analysis_attr_resolution_ctx,
        };

        let mut resolved_attrs = Vec::with_capacity(self.attrs().len());
        for (name, attr) in self.attrs().iter() {
            resolved_attrs.push((
                name,
                attr.resolve_single(self.name().pkg(), &resolution_ctx)?,
            ));
        }
        let attributes = env
            .heap()
            .alloc_typed_unchecked(AllocStruct(resolved_attrs))
            .cast();
        Ok(attributes)
    }

    fn get_fulfilled_promise_artifacts<'v>(
        self: Arc<Self>,
        promise_artifact_mappings: SmallMap<String, Value<'v>>,
        anon_target_result: Value<'v>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> buck2_error::Result<HashMap<PromiseArtifactId, Artifact>> {
        let mut fulfilled_artifact_mappings = HashMap::new();

        for (id, func) in promise_artifact_mappings.values().enumerate() {
            let artifact = eval.eval_function(*func, &[anon_target_result], &[])?;

            let promise_id = PromiseArtifactId::new(BaseDeferredKey::AnonTarget(self.dupe()), id);

            match ValueAsArtifactLike::unpack_value(artifact)? {
                Some(artifact) => {
                    fulfilled_artifact_mappings
                        .insert(promise_id.clone(), artifact.0.get_bound_artifact()?);
                }
                None => {
                    return Err(
                        PromiseArtifactResolveError::NotAnArtifact(artifact.to_repr()).into(),
                    );
                }
            }
        }

        Ok(fulfilled_artifact_mappings)
    }

    fn eval_kind(self: Arc<Self>) -> StarlarkEvalKind {
        StarlarkEvalKind::AnonTarget(self)
    }
}

impl BaseDeferredKeyDyn for AnonTarget {
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
        let cell_relative_path = self.name().pkg().cell_relative_path().as_str();

        // It is performance critical that we use slices and allocate via `join` instead of
        // repeated calls to `join` on the path object because `join` allocates on each call,
        // which has a significant impact.
        let parts = [
            base.as_str(),
            "/",
            prefix.as_str(),
            "-anon/",
            self.name().pkg().cell_name().as_str(),
            "/",
            self.exec_cfg().cfg().output_hash().as_str(),
            cell_relative_path,
            if cell_relative_path.is_empty() {
                ""
            } else {
                "/"
            },
            self.partial_hash(),
            "/__",
            self.name().name().as_str(),
            "__",
            action_key.unwrap_or_default(),
            if action_key.is_none() { "" } else { "__" },
            "/",
            path.as_str(),
        ];

        ProjectRelativePathBuf::unchecked_new(parts.concat())
    }

    fn configured_label(&self) -> Option<ConfiguredTargetLabel> {
        Some(self.configured_label())
    }

    fn to_proto(&self) -> BaseDeferredKeyProto {
        BaseDeferredKeyProto::AnonTarget(self.as_proto())
    }

    fn into_any(self: Arc<Self>) -> Arc<dyn Any + Send + Sync> {
        self
    }

    fn execution_platform_resolution(&self) -> &ExecutionPlatformResolution {
        // TODO(wendyy) support exec platforms for anon targets
        unimplemented!("Execution platforms are not supported for anon targets (yet)")
    }

    fn global_cfg_options(&self) -> Option<GlobalCfgOptions> {
        match &self.variant {
            AnonTargetVariant::Bzl => None,
            AnonTargetVariant::Bxl(global_cfg_options) => Some(global_cfg_options.dupe()),
        }
    }
}
