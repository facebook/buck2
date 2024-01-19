/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use buck2_analysis::analysis::env::RuleAnalysisAttrResolutionContext;
use buck2_analysis::attrs::resolve::attr_type::arg::ConfiguredStringWithMacrosExt;
use buck2_analysis::attrs::resolve::attr_type::dep::DepAttrTypeExt;
use buck2_analysis::attrs::resolve::ctx::AttrResolutionContext;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_build_api::keep_going;
use buck2_core::package::PackageLabel;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_interpreter::error::BuckStarlarkError;
use buck2_interpreter::types::configured_providers_label::StarlarkProvidersLabel;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::attrs::attr_type::query::ResolvedQueryLiterals;
use buck2_node::attrs::configured_traversal::ConfiguredAttrTraversal;
use dice::DiceComputations;
use dupe::Dupe;
use futures::stream::FuturesUnordered;
use starlark::values::dict::Dict;
use starlark::values::tuple::AllocTuple;
use starlark::values::Value;
use starlark_map::small_map::SmallMap;

use crate::anon_target_attr::AnonTargetAttr;
use crate::anon_targets::get_artifact_from_anon_target_analysis;
use crate::anon_targets::AnonTargetKey;
use crate::anon_targets::AnonTargetsError;
use crate::promise_artifacts::PromiseArtifactAttr;

// No macros in anon targets, so query results are empty. Execution platform resolution should
// always be inherited from the anon target.
pub(crate) struct AnonTargetAttrResolutionContext<'v> {
    #[allow(unused)] // TODO(@wendyy)
    pub(crate) promised_artifacts_map: HashMap<&'v PromiseArtifactAttr, Artifact>,
    pub(crate) rule_analysis_attr_resolution_ctx: RuleAnalysisAttrResolutionContext<'v>,
}

pub trait AnonTargetAttrResolution {
    fn resolve<'v>(
        &self,
        pkg: PackageLabel,
        ctx: &AnonTargetAttrResolutionContext<'v>,
    ) -> anyhow::Result<Vec<Value<'v>>>;

    fn resolve_single<'v>(
        &self,
        pkg: PackageLabel,
        ctx: &AnonTargetAttrResolutionContext<'v>,
    ) -> anyhow::Result<Value<'v>>;
}

impl AnonTargetAttrResolution for AnonTargetAttr {
    /// "Resolves" the anon target attr value to the resolved value provided to the rule implementation.
    ///
    /// `resolve` may return multiple values. It is up to the caller to fail if
    /// an inappropriate number of elements is returned. e.g. `attrs.list()` might
    /// accept and merge multiple returned values from `attrs.source()`, but
    /// `attrs.optional()` might only accept a single value, and fail otherwise.
    fn resolve<'v>(
        &self,
        pkg: PackageLabel,
        ctx: &AnonTargetAttrResolutionContext<'v>,
    ) -> anyhow::Result<Vec<Value<'v>>> {
        Ok(vec![self.resolve_single(pkg, ctx)?])
    }

    /// Resolving a single value is common, so `resolve_single` will validate
    /// this function's output, and return a single value or an error.
    fn resolve_single<'v>(
        &self,
        pkg: PackageLabel,
        anon_resolution_ctx: &AnonTargetAttrResolutionContext<'v>,
    ) -> anyhow::Result<Value<'v>> {
        let ctx = &anon_resolution_ctx.rule_analysis_attr_resolution_ctx;
        match self {
            AnonTargetAttr::Bool(v) => Ok(Value::new_bool(v.0)),
            AnonTargetAttr::Int(v) => Ok(ctx.heap().alloc(*v)),
            AnonTargetAttr::String(v) | AnonTargetAttr::EnumVariant(v) => {
                Ok(ctx.heap().alloc(v.as_str()))
            }
            AnonTargetAttr::List(list) => {
                let mut values = Vec::with_capacity(list.len());
                for v in list.iter() {
                    values.append(&mut v.resolve(pkg.dupe(), anon_resolution_ctx)?);
                }
                Ok(ctx.heap().alloc(values))
            }
            AnonTargetAttr::Tuple(list) => {
                let mut values = Vec::with_capacity(list.len());
                for v in list.iter() {
                    values.append(&mut v.resolve(pkg.dupe(), anon_resolution_ctx)?);
                }
                Ok(ctx.heap().alloc(AllocTuple(values)))
            }
            AnonTargetAttr::Dict(dict) => {
                let mut res = SmallMap::with_capacity(dict.len());
                for (k, v) in dict.iter() {
                    res.insert_hashed(
                        k.resolve_single(pkg.dupe(), anon_resolution_ctx)?
                            .get_hashed()
                            .map_err(BuckStarlarkError::new)?,
                        v.resolve_single(pkg.dupe(), anon_resolution_ctx)?,
                    );
                }
                Ok(ctx.heap().alloc(Dict::new(res)))
            }
            AnonTargetAttr::None => Ok(Value::new_none()),
            AnonTargetAttr::OneOf(box l, _) => l.resolve_single(pkg, anon_resolution_ctx),
            AnonTargetAttr::Dep(d) => DepAttrType::resolve_single(ctx, d),
            AnonTargetAttr::Artifact(d) => Ok(ctx.heap().alloc(StarlarkArtifact::new(d.clone()))),
            AnonTargetAttr::Arg(a) => a.resolve(ctx, &pkg),
            AnonTargetAttr::PromiseArtifact(artifact) => {
                // TODO(@wendyy) - use promised artifact map here to construct fulfilled `StarlarkPromiseArtifact`
                Ok(ctx.heap().alloc(artifact.clone()))
            }
            AnonTargetAttr::Label(label) => {
                Ok(ctx.heap().alloc(StarlarkProvidersLabel::new(label.clone())))
            }
        }
    }
}

// Container for things that require looking up analysis results in order to resolve the attribute.
pub(crate) struct AnonTargetDependents {
    pub(crate) deps: Vec<ConfiguredTargetLabel>,
    pub(crate) promise_artifacts: Vec<PromiseArtifactAttr>,
}

// Container for analysis results of the anon target dependents.
pub(crate) struct AnonTargetDependentAnalysisResults<'v> {
    pub(crate) dep_analysis_results:
        HashMap<&'v ConfiguredTargetLabel, FrozenProviderCollectionValue>,
    pub(crate) promised_artifacts: HashMap<&'v PromiseArtifactAttr, Artifact>,
}

impl AnonTargetDependents {
    pub(crate) fn get_dependents(
        anon_target: &AnonTargetKey,
    ) -> anyhow::Result<AnonTargetDependents> {
        struct DepTraversal(Vec<ConfiguredTargetLabel>);

        impl ConfiguredAttrTraversal for DepTraversal {
            fn dep(&mut self, dep: &ConfiguredProvidersLabel) -> anyhow::Result<()> {
                self.0.push(dep.target().dupe());
                Ok(())
            }

            fn query(
                &mut self,
                _query: &str,
                _resolved_literals: &ResolvedQueryLiterals<ConfiguredProvidersLabel>,
            ) -> anyhow::Result<()> {
                Err(AnonTargetsError::QueryMacroNotSupported.into())
            }
        }

        let mut dep_traversal = DepTraversal(Vec::new());
        // @TODO(@wendyy) - populate after switching over to PromiseArtifactAttrs
        let promise_artifacts = Vec::new();
        for x in anon_target.0.attrs().values() {
            x.traverse(anon_target.0.name().pkg(), &mut dep_traversal)?;
        }
        Ok(AnonTargetDependents {
            deps: dep_traversal.0,
            promise_artifacts,
        })
    }

    pub(crate) async fn get_analysis_results<'v>(
        &'v self,
        dice: &'v DiceComputations,
    ) -> anyhow::Result<AnonTargetDependentAnalysisResults<'v>> {
        let dep_analysis_results: HashMap<_, _> = keep_going::try_join_all(
            dice,
            self.deps
                .iter()
                .map(async move |dep| {
                    let res = dice
                        .get_analysis_result(dep)
                        .await
                        .and_then(|v| v.require_compatible());
                    res.map(|x| (dep, x.providers().dupe()))
                })
                .collect::<FuturesUnordered<_>>(),
        )
        .await?;

        let promised_artifacts: HashMap<_, _> = keep_going::try_join_all(
            dice,
            self.promise_artifacts
                .iter()
                .map(async move |promise_artifact_attr| {
                    get_artifact_from_anon_target_analysis(&promise_artifact_attr.id, dice)
                        .await
                        .map(|artifact| (promise_artifact_attr, artifact))
                })
                .collect::<FuturesUnordered<_>>(),
        )
        .await?;

        Ok(AnonTargetDependentAnalysisResults {
            dep_analysis_results,
            promised_artifacts,
        })
    }
}
