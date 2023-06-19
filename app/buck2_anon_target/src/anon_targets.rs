/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;
use std::fmt::Debug;
use std::mem;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context as _;
use async_trait::async_trait;
use buck2_analysis::analysis::calculation::get_rule_impl;
use buck2_analysis::analysis::env::RuleAnalysisAttrResolutionContext;
use buck2_analysis::analysis::env::RuleImplFunction;
use buck2_build_api::analysis::anon_promises_dyn::AnonPromisesDyn;
use buck2_build_api::analysis::anon_targets_registry::AnonTargetsRegistryDyn;
use buck2_build_api::analysis::anon_targets_registry::ANON_TARGET_REGISTRY_NEW;
use buck2_build_api::analysis::calculation::RuleAnalysisCalculation;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::deferred::calculation::EVAL_ANON_TARGET;
use buck2_build_api::deferred::types::DeferredTable;
use buck2_build_api::interpreter::rule_defs::context::AnalysisContext;
use buck2_build_api::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use buck2_build_api::interpreter::rule_defs::provider::collection::ProviderCollection;
use buck2_build_api::keep_going;
use buck2_common::result::SharedResult;
use buck2_configured::nodes::calculation::find_execution_platform_by_configuration;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_core::base_deferred_key::BaseDeferredKeyDyn;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::configuration::config_setting::ConfigSettingData;
use buck2_core::configuration::data::ConfigurationData;
use buck2_core::configuration::pair::ConfigurationNoExec;
use buck2_core::configuration::pair::ConfigurationWithExec;
use buck2_core::configuration::transition::applied::TransitionApplied;
use buck2_core::configuration::transition::id::TransitionId;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::lex_target_pattern;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::PatternData;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::label::ConfiguredTargetLabel;
use buck2_core::target::label::TargetLabel;
use buck2_core::target::name::TargetNameRef;
use buck2_core::unsafe_send_future::UnsafeSendFuture;
use buck2_events::dispatch::get_dispatcher;
use buck2_events::dispatch::span_async;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_interpreter::dice::starlark_provider::with_starlark_eval_provider;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use buck2_interpreter::types::label::Label;
use buck2_interpreter_for_build::rule::FrozenRuleCallable;
use buck2_node::attrs::attr_type::query::ResolvedQueryLiterals;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coerced_path::CoercedPath;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::attrs::configured_traversal::ConfiguredAttrTraversal;
use buck2_node::attrs::internal::internal_attrs;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use buck2_util::arc_str::ArcSlice;
use buck2_util::arc_str::ArcStr;
use buck2_util::collections::ordered_map::OrderedMap;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use futures::stream::FuturesUnordered;
use futures::Future;
use futures::FutureExt;
use gazebo::prelude::*;
use more_futures::cancellation::CancellationContext;
use starlark::any::AnyLifetime;
use starlark::any::ProvidesStaticType;
use starlark::environment::Module;
use starlark::values::dict::DictOf;
use starlark::values::structs::AllocStruct;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueTyped;
use thiserror::Error;

use crate::anon_promises::AnonPromises;
use crate::anon_target_attr::AnonTargetAttr;
use crate::anon_target_attr_coerce::AnonTargetAttrTypeCoerce;
use crate::anon_target_attr_resolve::AnonTargetAttrExt;
use crate::anon_target_node::AnonTarget;

#[derive(Debug, Trace, Allocative, ProvidesStaticType)]
pub struct AnonTargetsRegistry<'v> {
    // We inherit the execution platform of our parent
    execution_platform: ExecutionPlatformResolution,
    promises: AnonPromises<'v>,
}

#[derive(Debug, Error)]
pub enum AnonTargetsError {
    #[error("Not allowed to call `anon_targets` in this context")]
    AssertNoPromisesFailed,
    #[error(
        "Invalid `name` attribute, must be a label or a string, got `{value}` of type `{typ}`"
    )]
    InvalidNameType { typ: String, value: String },
    #[error("`name` attribute must be a valid target label, got `{0}`")]
    NotTargetLabel(String),
    #[error("can't parse strings during `anon_targets` coercion, got `{0}`")]
    CantParseDuringCoerce(String),
    #[error("Unknown attribute `{0}`")]
    UnknownAttribute(String),
    #[error("Internal attribute `{0}` not allowed as argument to `anon_targets`")]
    InternalAttribute(String),
    #[error("Missing attribute `{0}`")]
    MissingAttribute(String),
    #[error("Query macros are not supported")]
    QueryMacroNotSupported,
}

#[derive(Hash, Eq, PartialEq, Clone, Dupe, Debug, Display, Trace, Allocative)]
pub(crate) struct AnonTargetKey(Arc<AnonTarget>);

impl AnonTargetKey {
    fn downcast(key: Arc<dyn BaseDeferredKeyDyn>) -> anyhow::Result<Self> {
        Ok(AnonTargetKey(
            key.into_any()
                .downcast()
                .ok()
                .context("Expecting AnonTarget (internal error)")?,
        ))
    }

    pub(crate) fn new<'v>(
        execution_platform: &ExecutionPlatformResolution,
        rule: ValueTyped<'v, FrozenRuleCallable>,
        attributes: DictOf<'v, &'v str, Value<'v>>,
    ) -> anyhow::Result<Self> {
        let mut name = None;
        let internal_attrs = internal_attrs();

        let entries = attributes.collect_entries();
        let attrs_spec = rule.attributes();
        let mut attrs = OrderedMap::with_capacity(attrs_spec.len());
        for (k, v) in entries {
            if k == "name" {
                name = Some(Self::coerce_name(v)?);
            } else if internal_attrs.contains_key(k) {
                return Err(AnonTargetsError::InternalAttribute(k.to_owned()).into());
            } else {
                let attr = attrs_spec
                    .attribute(k)
                    .ok_or_else(|| AnonTargetsError::UnknownAttribute(k.to_owned()))?;
                attrs.insert(
                    k.to_owned(),
                    Self::coerce_to_anon_target_attr(attr.coercer(), v)
                        .with_context(|| format!("Error coercing attribute `{}`", k))?,
                );
            }
        }
        for (k, _, a) in attrs_spec.attr_specs() {
            if !attrs.contains_key(k) && !internal_attrs.contains_key(k) {
                if let Some(x) = a.default() {
                    attrs.insert(
                        k.to_owned(),
                        Self::coerced_to_anon_target_attr(x, a.coercer())?,
                    );
                } else {
                    return Err(AnonTargetsError::MissingAttribute(k.to_owned()).into());
                }
            }
        }

        // We need to ensure there is a "name" attribute which corresponds to something we can turn in to a label.
        // If there isn't a good one, make something up
        let name = match name {
            None => Self::create_name(&rule.rule_type().name)?,
            Some(name) => name,
        };

        Ok(Self(Arc::new(AnonTarget::new(
            rule.rule_type().dupe(),
            name,
            attrs.into(),
            execution_platform.cfg().dupe(),
        ))))
    }

    /// We need to parse a TargetLabel from a String, but it doesn't matter if the pieces aren't
    /// valid targets in the context of this build (e.g. if the package really exists),
    /// just that it is syntactically valid.
    fn parse_target_label(x: &str) -> anyhow::Result<TargetLabel> {
        let err = || AnonTargetsError::NotTargetLabel(x.to_owned());
        let lex = lex_target_pattern::<TargetPatternExtra>(x, false).with_context(err)?;
        // TODO(nga): `CellName` contract requires it refers to declared cell name.
        //   This `unchecked_new` violates it.
        let cell =
            CellName::unchecked_new(lex.cell_alias.filter(|a| !a.is_empty()).unwrap_or("anon"))?;
        match lex.pattern.reject_ambiguity()? {
            PatternData::TargetInPackage {
                package,
                target_name,
                extra: TargetPatternExtra,
            } => Ok(TargetLabel::new(
                PackageLabel::new(cell, CellRelativePath::new(package)),
                target_name.as_ref(),
            )),
            _ => Err(err().into()),
        }
    }

    fn create_name(rule_name: &str) -> anyhow::Result<TargetLabel> {
        // TODO(nga): this creates non-existing cell reference.
        let cell_name = CellName::unchecked_new("anon")?;
        let pkg = PackageLabel::new(cell_name, CellRelativePath::empty());
        Ok(TargetLabel::new(pkg, TargetNameRef::new(rule_name)?))
    }

    fn coerce_name(x: Value) -> anyhow::Result<TargetLabel> {
        if let Some(x) = Label::from_value(x) {
            Ok(x.label().target().unconfigured().dupe())
        } else if let Some(x) = x.unpack_str() {
            Self::parse_target_label(x)
        } else {
            Err(AnonTargetsError::InvalidNameType {
                typ: x.get_type().to_owned(),
                value: x.to_string(),
            }
            .into())
        }
    }

    fn coerce_to_anon_target_attr(attr: &AttrType, x: Value) -> anyhow::Result<AnonTargetAttr> {
        let ctx = AnonAttrCtx::new();

        attr.coerce_item(AttrIsConfigurable::No, &ctx, x)
    }

    fn coerced_to_anon_target_attr(
        x: &CoercedAttr,
        ty: &AttrType,
    ) -> anyhow::Result<AnonTargetAttr> {
        AnonTargetAttr::from_coerced_attr(x, ty, &AnonAttrCtx::new())
    }

    pub(crate) async fn resolve(&self, dice: &DiceComputations) -> anyhow::Result<AnalysisResult> {
        #[async_trait]
        impl Key for AnonTargetKey {
            type Value = SharedResult<AnalysisResult>;

            async fn compute(
                &self,
                ctx: &DiceComputations,
                _cancellation: &CancellationContext,
            ) -> Self::Value {
                Ok(self.run_analysis(ctx).await?)
            }

            fn equality(_: &Self::Value, _: &Self::Value) -> bool {
                false
            }
        }

        Ok(dice.compute(self).await??)
    }

    fn run_analysis<'a>(
        &'a self,
        dice: &'a DiceComputations,
    ) -> impl Future<Output = anyhow::Result<AnalysisResult>> + Send + 'a {
        let fut = async move { self.run_analysis_impl(dice).await };
        unsafe { UnsafeSendFuture::new_encapsulates_starlark(fut) }
    }

    fn deps(&self) -> anyhow::Result<Vec<ConfiguredTargetLabel>> {
        struct Traversal(Vec<ConfiguredTargetLabel>);

        impl ConfiguredAttrTraversal for Traversal {
            fn dep(&mut self, dep: &ConfiguredProvidersLabel) -> anyhow::Result<()> {
                self.0.push(dep.target().dupe());
                Ok(())
            }

            fn query_macro(
                &mut self,
                _query: &str,
                _resolved_literals: &ResolvedQueryLiterals<ConfiguredProvidersLabel>,
            ) -> anyhow::Result<()> {
                Err(AnonTargetsError::QueryMacroNotSupported.into())
            }
        }

        let mut traversal = Traversal(Vec::new());
        for x in self.0.attrs().values() {
            x.traverse(self.0.name().pkg(), &mut traversal)?;
        }
        Ok(traversal.0)
    }
    async fn run_analysis_impl(&self, dice: &DiceComputations) -> anyhow::Result<AnalysisResult> {
        let deps = self.deps()?;
        let dep_analysis_results: HashMap<_, _> = keep_going::try_join_all(
            dice,
            deps.iter()
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

        let exec_resolution = ExecutionPlatformResolution::new(
            Some(
                find_execution_platform_by_configuration(
                    dice,
                    self.0.exec_cfg().cfg(),
                    self.0.exec_cfg().cfg(),
                )
                .await?,
            ),
            Vec::new(),
        );

        let rule_impl = get_rule_impl(dice, self.0.rule_type()).await?;
        let env = Module::new();
        let print = EventDispatcherPrintHandler(get_dispatcher());

        span_async(
            buck2_data::AnalysisStart {
                target: Some(self.0.as_proto().into()),
                rule: self.0.rule_type().to_string(),
            },
            async move {
                let (mut eval, ctx, list_res) = with_starlark_eval_provider(
                    dice,
                    &mut StarlarkProfilerOrInstrumentation::disabled(),
                    format!("anon_analysis:{}", self),
                    |provider| {
                        let mut eval = provider.make(&env)?;
                        eval.set_print_handler(&print);

                        // No attributes are allowed to contain macros or other stuff, so an empty resolution context works
                        let resolution_ctx = RuleAnalysisAttrResolutionContext {
                            module: &env,
                            dep_analysis_results,
                            query_results: HashMap::new(),
                        };

                        let mut resolved_attrs = Vec::with_capacity(self.0.attrs().len());
                        for (name, attr) in self.0.attrs().iter() {
                            resolved_attrs.push((
                                name,
                                attr.resolve_single(self.0.name().pkg(), &resolution_ctx)?,
                            ));
                        }
                        let attributes = env.heap().alloc(AllocStruct(resolved_attrs));

                        let registry = AnalysisRegistry::new_from_owner(
                            BaseDeferredKey::AnonTarget(self.0.dupe()),
                            exec_resolution,
                        )?;

                        let ctx = env.heap().alloc_typed(AnalysisContext::new(
                            eval.heap(),
                            attributes,
                            Some(eval.heap().alloc_typed(Label::new(
                                ConfiguredProvidersLabel::new(
                                    self.0.configured_label(),
                                    ProvidersName::Default,
                                ),
                            ))),
                            registry,
                            dice.global_data().get_digest_config(),
                        ));

                        let list_res = rule_impl.invoke(&mut eval, ctx)?;
                        Ok((eval, ctx, list_res))
                    },
                )
                .await?;

                ctx.actions
                    .run_promises(dice, &mut eval, format!("anon_analysis$promises:{}", self))
                    .await?;
                let res_typed = ProviderCollection::try_from_value(list_res)?;
                let res = env.heap().alloc(res_typed);
                env.set("", res);

                // Pull the ctx object back out, and steal ctx.action's state back
                let analysis_registry = ctx.take_state();
                std::mem::drop(eval);

                let (frozen_env, deferreds) = analysis_registry.finalize(&env)?(env)?;

                let res = frozen_env.get("").unwrap();
                let provider_collection = FrozenProviderCollectionValue::try_from_value(res)
                    .expect("just created this, this shouldn't happen");

                // this could look nicer if we had the entire analysis be a deferred
                let deferred = DeferredTable::new(deferreds.take_result()?);
                Ok(AnalysisResult::new(provider_collection, deferred, None))
            }
            .map(|res| {
                (
                    res,
                    buck2_data::AnalysisEnd {
                        target: Some(self.0.as_proto().into()),
                        rule: self.0.rule_type().to_string(),
                        profile: None, // Not implemented for anon targets
                    },
                )
            }),
        )
        .await
    }
}

/// Several attribute functions need a context, make one that is mostly useless.
struct AnonAttrCtx {
    cfg: ConfigurationData,
    transitions: OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>>,
}

impl AnonAttrCtx {
    fn new() -> Self {
        Self {
            cfg: ConfigurationData::unspecified(),
            transitions: OrderedMap::new(),
        }
    }
}

impl AttrCoercionContext for AnonAttrCtx {
    fn coerce_label(&self, value: &str) -> anyhow::Result<ProvidersLabel> {
        Err(AnonTargetsError::CantParseDuringCoerce(value.to_owned()).into())
    }

    fn intern_str(&self, value: &str) -> ArcStr {
        // TODO(scottcao): do intern.
        ArcStr::from(value)
    }

    fn intern_list(&self, value: Vec<CoercedAttr>) -> ArcSlice<CoercedAttr> {
        // TODO(scottcao): do intern.
        value.into()
    }

    fn intern_dict(
        &self,
        value: Vec<(CoercedAttr, CoercedAttr)>,
    ) -> ArcSlice<(CoercedAttr, CoercedAttr)> {
        // TODO(scottcao): do intern.
        value.into()
    }

    fn intern_select(
        &self,
        value: Vec<(TargetLabel, CoercedAttr)>,
    ) -> ArcSlice<(TargetLabel, CoercedAttr)> {
        // TODO(scottcao): do intern.
        value.into()
    }

    fn coerce_path(&self, value: &str, _allow_directory: bool) -> anyhow::Result<CoercedPath> {
        Err(AnonTargetsError::CantParseDuringCoerce(value.to_owned()).into())
    }

    fn coerce_target_pattern(
        &self,
        pattern: &str,
    ) -> anyhow::Result<ParsedPattern<TargetPatternExtra>> {
        Err(AnonTargetsError::CantParseDuringCoerce(pattern.to_owned()).into())
    }

    fn visit_query_function_literals(
        &self,
        _visitor: &mut dyn buck2_query::query::syntax::simple::functions::QueryLiteralVisitor,
        _expr: &buck2_query_parser::spanned::Spanned<buck2_query_parser::Expr>,
        query: &str,
    ) -> anyhow::Result<()> {
        Err(AnonTargetsError::CantParseDuringCoerce(query.to_owned()).into())
    }
}

impl AttrConfigurationContext for AnonAttrCtx {
    fn matches<'a>(&'a self, _label: &TargetLabel) -> Option<&'a ConfigSettingData> {
        None
    }

    fn cfg(&self) -> ConfigurationNoExec {
        ConfigurationNoExec::new(self.cfg.dupe())
    }

    fn exec_cfg(&self) -> ConfigurationNoExec {
        ConfigurationNoExec::new(self.cfg.dupe())
    }

    fn toolchain_cfg(&self) -> ConfigurationWithExec {
        ConfigurationWithExec::new(self.cfg.dupe(), self.cfg.dupe())
    }

    fn platform_cfg(&self, _label: &TargetLabel) -> anyhow::Result<ConfigurationData> {
        Ok(self.cfg.dupe())
    }

    fn resolved_transitions(&self) -> &OrderedMap<Arc<TransitionId>, Arc<TransitionApplied>> {
        &self.transitions
    }
}

pub(crate) fn init_eval_anon_target() {
    EVAL_ANON_TARGET
        .init(|ctx, key| Box::pin(async move { AnonTargetKey::downcast(key)?.resolve(ctx).await }));
}

pub(crate) fn init_anon_target_registry_new() {
    ANON_TARGET_REGISTRY_NEW.init(|_phantom, execution_platform| {
        Box::new(AnonTargetsRegistry {
            execution_platform,
            promises: AnonPromises::default(),
        })
    });
}

impl<'v> AnonTargetsRegistry<'v> {
    pub(crate) fn downcast_mut(
        registry: &mut dyn AnonTargetsRegistryDyn<'v>,
    ) -> anyhow::Result<&'v mut AnonTargetsRegistry<'v>> {
        let registry: &mut AnonTargetsRegistry = registry
            .as_any_mut()
            .downcast_mut::<AnonTargetsRegistry>()
            .context("AnonTargetsRegistryDyn is not an AnonTargetsRegistry (internal error)")?;
        unsafe {
            // It is hard or impossible to express this safely with the borrow checker.
            // Has something to do with 'v being invariant.
            Ok(mem::transmute::<
                &mut AnonTargetsRegistry,
                &mut AnonTargetsRegistry,
            >(registry))
        }
    }

    pub(crate) fn register_one(
        &mut self,
        promise: ValueTyped<'v, StarlarkPromise<'v>>,
        rule: ValueTyped<'v, FrozenRuleCallable>,
        attributes: DictOf<'v, &'v str, Value<'v>>,
    ) -> anyhow::Result<()> {
        self.promises.push_one(
            promise,
            AnonTargetKey::new(&self.execution_platform, rule, attributes)?,
        );
        Ok(())
    }

    pub(crate) fn register_many(
        &mut self,
        promise: ValueTyped<'v, StarlarkPromise<'v>>,
        rules: Vec<(
            ValueTyped<'v, FrozenRuleCallable>,
            DictOf<'v, &'v str, Value<'v>>,
        )>,
    ) -> anyhow::Result<()> {
        let keys = rules.into_try_map(|(rule, attributes)| {
            AnonTargetKey::new(&self.execution_platform, rule, attributes)
        })?;
        self.promises.push_list(promise, keys);
        Ok(())
    }
}

impl<'v> AnonTargetsRegistryDyn<'v> for AnonTargetsRegistry<'v> {
    fn as_any_mut(&mut self) -> &mut dyn AnyLifetime<'v> {
        self
    }

    fn take_promises(&mut self) -> Option<Box<dyn AnonPromisesDyn<'v>>> {
        // We swap it out, so we can still collect new promises
        Some(mem::take(&mut self.promises))
            .filter(|p| !p.is_empty())
            .map(|p| Box::new(p) as Box<dyn AnonPromisesDyn>)
    }

    /*
    pub(crate) fn get_promises(&mut self) -> Option<AnonTargetsRegistry<'v>> {
        if self.entries.is_empty() {
            None
        } else {
            // We swap it out, so we can still collect new promises
            let mut new = AnonTargetsRegistry::new(self.execution_platform.dupe());
            mem::swap(&mut new, self);
            Some(new)
        }
    }
    */

    fn assert_no_promises(&self) -> anyhow::Result<()> {
        if self.promises.is_empty() {
            Ok(())
        } else {
            Err(AnonTargetsError::AssertNoPromisesFailed.into())
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn anon_target_name() {
        assert_eq!(
            AnonTargetKey::parse_target_label("//foo:bar")
                .unwrap()
                .to_string(),
            "anon//foo:bar"
        );
        assert_eq!(
            AnonTargetKey::parse_target_label("cell//foo/bar:baz")
                .unwrap()
                .to_string(),
            "cell//foo/bar:baz"
        );
        assert!(AnonTargetKey::parse_target_label("foo").is_err());
        assert!(AnonTargetKey::parse_target_label("//foo:").is_err());
    }
}
