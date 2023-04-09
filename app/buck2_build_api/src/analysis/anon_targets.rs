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
use buck2_common::result::SharedResult;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::collections::ordered_map::OrderedMap;
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
use buck2_data::ToProtoMessage;
use buck2_events::dispatch::get_dispatcher;
use buck2_events::dispatch::span_async;
use buck2_execute::digest_config::HasDigestConfig;
use buck2_interpreter::starlark_promise::StarlarkPromise;
use buck2_interpreter::types::label::Label;
use buck2_interpreter_for_build::attrs::coerce::attr_type::AttrTypeInnerExt;
use buck2_interpreter_for_build::interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_interpreter_for_build::rule::FrozenRuleCallable;
use buck2_node::attrs::attr_type::attr_literal::AttrLiteral;
use buck2_node::attrs::attr_type::dep::DepAttr;
use buck2_node::attrs::attr_type::dep::DepAttrTransition;
use buck2_node::attrs::attr_type::dep::DepAttrType;
use buck2_node::attrs::attr_type::AttrType;
use buck2_node::attrs::attr_type::AttrTypeInner;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::coerced_path::CoercedPath;
use buck2_node::attrs::coercion_context::AttrCoercionContext;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::attrs::configuration_context::AttrConfigurationContext;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::attrs::configured_traversal::ConfiguredAttrTraversal;
use buck2_node::attrs::internal::internal_attrs;
use buck2_node::configuration::execution::ExecutionPlatformResolution;
use buck2_util::arc_str::ArcSlice;
use buck2_util::arc_str::ArcStr;
use derive_more::Display;
use dice::DiceComputations;
use dice::Key;
use dupe::Dupe;
use either::Either;
use futures::future;
use futures::stream::FuturesUnordered;
use futures::Future;
use futures::FutureExt;
use gazebo::prelude::*;
use ref_cast::RefCast;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::values::dict::DictOf;
use starlark::values::list::AllocList;
use starlark::values::list::ListRef;
use starlark::values::structs::AllocStruct;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueTyped;
use thiserror::Error;

use crate::analysis::anon_target_node::AnonTarget;
use crate::analysis::calculation::get_rule_impl;
use crate::analysis::calculation::RuleAnalysisCalculation;
use crate::analysis::registry::AnalysisRegistry;
use crate::analysis::AnalysisResult;
use crate::analysis::RuleAnalysisAttrResolutionContext;
use crate::analysis::RuleImplFunction;
use crate::attrs::resolve::configured_attr::ConfiguredAttrExt;
use crate::deferred::base_deferred_key::BaseDeferredKey;
use crate::deferred::types::DeferredTable;
use crate::interpreter::rule_defs::context::AnalysisContext;
use crate::interpreter::rule_defs::provider::collection::FrozenProviderCollectionValue;
use crate::interpreter::rule_defs::provider::collection::ProviderCollection;
use crate::interpreter::rule_defs::provider::dependency::Dependency;
use crate::keep_going;
use crate::nodes::calculation::find_execution_platform_by_configuration;

#[derive(Debug, Trace, Allocative)]
pub struct AnonTargetsRegistry<'v> {
    // We inherit the execution platform of our parent
    execution_platform: ExecutionPlatformResolution,
    // The actual data
    entries: Vec<(
        ValueTyped<'v, StarlarkPromise<'v>>,
        // Either a single entry, or a list that becomes a list of providers
        Either<AnonTargetKey, Vec<AnonTargetKey>>,
    )>,
}

#[derive(Debug, Error)]
enum AnonTargetsError {
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
    #[error("Invalid `attr.dep` value, expected `dependency`, got `{0}`")]
    InvalidDep(String),
    #[error("Invalid `attr.list` value, expected `list`, got `{0}`")]
    InvalidList(String),
}

#[repr(transparent)]
#[derive(
    Hash, Eq, PartialEq, Clone, Dupe, Debug, Display, Trace, Allocative, RefCast
)]
struct AnonTargetKey(Arc<AnonTarget>);

impl AnonTargetKey {
    fn new<'v>(
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
                    Self::coerce_attr(attr.coercer(), v)
                        .with_context(|| format!("Error coercing attribute `{}`", k))?,
                );
            }
        }
        for (k, _, a) in attrs_spec.attr_specs() {
            if !attrs.contains_key(k) && !internal_attrs.contains_key(k) {
                if let Some(x) = a.default() {
                    attrs.insert(k.to_owned(), Self::configure_attr(x)?);
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

    fn coerce_attr(attr: &AttrType, x: Value) -> anyhow::Result<ConfiguredAttr> {
        fn unpack_dep(x: &AttrTypeInner) -> Option<DepAttrType> {
            match x {
                AttrTypeInner::Dep(d) => Some(d.dupe()),
                AttrTypeInner::ConfiguredDep(d) => Some(DepAttrType {
                    required_providers: d.required_providers.dupe(),
                    transition: DepAttrTransition::Identity,
                }),
                _ => None,
            }
        }

        let ctx = AnonAttrCtx::new();
        let a = if let Some(attr_type) = unpack_dep(&attr.0) {
            match Dependency::from_value(x) {
                Some(dep) => {
                    let label = dep.label().inner().clone();
                    AttrLiteral::ConfiguredDep(Box::new(DepAttr { attr_type, label }))
                }
                _ => return Err(AnonTargetsError::InvalidDep(x.get_type().to_owned()).into()),
            }
        } else if let AttrTypeInner::List(inner) = &*attr.0 {
            match ListRef::from_value(x) {
                // We don't do anything special for list, but we want to make sure that dependencies inside lists are looked up properly
                Some(list) => {
                    return Ok(ConfiguredAttr(AttrLiteral::List(
                        list.content()
                            .try_map(|v| Self::coerce_attr(&inner.inner, *v))?
                            .into(),
                    )));
                }
                None => return Err(AnonTargetsError::InvalidList(x.to_string()).into()),
            }
        } else {
            attr.0.coerce_item(AttrIsConfigurable::No, &ctx, x)?
        };
        a.configure(&ctx)
    }

    fn configure_attr(x: &CoercedAttr) -> anyhow::Result<ConfiguredAttr> {
        x.configure(&AnonAttrCtx::new())
    }

    async fn resolve(&self, dice: &DiceComputations) -> anyhow::Result<AnalysisResult> {
        #[async_trait]
        impl Key for AnonTargetKey {
            type Value = SharedResult<AnalysisResult>;

            async fn compute(&self, ctx: &DiceComputations) -> Self::Value {
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

    fn deps(&self) -> anyhow::Result<Vec<&ConfiguredTargetLabel>> {
        struct Traversal<'a>(Vec<&'a ConfiguredTargetLabel>);

        impl<'a> ConfiguredAttrTraversal<'a> for Traversal<'a> {
            fn dep(&mut self, dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
                self.0.push(dep.target());
                Ok(())
            }
        }

        let mut traversal = Traversal(Vec::new());
        for x in self.0.attrs().values() {
            x.traverse(self.0.name().pkg(), &mut traversal)?;
        }
        Ok(traversal.0)
    }

    async fn run_analysis_impl(&self, dice: &DiceComputations) -> anyhow::Result<AnalysisResult> {
        let dep_analysis_results: HashMap<_, _> = keep_going::try_join_all(
            self.deps()?
                .into_iter()
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
                let analysis_registry = {
                    let mut eval = Evaluator::new(&env);
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
                    );

                    let ctx = env.heap().alloc_typed(AnalysisContext::new(
                        eval.heap(),
                        attributes,
                        Some(
                            eval.heap()
                                .alloc_typed(Label::new(ConfiguredProvidersLabel::new(
                                    self.0.configured_label(),
                                    ProvidersName::Default,
                                ))),
                        ),
                        registry,
                        dice.global_data().get_digest_config(),
                    ));

                    let list_res = rule_impl.invoke(&mut eval, ctx)?;
                    ctx.run_promises(dice, &mut eval).await?;
                    let res_typed = ProviderCollection::try_from_value(list_res)?;
                    let res = env.heap().alloc(res_typed);
                    env.set("", res);

                    // Pull the ctx object back out, and steal ctx.action's state back
                    ctx.take_state()
                };
                let (frozen_env, deferreds) = analysis_registry.finalize(&env)(env)?;

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

pub(crate) async fn eval_anon_target(
    dice: &DiceComputations,
    target: &Arc<AnonTarget>,
) -> anyhow::Result<AnalysisResult> {
    AnonTargetKey::ref_cast(target).resolve(dice).await
}

impl<'v> AnonTargetsRegistry<'v> {
    pub(crate) fn new(execution_platform: ExecutionPlatformResolution) -> Self {
        Self {
            execution_platform,
            entries: Vec::new(),
        }
    }

    pub(crate) fn register_one(
        &mut self,
        promise: ValueTyped<'v, StarlarkPromise<'v>>,
        rule: ValueTyped<'v, FrozenRuleCallable>,
        attributes: DictOf<'v, &'v str, Value<'v>>,
    ) -> anyhow::Result<()> {
        self.entries.push((
            promise,
            Either::Left(AnonTargetKey::new(
                &self.execution_platform,
                rule,
                attributes,
            )?),
        ));
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
        self.entries.push((promise, Either::Right(keys)));
        Ok(())
    }

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

    pub async fn run_promises(
        self,
        dice: &DiceComputations,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<()> {
        // Resolve all the targets in parallel
        // We have vectors of vectors, so we create a "shape" which has the same shape but with indices
        let mut shape = Vec::new();
        let mut targets = Vec::new();
        for (promise, xs) in self.entries {
            match xs {
                Either::Left(x) => {
                    shape.push((promise, Either::Left(shape.len())));
                    targets.push(x);
                }
                Either::Right(xs) => {
                    shape.push((promise, Either::Right(shape.len()..shape.len() + xs.len())));
                    targets.extend(xs);
                }
            }
        }

        let values =
            future::try_join_all(targets.iter().map(|target| target.resolve(dice))).await?;
        // But must bind the promises sequentially
        for (promise, xs) in shape {
            match xs {
                Either::Left(i) => {
                    let val = values[i]
                        .provider_collection
                        .value()
                        .owned_value(eval.frozen_heap());
                    promise.resolve(val, eval)?
                }
                Either::Right(is) => {
                    let xs: Vec<_> = is
                        .map(|i| {
                            values[i]
                                .provider_collection
                                .value()
                                .owned_value(eval.frozen_heap())
                        })
                        .collect();
                    let list = eval.heap().alloc(AllocList(xs));
                    promise.resolve(list, eval)?
                }
            }
        }
        Ok(())
    }

    pub(crate) fn assert_no_promises(&self) -> anyhow::Result<()> {
        if self.entries.is_empty() {
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
