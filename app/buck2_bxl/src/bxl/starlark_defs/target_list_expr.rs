/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::iter;

use allocative::Allocative;
use buck2_build_api::configure_targets::get_maybe_compatible_targets;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::configuration::compatibility::IncompatiblePlatformReason;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::soft_error;
use buck2_core::target::label::label::TargetLabel;
use buck2_error::BuckErrorContext;
use buck2_interpreter::types::target_label::StarlarkConfiguredTargetLabel;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use buck2_node::load_patterns::load_patterns;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::nodes::frontend::TargetGraphCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_util::truncate::truncate;
use dice::DiceComputations;
use dupe::Dupe;
use dupe::IterDupedExt;
use either::Either;
use futures::FutureExt;
use starlark::collections::SmallSet;
use starlark::values::list::UnpackList;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::Heap;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueOf;

use crate::bxl::starlark_defs::context::BxlContextCoreData;
use crate::bxl::starlark_defs::context::BxlContextNoDice;
use crate::bxl::starlark_defs::context::ErrorPrinter;
use crate::bxl::starlark_defs::eval_extra::BxlEvalExtra;
use crate::bxl::starlark_defs::nodes::configured::StarlarkConfiguredTargetNode;
use crate::bxl::starlark_defs::nodes::unconfigured::StarlarkTargetNode;
use crate::bxl::starlark_defs::target_expr::TargetExpr;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;

/// TargetExpr is just a simple type that can be used in starlark_module
/// functions for arguments that should be target sets. It will accept a
/// literal (like `//some:target`) or list of literals or a TargetSet Value (from one of the
/// BXL functions that return them).
pub(crate) enum TargetListExpr<'v, Node: QueryTarget> {
    One(TargetExpr<'v, Node>),
    Iterable(Vec<TargetExpr<'v, Node>>),
    TargetSet(Cow<'v, TargetSet<Node>>),
}

// Filters out incompatible targets and emits the error message
pub(crate) fn filter_incompatible<T: ErrorPrinter>(
    targets: impl IntoIterator<Item = MaybeCompatible<ConfiguredTargetNode>>,
    error_printer: &T,
) -> buck2_error::Result<TargetSet<ConfiguredTargetNode>> {
    let mut target_set = TargetSet::new();
    let mut incompatible_targets = SmallSet::new();

    for res in targets {
        match res {
            MaybeCompatible::Incompatible(reason) => {
                incompatible_targets.insert(reason.target.dupe());
            }
            MaybeCompatible::Compatible(target) => {
                target_set.insert(target);
            }
        }
    }

    if !incompatible_targets.is_empty() {
        error_printer.print_to_error_stream(
            IncompatiblePlatformReason::skipping_message_for_multiple(incompatible_targets.iter()),
        )?;
    }

    Ok(target_set)
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum TargetNodeOrTargetLabel<'v> {
    TargetNode(&'v StarlarkTargetNode),
    TargetLabel(&'v StarlarkTargetLabel),
}

impl<'v> TargetNodeOrTargetLabel<'v> {
    fn label(&self) -> &'v TargetLabel {
        match self {
            TargetNodeOrTargetLabel::TargetNode(node) => node.0.label(),
            TargetNodeOrTargetLabel::TargetLabel(label) => label.label(),
        }
    }
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum TargetNodeOrTargetLabelOrStr<'v> {
    TargetNode(&'v StarlarkTargetNode),
    TargetLabel(&'v StarlarkTargetLabel),
    Str(&'v str),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum ConfiguredTargetNodeArg<'v> {
    ConfiguredTargetNode(&'v StarlarkConfiguredTargetNode),
    ConfiguredTargetLabel(&'v StarlarkConfiguredTargetLabel),
    Str(&'v str),
    Unconfigured(TargetNodeOrTargetLabel<'v>),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum TargetSetOrTargetList<'v> {
    TargetSet(&'v StarlarkTargetSet<TargetNode>),
    TargetList(UnpackList<ValueOf<'v, TargetNodeOrTargetLabelOrStr<'v>>>),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum ConfiguredTargetListArg<'v> {
    ConfiguredTargetSet(&'v StarlarkTargetSet<ConfiguredTargetNode>),
    TargetSet(&'v StarlarkTargetSet<TargetNode>),
    TargetList(UnpackList<ConfiguredTargetNodeArg<'v>>),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum TargetListExprArg<'v> {
    Target(TargetNodeOrTargetLabelOrStr<'v>),
    List(TargetSetOrTargetList<'v>),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum ConfiguredTargetListExprArg<'v> {
    Target(ConfiguredTargetNodeArg<'v>),
    List(ValueOf<'v, ConfiguredTargetListArg<'v>>),
}

impl<'v> TargetListExpr<'v, TargetNode> {
    pub(crate) fn iter(&self) -> Box<dyn Iterator<Item = TargetExpr<'v, TargetNode>> + '_> {
        match &self {
            Self::One(one) => Box::new(iter::once(one.clone())),
            Self::Iterable(iterable) => Box::new(iterable.iter().cloned()),
            Self::TargetSet(target_set) => {
                Box::new(target_set.iter().map(|s| TargetExpr::Node(s.clone())))
            }
        }
    }

    /// Get a `TargetSet<TargetNode>` from the `TargetExpr`
    pub(crate) async fn get(
        self,
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<Cow<'v, TargetSet<TargetNode>>> {
        let set = ctx
            .try_compute_join(self.iter(), |ctx, node_or_ref| {
                async move { node_or_ref.get_from_dice(ctx).await }.boxed()
            })
            .await?
            .into_iter()
            .collect();
        Ok(Cow::Owned(set))
    }

    /// Get a single `TargetNode`
    pub(crate) async fn get_one(
        &self,
        ctx: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<Option<TargetNode>> {
        Ok(match &self {
            Self::One(node_or_ref) => Some(node_or_ref.get_from_dice(ctx).await?),
            _ => None,
        })
    }
}

#[derive(Debug, buck2_error::Error)]
#[buck2(tag = Input)]
pub(crate) enum TargetExprError {
    #[error(
        "Expected a single target like item, but was `{0}`. If you have passed in a `label`, make sure to call `configured_target()` to get the underlying configured target label."
    )]
    NotATarget(String),
    #[error(
        "Unconfigured target with label `{0}` was passed into cquery. Targets passed into cquery should be configured (recommendation is to use `ctx.target_universe()`)."
    )]
    UnconfiguredTargetInCquery(String),
    #[error(
        "`keep_going` is currently only implemented for a single target pattern as a string literal."
    )]
    KeepGoingOnlyForStringLiteral,
}

impl<'v> TargetListExpr<'v, ConfiguredTargetNode> {
    fn iter(&self) -> Box<dyn Iterator<Item = TargetExpr<'v, ConfiguredTargetNode>> + '_> {
        match &self {
            Self::One(one) => Box::new(iter::once(one.clone())),
            Self::Iterable(iterable) => Box::new(iterable.iter().cloned()),
            Self::TargetSet(target_set) => {
                Box::new(target_set.iter().map(|s| TargetExpr::Node(s.clone())))
            }
        }
    }

    /// Get a vector of maybe compatible `ConfiguredTargetNode`s from the `TargetExpr`.
    /// Any callers of this function will need to call `filter_incompatible()` on the result
    /// in order to get the `TargetSet<ConfiguredTargetNode>`.
    pub(crate) async fn get(
        self,
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<Vec<MaybeCompatible<ConfiguredTargetNode>>> {
        dice.compute_join(self.iter(), |ctx, node_or_ref| {
            async move { ctx.get_configured_target_node(node_or_ref.node_ref()).await }.boxed()
        })
        .await
        .into_iter()
        .collect()
    }

    /// Get a single maybe compatible `ConfiguredTargetNode`.
    pub(crate) async fn get_one(
        &self,
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<Option<MaybeCompatible<ConfiguredTargetNode>>> {
        Ok(match &self {
            Self::One(node_or_ref) => Some(
                dice.get_configured_target_node(node_or_ref.node_ref())
                    .await?,
            ),
            _ => None,
        })
    }

    pub(crate) fn as_provider_labels(&self) -> Vec<ConfiguredProvidersLabel> {
        self.iter()
            .map(|e| ConfiguredProvidersLabel::default_for(e.node_ref().dupe()))
            .collect()
    }

    pub(crate) async fn unpack_opt<'c>(
        arg: ConfiguredTargetListExprArg<'v>,
        global_cfg_options: &GlobalCfgOptions,
        ctx: &BxlContextNoDice<'v>,
        dice: &mut DiceComputations<'_>,
        allow_unconfigured: bool,
    ) -> buck2_error::Result<TargetListExpr<'v, ConfiguredTargetNode>> {
        match arg {
            ConfiguredTargetListExprArg::Target(arg) => {
                Ok(
                    Self::unpack_literal(arg, global_cfg_options, ctx, dice, allow_unconfigured)
                        .await?,
                )
            }
            ConfiguredTargetListExprArg::List(arg) => {
                Ok(
                    Self::unpack_iterable(arg, global_cfg_options, ctx, dice, allow_unconfigured)
                        .await?,
                )
            }
        }
    }

    pub(crate) async fn unpack<'c>(
        // TODO(nga): this does not accept unconfigured targets, so should be narrower type here.
        arg: ConfiguredTargetListExprArg<'v>,
        global_cfg_options: &GlobalCfgOptions,
        ctx: &BxlContextNoDice<'v>,
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<TargetListExpr<'v, ConfiguredTargetNode>> {
        Self::unpack_opt(arg, global_cfg_options, ctx, dice, false).await
    }

    pub(crate) async fn unpack_allow_unconfigured<'c>(
        arg: ConfiguredTargetListExprArg<'v>,
        global_cfg_options: &GlobalCfgOptions,
        ctx: &BxlContextNoDice<'v>,
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<TargetListExpr<'v, ConfiguredTargetNode>> {
        Self::unpack_opt(arg, global_cfg_options, ctx, dice, true).await
    }

    fn check_allow_unconfigured(
        allow_unconfigured: bool,
        unconfigured_label: &str,
        global_cfg_options: &GlobalCfgOptions,
    ) -> buck2_error::Result<()> {
        if !allow_unconfigured {
            if global_cfg_options.target_platform.is_none() {
                soft_error!(
                    "bxl_unconfigured_target_in_cquery",
                    TargetExprError::UnconfiguredTargetInCquery(unconfigured_label.to_owned())
                        .into()
                )?;
            }
        }
        Ok(())
    }

    async fn unpack_literal(
        arg: ConfiguredTargetNodeArg<'v>,
        global_cfg_options: &GlobalCfgOptions,
        ctx: &BxlContextNoDice<'_>,
        dice: &mut DiceComputations<'_>,
        allow_unconfigured: bool,
    ) -> buck2_error::Result<TargetListExpr<'v, ConfiguredTargetNode>> {
        match arg {
            ConfiguredTargetNodeArg::ConfiguredTargetNode(configured_target) => {
                Ok(Self::One(TargetExpr::Node(configured_target.0.dupe())))
            }
            ConfiguredTargetNodeArg::ConfiguredTargetLabel(configured_target) => Ok(
                TargetListExpr::One(TargetExpr::Label(Cow::Borrowed(configured_target.label()))),
            ),
            ConfiguredTargetNodeArg::Str(s) => {
                Self::check_allow_unconfigured(allow_unconfigured, s, global_cfg_options)?;

                Self::unpack_string_literal(s, global_cfg_options, ctx, dice, false).await
            }
            ConfiguredTargetNodeArg::Unconfigured(label) => {
                Self::check_allow_unconfigured(
                    allow_unconfigured,
                    &label.label().to_string(),
                    global_cfg_options,
                )?;
                Ok(TargetListExpr::One(TargetExpr::Label(Cow::Owned(
                    dice.get_configured_target(label.label(), global_cfg_options)
                        .await?,
                ))))
            }
        }
    }

    // Ideally we refactor the entire unpacking logic for configured targets to make this easier,
    // but let's support keep_going for string literals for now.
    pub(crate) async fn unpack_keep_going<'c>(
        arg: ConfiguredTargetListExprArg<'v>,
        global_cfg_options: &GlobalCfgOptions,
        ctx: &BxlContextNoDice<'v>,
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<TargetListExpr<'v, ConfiguredTargetNode>> {
        match arg {
            ConfiguredTargetListExprArg::Target(ConfiguredTargetNodeArg::Str(val)) => {
                Self::unpack_string_literal(val, global_cfg_options, ctx, dice, true).await
            }
            _ => Err(TargetExprError::KeepGoingOnlyForStringLiteral.into()),
        }
    }

    // Unpack functionality for a string literal, with keep_going support
    async fn unpack_string_literal(
        val: &str,
        global_cfg_options: &GlobalCfgOptions,
        ctx: &BxlContextNoDice<'_>,
        dice: &mut DiceComputations<'_>,
        keep_going: bool,
    ) -> buck2_error::Result<TargetListExpr<'v, ConfiguredTargetNode>> {
        match ParsedPattern::<TargetPatternExtra>::parse_relaxed(
            ctx.target_alias_resolver(),
            // TODO(nga): Parse relaxed relative to cell root is incorrect.
            CellPathRef::new(ctx.cell_name(), CellRelativePath::empty()),
            val,
            ctx.cell_resolver(),
            ctx.cell_alias_resolver(),
        )? {
            ParsedPattern::Target(pkg, name, TargetPatternExtra) => {
                let result = match dice
                    .get_configured_target(
                        &TargetLabel::new(pkg, name.as_ref()),
                        global_cfg_options,
                    )
                    .await
                {
                    Ok(label) => {
                        // check if we can get a maybe compatible configured target node successfully here to make
                        // sure keep_going works. We will try to get the node later, but due to how complex this
                        // code is, it's much easier to just call it once here as a sanity check.
                        match dice.get_configured_target_node(&label).await {
                            Ok(_) => Ok(TargetListExpr::One(TargetExpr::Label(Cow::Owned(label)))),
                            Err(e) => Err(e),
                        }
                    }
                    Err(e) => Err(e),
                };

                result.or_else(|e| {
                    if keep_going {
                        Ok(TargetListExpr::Iterable(Vec::new()))
                    } else {
                        Err(e)
                    }
                })
            }
            pattern => {
                let loaded_patterns =
                    load_patterns(dice, vec![pattern], MissingTargetBehavior::Fail).await?;

                let maybe_compatible = get_maybe_compatible_targets(
                    dice,
                    loaded_patterns.iter_loaded_targets_by_package(),
                    global_cfg_options,
                    keep_going,
                )
                .await?;

                let maybe_compatible: Vec<_> = if keep_going {
                    maybe_compatible.filter_map(|r| r.ok()).collect()
                } else {
                    maybe_compatible.collect::<buck2_error::Result<_>>()?
                };

                let result = filter_incompatible(maybe_compatible, ctx)?;
                Ok(TargetListExpr::TargetSet(Cow::Owned(result)))
            }
        }
    }

    async fn unpack_iterable<'c>(
        value: ValueOf<'v, ConfiguredTargetListArg<'v>>,
        global_cfg_options: &GlobalCfgOptions,
        ctx: &BxlContextNoDice<'_>,
        dice: &mut DiceComputations<'_>,
        allow_unconfigured: bool,
    ) -> buck2_error::Result<TargetListExpr<'v, ConfiguredTargetNode>> {
        match value.typed {
            ConfiguredTargetListArg::ConfiguredTargetSet(s) => {
                Ok(Self::TargetSet(Cow::Borrowed(s)))
            }
            ConfiguredTargetListArg::TargetSet(s) => Ok(TargetListExpr::Iterable(
                dice.try_compute_join(s.0.iter(), |dice, node| {
                    async move {
                        Self::check_allow_unconfigured(
                            allow_unconfigured,
                            &node.label().to_string(),
                            global_cfg_options,
                        )?;

                        buck2_error::Ok(TargetExpr::Label(Cow::Owned(
                            dice.get_configured_target(node.label(), global_cfg_options)
                                .await?,
                        )))
                    }
                    .boxed()
                })
                .await?,
            )),
            ConfiguredTargetListArg::TargetList(unpack) => {
                let mut resolved = vec![];

                // FIXME(JakobDegen): These iterations don't run in parallel, which is pretty sad.
                // Unfortunately, that's also not easy to fix because for some reason this code
                // prints to console
                for item in unpack.items {
                    let unpacked = Self::unpack_literal(
                        item,
                        global_cfg_options,
                        ctx,
                        dice,
                        allow_unconfigured,
                    )
                    .await?;

                    match unpacked {
                        TargetListExpr::One(node) => resolved.push(node),
                        TargetListExpr::TargetSet(set) => match set {
                            Cow::Borrowed(s) => itertools::Either::Left(s.iter().duped()),
                            Cow::Owned(s) => itertools::Either::Right(s.into_iter()),
                        }
                        .for_each(|t| resolved.push(TargetExpr::Node(t))),
                        _ => {
                            let error: buck2_error::Error =
                                TargetExprError::NotATarget(value.value.to_repr()).into();
                            return Err(error.context(format!(
                                "Error resolving list `{}`",
                                truncate(&value.value.to_repr(), 150)
                            )));
                        }
                    }
                }

                Ok(Self::Iterable(resolved))
            }
        }
    }
}

impl<'v> TargetListExpr<'v, TargetNode> {
    pub(crate) async fn unpack<'c>(
        value: TargetListExprArg<'v>,
        ctx: &BxlContextNoDice<'_>,
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<TargetListExpr<'v, TargetNode>> {
        match value {
            TargetListExprArg::Target(x) => Self::unpack_literal(x, ctx, dice).await,
            TargetListExprArg::List(x) => Self::unpack_iterable(x, ctx, dice).await,
        }
    }

    async fn unpack_literal(
        value: TargetNodeOrTargetLabelOrStr<'v>,
        ctx: &BxlContextNoDice<'_>,
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<TargetListExpr<'v, TargetNode>> {
        match value {
            TargetNodeOrTargetLabelOrStr::TargetNode(target) => {
                Ok(TargetListExpr::One(TargetExpr::Node(target.0.dupe())))
            }
            TargetNodeOrTargetLabelOrStr::TargetLabel(target) => Ok(TargetListExpr::One(
                TargetExpr::Label(Cow::Borrowed(target.label())),
            )),
            TargetNodeOrTargetLabelOrStr::Str(s) => {
                match ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                    ctx.target_alias_resolver(),
                    // TODO(nga): Parse relaxed relative to cell root is incorrect.
                    CellPathRef::new(ctx.cell_name(), CellRelativePath::empty()),
                    s,
                    ctx.cell_resolver(),
                    ctx.cell_alias_resolver(),
                )? {
                    ParsedPattern::Target(pkg, name, TargetPatternExtra) => {
                        Ok(TargetListExpr::One(TargetExpr::Label(Cow::Owned(
                            TargetLabel::new(pkg, name.as_ref()),
                        ))))
                    }
                    pattern => {
                        let loaded_patterns =
                            load_patterns(dice, vec![pattern], MissingTargetBehavior::Fail).await?;
                        let mut target_set = TargetSet::new();
                        for (_package, results) in loaded_patterns.into_iter() {
                            target_set.extend(results?.into_values());
                        }
                        Ok(Self::TargetSet(Cow::Owned(target_set)))
                    }
                }
            }
        }
    }

    async fn unpack_iterable<'c>(
        value: TargetSetOrTargetList<'v>,
        ctx: &BxlContextNoDice<'_>,
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<TargetListExpr<'v, TargetNode>> {
        match value {
            TargetSetOrTargetList::TargetSet(s) => Ok(Self::TargetSet(Cow::Borrowed(s))),
            TargetSetOrTargetList::TargetList(items) => {
                let mut resolved = vec![];

                for item in items.items {
                    let unpacked = Self::unpack_literal(item.typed, ctx, dice).await?;

                    match unpacked {
                        TargetListExpr::One(node) => resolved.push(node),
                        TargetListExpr::TargetSet(set) => match set {
                            Cow::Borrowed(s) => itertools::Either::Left(s.iter().duped()),
                            Cow::Owned(s) => itertools::Either::Right(s.into_iter()),
                        }
                        .for_each(|t| resolved.push(TargetExpr::Node(t))),
                        TargetListExpr::Iterable(_) => {
                            return Err(TargetExprError::NotATarget(item.value.to_repr()))
                                .buck_error_context("list in a list");
                        }
                    }
                }
                Ok(Self::Iterable(resolved))
            }
        }
    }
}

#[derive(Debug, Allocative)]
pub(crate) enum SingleOrCompatibleConfiguredTargets {
    Single(ConfiguredTargetNode),
    Compatibles(Vec<MaybeCompatible<ConfiguredTargetNode>>),
}

impl SingleOrCompatibleConfiguredTargets {
    pub(crate) fn into_value<'v>(
        self,
        heap: &'v Heap,
        bxl_eval_extra: &BxlEvalExtra,
    ) -> buck2_error::Result<Value<'v>> {
        match self {
            SingleOrCompatibleConfiguredTargets::Single(node) => {
                Ok(heap.alloc(StarlarkConfiguredTargetNode(node)))
            }
            SingleOrCompatibleConfiguredTargets::Compatibles(compatibles) => {
                let target_set = filter_incompatible(compatibles, bxl_eval_extra)?;
                Ok(heap.alloc(StarlarkTargetSet(target_set)))
            }
        }
    }
}

async fn unpack_string_literal<'v>(
    val: &str,
    global_cfg_options: &GlobalCfgOptions,
    ctx: &BxlContextCoreData,
    dice: &mut DiceComputations<'_>,
) -> buck2_error::Result<SingleOrCompatibleConfiguredTargets> {
    match ParsedPattern::<TargetPatternExtra>::parse_relaxed(
        ctx.target_alias_resolver(),
        // TODO(nga): Parse relaxed relative to cell root is incorrect.
        CellPathRef::new(ctx.cell_name(), CellRelativePath::empty()),
        val,
        ctx.cell_resolver(),
        ctx.cell_alias_resolver(),
    )? {
        ParsedPattern::Target(pkg, name, TargetPatternExtra) => {
            let label = dice
                .get_configured_target(&TargetLabel::new(pkg, name.as_ref()), global_cfg_options)
                .await?;
            let compatible_node = dice.get_configured_target_node(&label).await?;
            compatible_node
                .require_compatible()
                .map(SingleOrCompatibleConfiguredTargets::Single)
        }
        pattern => {
            let loaded_patterns =
                load_patterns(dice, vec![pattern], MissingTargetBehavior::Fail).await?;

            let maybe_compatible = get_maybe_compatible_targets(
                dice,
                loaded_patterns.iter_loaded_targets_by_package(),
                global_cfg_options,
                true,
            )
            .await?;

            let maybe_compatible = maybe_compatible.collect::<buck2_error::Result<_>>()?;
            Ok(SingleOrCompatibleConfiguredTargets::Compatibles(
                maybe_compatible,
            ))
        }
    }
}

#[derive(Debug, Clone, Allocative)]
pub(crate) enum OwnedTargetNodeOrTargetLabel {
    TargetNode(StarlarkTargetNode),
    TargetLabel(StarlarkTargetLabel),
}

impl OwnedTargetNodeOrTargetLabel {
    pub(crate) fn from_ref(reference: TargetNodeOrTargetLabel<'_>) -> Self {
        match reference {
            TargetNodeOrTargetLabel::TargetNode(node) => {
                OwnedTargetNodeOrTargetLabel::TargetNode(node.dupe())
            }
            TargetNodeOrTargetLabel::TargetLabel(label) => {
                OwnedTargetNodeOrTargetLabel::TargetLabel(label.dupe())
            }
        }
    }

    fn label(&self) -> &TargetLabel {
        match self {
            OwnedTargetNodeOrTargetLabel::TargetNode(node) => node.0.label(),
            OwnedTargetNodeOrTargetLabel::TargetLabel(label) => label.label(),
        }
    }

    pub(crate) async fn to_configured_target_node(
        &self,
        global_cfg_options: &GlobalCfgOptions,
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<ConfiguredTargetNode> {
        let configured_label = dice
            .get_configured_target(self.label(), global_cfg_options)
            .await?;
        dice.get_configured_target_node(&configured_label)
            .await?
            .require_compatible()
    }

    pub(crate) async fn to_unconfigured_target_node(
        &self,
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<TargetNode> {
        match self {
            OwnedTargetNodeOrTargetLabel::TargetNode(node) => Ok(node.0.dupe()),
            OwnedTargetNodeOrTargetLabel::TargetLabel(label) => Ok(dice
                .get_target_node(label.label())
                .await
                .map(|node| node.dupe())?),
        }
    }
}

#[derive(Debug, Clone, Allocative)]
pub(crate) enum OwnedConfiguredTargetNodeArg {
    ConfiguredTargetNode(StarlarkConfiguredTargetNode),
    ConfiguredTargetLabel(StarlarkConfiguredTargetLabel),
    String(String),
    Unconfigured(OwnedTargetNodeOrTargetLabel),
}

impl OwnedConfiguredTargetNodeArg {
    pub(crate) fn from_ref(reference: ConfiguredTargetNodeArg<'_>) -> Self {
        match reference {
            ConfiguredTargetNodeArg::ConfiguredTargetNode(node) => {
                OwnedConfiguredTargetNodeArg::ConfiguredTargetNode(node.dupe())
            }
            ConfiguredTargetNodeArg::ConfiguredTargetLabel(label) => {
                OwnedConfiguredTargetNodeArg::ConfiguredTargetLabel(label.dupe())
            }
            ConfiguredTargetNodeArg::Str(str) => {
                OwnedConfiguredTargetNodeArg::String(str.to_owned())
            }
            ConfiguredTargetNodeArg::Unconfigured(unconfigured) => {
                OwnedConfiguredTargetNodeArg::Unconfigured(OwnedTargetNodeOrTargetLabel::from_ref(
                    unconfigured,
                ))
            }
        }
    }

    pub(crate) async fn to_configured_target_node(
        &self,
        global_cfg_options: &GlobalCfgOptions,
        ctx: &BxlContextCoreData,
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<SingleOrCompatibleConfiguredTargets> {
        match self {
            OwnedConfiguredTargetNodeArg::ConfiguredTargetNode(node) => {
                Ok(SingleOrCompatibleConfiguredTargets::Single(node.0.dupe()))
            }
            OwnedConfiguredTargetNodeArg::ConfiguredTargetLabel(label) => {
                let compatible = dice.get_configured_target_node(label.label()).await?;
                compatible
                    .require_compatible()
                    .map(SingleOrCompatibleConfiguredTargets::Single)
            }
            OwnedConfiguredTargetNodeArg::String(str) => {
                unpack_string_literal(str, global_cfg_options, ctx, dice).await
            }
            OwnedConfiguredTargetNodeArg::Unconfigured(unconfigured) => unconfigured
                .to_configured_target_node(global_cfg_options, dice)
                .await
                .map(SingleOrCompatibleConfiguredTargets::Single),
        }
    }
}

#[derive(Debug, Clone, Allocative)]
pub(crate) enum OwnedTargetNodeArg {
    Unconfigured(OwnedTargetNodeOrTargetLabel),
    String(String),
}

impl OwnedTargetNodeArg {
    pub(crate) fn from_ref(expr: &TargetNodeOrTargetLabelOrStr<'_>) -> Self {
        match *expr {
            TargetNodeOrTargetLabelOrStr::TargetNode(node) => OwnedTargetNodeArg::Unconfigured(
                OwnedTargetNodeOrTargetLabel::TargetNode(node.dupe()),
            ),
            TargetNodeOrTargetLabelOrStr::TargetLabel(label) => OwnedTargetNodeArg::Unconfigured(
                OwnedTargetNodeOrTargetLabel::TargetLabel(label.dupe()),
            ),
            TargetNodeOrTargetLabelOrStr::Str(str) => OwnedTargetNodeArg::String(str.to_owned()),
        }
    }

    pub(crate) async fn to_unconfigured_target_node(
        &self,
        ctx: &BxlContextCoreData,
        dice: &mut DiceComputations<'_>,
    ) -> buck2_error::Result<Either<StarlarkTargetNode, StarlarkTargetSet<TargetNode>>> {
        match self {
            OwnedTargetNodeArg::Unconfigured(unconfigured) => unconfigured
                .to_unconfigured_target_node(dice)
                .await
                .map(|node| Either::Left(StarlarkTargetNode(node))),
            OwnedTargetNodeArg::String(str) => {
                match ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                    ctx.target_alias_resolver(),
                    CellPathRef::new(ctx.cell_name(), CellRelativePath::empty()),
                    &str,
                    ctx.cell_resolver(),
                    ctx.cell_alias_resolver(),
                )? {
                    ParsedPattern::Target(pkg, name, TargetPatternExtra) => {
                        let label = TargetLabel::new(pkg, name.as_ref());
                        Ok(dice
                            .get_target_node(&label)
                            .await
                            .map(|node| Either::Left(StarlarkTargetNode(node)))?)
                    }
                    pattern => {
                        let loaded_patterns =
                            load_patterns(dice, vec![pattern], MissingTargetBehavior::Fail).await?;
                        let mut target_set = TargetSet::new();
                        for (_package, results) in loaded_patterns.into_iter() {
                            target_set.extend(results?.into_values());
                        }
                        Ok(Either::Right(StarlarkTargetSet(target_set)))
                    }
                }
            }
        }
    }
}
