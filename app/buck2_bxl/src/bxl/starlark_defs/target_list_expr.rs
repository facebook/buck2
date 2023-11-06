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

use anyhow::Context;
use buck2_build_api::configure_targets::get_maybe_compatible_targets;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::configuration::compatibility::IncompatiblePlatformReason;
use buck2_core::configuration::compatibility::MaybeCompatible;
use buck2_core::pattern::pattern_type::TargetPatternExtra;
use buck2_core::pattern::ParsedPattern;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::soft_error;
use buck2_core::target::label::TargetLabel;
use buck2_interpreter::types::target_label::StarlarkConfiguredTargetLabel;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use buck2_node::load_patterns::load_patterns;
use buck2_node::load_patterns::MissingTargetBehavior;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::configured_frontend::ConfiguredTargetNodeCalculation;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use buck2_util::truncate::truncate;
use dice::DiceComputations;
use dupe::Dupe;
use dupe::IterDupedExt;
use futures::future;
use starlark::collections::SmallSet;
use starlark::values::list::UnpackList;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::UnpackValue;
use starlark::values::ValueOf;

use crate::bxl::starlark_defs::context::BxlContextNoDice;
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
        dice: &mut DiceComputations,
    ) -> anyhow::Result<Vec<MaybeCompatible<ConfiguredTargetNode>>> {
        let futs = self.iter().map(|node_or_ref| async {
            let node_or_ref = node_or_ref;
            dice.get_configured_target_node(node_or_ref.node_ref())
                .await
        });

        futures::future::join_all(futs).await.into_iter().collect()
    }

    /// Get a single maybe compatible `ConfiguredTargetNode`.
    pub(crate) async fn get_one(
        &self,
        dice: &mut DiceComputations,
    ) -> anyhow::Result<Option<MaybeCompatible<ConfiguredTargetNode>>> {
        Ok(match &self {
            Self::One(node_or_ref) => Some(
                dice.get_configured_target_node(node_or_ref.node_ref())
                    .await?,
            ),
            _ => None,
        })
    }
}

// Filters out incompatible targets and emits the error message
pub(crate) fn filter_incompatible(
    targets: impl Iterator<Item = MaybeCompatible<ConfiguredTargetNode>>,
    bxl_ctx: &BxlContextNoDice,
) -> anyhow::Result<TargetSet<ConfiguredTargetNode>> {
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
        bxl_ctx.print_to_error_stream(
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
        ctx: &DiceComputations,
    ) -> anyhow::Result<Cow<'v, TargetSet<TargetNode>>> {
        let mut set = TargetSet::new();
        let futs = self.iter().map(|node_or_ref| async {
            let node_or_ref = node_or_ref;
            node_or_ref.get_from_dice(ctx).await
        });

        for node in futures::future::join_all(futs).await {
            set.insert(node?);
        }

        Ok(Cow::Owned(set))
    }

    /// Get a single `TargetNode`
    pub(crate) async fn get_one(
        &self,
        ctx: &DiceComputations,
    ) -> anyhow::Result<Option<TargetNode>> {
        Ok(match &self {
            Self::One(node_or_ref) => Some(node_or_ref.get_from_dice(ctx).await?),
            _ => None,
        })
    }
}

#[derive(Debug, buck2_error::Error)]
pub(crate) enum TargetExprError {
    #[error(
        "Expected a single target like item, but was `{0}`. If you have passed in a `label`, make sure to call `configured_target()` to get the underlying configured target label."
    )]
    NotATarget(String),
    #[error(
        "Unconfigured target with label `{0}` was passed into cquery. Targets passed into cquery should be configured (recommendation is to use `ctx.target_universe()`)."
    )]
    UnconfiguredTargetInCquery(String),
}

impl<'v> TargetListExpr<'v, ConfiguredTargetNode> {
    pub(crate) fn as_provider_labels(&self) -> Vec<ConfiguredProvidersLabel> {
        self.iter()
            .map(|e| ConfiguredProvidersLabel::default_for(e.node_ref().dupe()))
            .collect()
    }

    pub(crate) async fn unpack_opt<'c>(
        arg: ConfiguredTargetListExprArg<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContextNoDice<'v>,
        dice: &mut DiceComputations,
        allow_unconfigured: bool,
    ) -> anyhow::Result<TargetListExpr<'v, ConfiguredTargetNode>> {
        match arg {
            ConfiguredTargetListExprArg::Target(arg) => {
                Ok(
                    Self::unpack_literal(arg, target_platform, ctx, dice, allow_unconfigured)
                        .await?,
                )
            }
            ConfiguredTargetListExprArg::List(arg) => {
                Ok(
                    Self::unpack_iterable(arg, target_platform, ctx, dice, allow_unconfigured)
                        .await?,
                )
            }
        }
    }

    pub(crate) async fn unpack<'c>(
        // TODO(nga): this does not accept unconfigured targets, so should be narrower type here.
        arg: ConfiguredTargetListExprArg<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContextNoDice<'v>,
        dice: &mut DiceComputations,
    ) -> anyhow::Result<TargetListExpr<'v, ConfiguredTargetNode>> {
        Self::unpack_opt(arg, target_platform, ctx, dice, false).await
    }

    pub(crate) async fn unpack_allow_unconfigured<'c>(
        arg: ConfiguredTargetListExprArg<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContextNoDice<'v>,
        dice: &mut DiceComputations,
    ) -> anyhow::Result<TargetListExpr<'v, ConfiguredTargetNode>> {
        Self::unpack_opt(arg, target_platform, ctx, dice, true).await
    }

    fn check_allow_unconfigured(
        allow_unconfigured: bool,
        unconfigured_label: &str,
        target_platform: &Option<TargetLabel>,
    ) -> anyhow::Result<()> {
        if !allow_unconfigured {
            if target_platform.is_none() {
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
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContextNoDice<'_>,
        dice: &mut DiceComputations,
        allow_unconfigured: bool,
    ) -> anyhow::Result<TargetListExpr<'v, ConfiguredTargetNode>> {
        match arg {
            ConfiguredTargetNodeArg::ConfiguredTargetNode(configured_target) => {
                Ok(Self::One(TargetExpr::Node(configured_target.0.dupe())))
            }
            ConfiguredTargetNodeArg::ConfiguredTargetLabel(configured_target) => Ok(
                TargetListExpr::One(TargetExpr::Label(Cow::Borrowed(configured_target.label()))),
            ),
            ConfiguredTargetNodeArg::Str(s) => {
                Self::check_allow_unconfigured(allow_unconfigured, s, target_platform)?;

                match ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                    &ctx.target_alias_resolver,
                    // TODO(nga): Parse relaxed relative to cell root is incorrect.
                    CellPathRef::new(ctx.cell_name, CellRelativePath::empty()),
                    s,
                    &ctx.cell_resolver,
                )? {
                    ParsedPattern::Target(pkg, name, TargetPatternExtra) => {
                        Ok(TargetListExpr::One(TargetExpr::Label(Cow::Owned(
                            dice.get_configured_target(
                                &TargetLabel::new(pkg, name.as_ref()),
                                target_platform.as_ref(),
                            )
                            .await?,
                        ))))
                    }
                    pattern => {
                        let loaded_patterns =
                            load_patterns(dice, vec![pattern], MissingTargetBehavior::Fail).await?;

                        let maybe_compatible = get_maybe_compatible_targets(
                            dice,
                            loaded_patterns.iter_loaded_targets_by_package(),
                            target_platform.as_ref(),
                        )
                        .await?
                        .collect::<Result<Vec<_>, _>>()?;

                        let result = filter_incompatible(maybe_compatible.into_iter(), ctx)?;
                        Ok(Self::TargetSet(Cow::Owned(result)))
                    }
                }
            }
            ConfiguredTargetNodeArg::Unconfigured(label) => {
                Self::check_allow_unconfigured(
                    allow_unconfigured,
                    &label.label().to_string(),
                    target_platform,
                )?;
                Ok(TargetListExpr::One(TargetExpr::Label(Cow::Owned(
                    dice.get_configured_target(label.label(), target_platform.as_ref())
                        .await?,
                ))))
            }
        }
    }

    async fn unpack_iterable<'c>(
        value: ValueOf<'v, ConfiguredTargetListArg<'v>>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContextNoDice<'_>,
        dice: &mut DiceComputations,
        allow_unconfigured: bool,
    ) -> anyhow::Result<TargetListExpr<'v, ConfiguredTargetNode>> {
        match value.typed {
            ConfiguredTargetListArg::ConfiguredTargetSet(s) => {
                return Ok(Self::TargetSet(Cow::Borrowed(s)));
            }
            ConfiguredTargetListArg::TargetSet(s) => {
                return Ok(TargetListExpr::Iterable(
                    future::try_join_all(s.0.iter().map(|node| async {
                        Self::check_allow_unconfigured(
                            allow_unconfigured,
                            &node.label().to_string(),
                            target_platform,
                        )?;
                        anyhow::Ok(TargetExpr::Label(Cow::Owned(
                            dice.get_configured_target(node.label(), target_platform.as_ref())
                                .await?,
                        )))
                    }))
                    .await?,
                ));
            }
            ConfiguredTargetListArg::TargetList(unpack) => {
                let mut resolved = vec![];

                for item in unpack.items {
                    let unpacked =
                        Self::unpack_literal(item, target_platform, ctx, dice, allow_unconfigured)
                            .await?;

                    match unpacked {
                        TargetListExpr::One(node) => resolved.push(node),
                        TargetListExpr::TargetSet(set) => match set {
                            Cow::Borrowed(s) => itertools::Either::Left(s.iter().duped()),
                            Cow::Owned(s) => itertools::Either::Right(s.into_iter()),
                        }
                        .for_each(|t| resolved.push(TargetExpr::Node(t))),
                        _ => {
                            return Err(anyhow::anyhow!(TargetExprError::NotATarget(
                                value.value.to_repr()
                            ))
                            .context(format!(
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
        dice: &mut DiceComputations,
    ) -> anyhow::Result<TargetListExpr<'v, TargetNode>> {
        match value {
            TargetListExprArg::Target(x) => Self::unpack_literal(x, ctx, dice).await,
            TargetListExprArg::List(x) => Self::unpack_iterable(x, ctx, dice).await,
        }
    }

    async fn unpack_literal(
        value: TargetNodeOrTargetLabelOrStr<'v>,
        ctx: &BxlContextNoDice<'_>,
        dice: &mut DiceComputations,
    ) -> anyhow::Result<TargetListExpr<'v, TargetNode>> {
        match value {
            TargetNodeOrTargetLabelOrStr::TargetNode(target) => {
                Ok(TargetListExpr::One(TargetExpr::Node(target.0.dupe())))
            }
            TargetNodeOrTargetLabelOrStr::TargetLabel(target) => Ok(TargetListExpr::One(
                TargetExpr::Label(Cow::Borrowed(target.label())),
            )),
            TargetNodeOrTargetLabelOrStr::Str(s) => {
                match ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                    &ctx.target_alias_resolver,
                    // TODO(nga): Parse relaxed relative to cell root is incorrect.
                    CellPathRef::new(ctx.cell_name, CellRelativePath::empty()),
                    s,
                    &ctx.cell_resolver,
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
        dice: &mut DiceComputations,
    ) -> anyhow::Result<TargetListExpr<'v, TargetNode>> {
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
                                .context("list in a list");
                        }
                    }
                }
                Ok(Self::Iterable(resolved))
            }
        }
    }
}
