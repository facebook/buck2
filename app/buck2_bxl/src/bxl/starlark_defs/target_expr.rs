/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;

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
use futures::TryFutureExt;
use starlark::collections::SmallSet;
use starlark::eval::Evaluator;
use starlark::values::list::ListRef;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use thiserror::Error;

use crate::bxl::starlark_defs::context::BxlContextNoDice;
use crate::bxl::starlark_defs::nodes::configured::StarlarkConfiguredTargetNode;
use crate::bxl::starlark_defs::nodes::unconfigured::StarlarkTargetNode;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;

/// TargetExpr is just a simple type that can be used in starlark_module
/// functions for arguments that should be target sets. It will accept a
/// literal (like `//some:target`) or list of literals or a TargetSet Value (from one of the
/// BXL functions that return them). It can be resolved to a `&TargetSet` with
/// the help of the `targets!()` macro.
pub(crate) enum TargetExpr<'v, Node: QueryTarget> {
    Node(Node),
    Label(Cow<'v, Node::NodeRef>),
    Iterable(Vec<Either<Node, Cow<'v, Node::NodeRef>>>),
    TargetSet(Cow<'v, TargetSet<Node>>),
}

impl<'v> TargetExpr<'v, ConfiguredTargetNode> {
    /// Get a vector of maybe compatible `ConfiguredTargetNode`s from the `TargetExpr`.
    /// Any callers of this function will need to call `filter_incompatible()` on the result
    /// in order to get the `TargetSet<ConfiguredTargetNode>`.
    pub(crate) async fn get(
        self,
        dice: &mut DiceComputations,
    ) -> anyhow::Result<Vec<MaybeCompatible<ConfiguredTargetNode>>> {
        match self {
            TargetExpr::Node(val) => Ok(vec![dice.get_configured_target_node(val.label()).await?]),
            TargetExpr::Label(label) => {
                Ok(vec![dice.get_configured_target_node(label.as_ref()).await?])
            }
            TargetExpr::Iterable(val) => {
                let futs = val.into_iter().map(|node_or_ref| async {
                    match node_or_ref {
                        Either::Left(node) => dice.get_configured_target_node(node.label()).await,
                        Either::Right(label) => {
                            dice.get_configured_target_node(label.as_ref()).await
                        }
                    }
                });

                futures::future::join_all(futs).await.into_iter().collect()
            }
            TargetExpr::TargetSet(val) => futures::future::join_all(val.iter().map(|node| {
                dice.get_configured_target_node(node.label())
                    .map_err(anyhow::Error::from)
            }))
            .await
            .into_iter()
            .collect(),
        }
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
enum TargetNodeOrTargetLabel<'v> {
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
enum TargetNodeOrTargetLabelOrStr<'v> {
    TargetNode(&'v StarlarkTargetNode),
    TargetLabel(&'v StarlarkTargetLabel),
    Str(&'v str),
}

impl<'v> TargetExpr<'v, TargetNode> {
    /// Get a `TargetSet<TargetNode>` from the `TargetExpr`
    pub(crate) async fn get(
        self,
        ctx: &DiceComputations,
    ) -> anyhow::Result<Cow<'v, TargetSet<TargetNode>>> {
        match self {
            TargetExpr::Node(val) => {
                let mut set = TargetSet::new();
                set.insert(val);
                Ok(Cow::Owned(set))
            }
            TargetExpr::Label(label) => {
                let node = ctx.get_target_node(&label).await?;
                let mut set = TargetSet::new();
                set.insert(node);
                Ok(Cow::Owned(set))
            }
            TargetExpr::Iterable(val) => {
                let mut set = TargetSet::new();
                let futs = val.into_iter().map(|node_or_ref| async {
                    match node_or_ref {
                        Either::Left(node) => Ok(node),
                        Either::Right(node_ref) => ctx.get_target_node(&node_ref).await,
                    }
                });

                for node in futures::future::join_all(futs).await {
                    set.insert(node?);
                }

                Ok(Cow::Owned(set))
            }
            TargetExpr::TargetSet(val) => Ok(val),
        }
    }
}

#[derive(Debug, Error)]
pub(crate) enum TargetExprError {
    #[error(
        "Expected a list of target like items, but was `{0}`. If you have passed in a list of `label`s, make sure to call `configured_target()` to get the underlying configured target label."
    )]
    NotAListOfTargets(String),
    #[error(
        "Expected a single target like item, but was `{0}`. If you have passed in a `label`, make sure to call `configured_target()` to get the underlying configured target label."
    )]
    NotATarget(String),
    #[error(
        "Unconfigured target with label `{0}` was passed into cquery. Targets passed into cquery should be configured (recommendation is to use `ctx.target_universe()`)."
    )]
    UnconfiguredTargetInCquery(String),
}

impl<'v> TargetExpr<'v, ConfiguredTargetNode> {
    pub(crate) fn as_provider_labels(&self) -> Vec<ConfiguredProvidersLabel> {
        match &self {
            TargetExpr::Iterable(i) => i
                .iter()
                .map(|e| match e {
                    Either::Left(node) => {
                        ConfiguredProvidersLabel::default_for(node.label().dupe())
                    }
                    Either::Right(label) => {
                        ConfiguredProvidersLabel::default_for(label.as_ref().clone())
                    }
                })
                .collect(),
            TargetExpr::Label(l) => vec![ConfiguredProvidersLabel::default_for(l.as_ref().clone())],
            TargetExpr::Node(n) => vec![ConfiguredProvidersLabel::default_for(n.label().dupe())],
            TargetExpr::TargetSet(t) => t
                .iter()
                .map(|n| ConfiguredProvidersLabel::default_for(n.label().dupe()))
                .collect(),
        }
    }

    pub(crate) async fn unpack_opt<'c>(
        value: Value<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContextNoDice<'v>,
        dice: &mut DiceComputations,
        eval: &Evaluator<'v, 'c>,
        allow_unconfigured: bool,
    ) -> anyhow::Result<Option<TargetExpr<'v, ConfiguredTargetNode>>> {
        Ok(
            if let Some(resolved) =
                Self::unpack_literal(value, target_platform, ctx, dice, allow_unconfigured).await?
            {
                Some(resolved)
            } else {
                Self::unpack_iterable(value, target_platform, ctx, dice, eval, allow_unconfigured)
                    .await?
            },
        )
    }

    pub(crate) async fn unpack<'c>(
        value: Value<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContextNoDice<'v>,
        dice: &mut DiceComputations,
        eval: &Evaluator<'v, 'c>,
    ) -> anyhow::Result<TargetExpr<'v, ConfiguredTargetNode>> {
        Ok(
            if let Some(resolved) =
                Self::unpack_opt(value, target_platform, ctx, dice, eval, false).await?
            {
                resolved
            } else {
                return Err(anyhow::anyhow!(TargetExprError::NotAListOfTargets(
                    value.to_repr()
                )));
            },
        )
    }

    pub(crate) async fn unpack_allow_unconfigured<'c>(
        value: Value<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContextNoDice<'v>,
        dice: &mut DiceComputations,
        eval: &Evaluator<'v, 'c>,
    ) -> anyhow::Result<TargetExpr<'v, ConfiguredTargetNode>> {
        Ok(
            if let Some(resolved) =
                Self::unpack_opt(value, target_platform, ctx, dice, eval, true).await?
            {
                resolved
            } else {
                return Err(anyhow::anyhow!(TargetExprError::NotAListOfTargets(
                    value.to_repr()
                )));
            },
        )
    }

    async fn unpack_literal(
        value: Value<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContextNoDice<'_>,
        dice: &mut DiceComputations,
        allow_unconfigured: bool,
    ) -> anyhow::Result<Option<TargetExpr<'v, ConfiguredTargetNode>>> {
        if let Some(configured_target) = value.downcast_ref::<StarlarkConfiguredTargetNode>() {
            Ok(Some(Self::Node(configured_target.0.dupe())))
        } else if let Some(configured_target) =
            value.downcast_ref::<StarlarkConfiguredTargetLabel>()
        {
            Ok(Some(Self::Label(Cow::Borrowed(configured_target.label()))))
        } else {
            // Handle the unconfigured case
            let mut unconfigured_label = None;
            let result = if let Some(s) = value.unpack_str() {
                unconfigured_label = Some(s.to_owned());

                match ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                    &ctx.target_alias_resolver,
                    // TODO(nga): Parse relaxed relative to cell root is incorrect.
                    CellPathRef::new(ctx.cell_name, CellRelativePath::empty()),
                    s,
                    &ctx.cell_resolver,
                )? {
                    ParsedPattern::Target(pkg, name, TargetPatternExtra) => {
                        Ok(Some(Self::Label(Cow::Owned(
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
                        Ok(Some(Self::TargetSet(Cow::Owned(result))))
                    }
                }
            } else {
                match TargetNodeOrTargetLabel::unpack_value(value) {
                    None => Ok(None),
                    Some(label) => {
                        unconfigured_label = Some(label.label().to_string());
                        Ok(Some(Self::Label(Cow::Owned(
                            dice.get_configured_target(label.label(), target_platform.as_ref())
                                .await?,
                        ))))
                    }
                }
            };

            if !allow_unconfigured {
                if let Some(unconfigured_label) = unconfigured_label {
                    if target_platform.is_none() {
                        soft_error!(
                            "bxl_unconfigured_target_in_cquery",
                            TargetExprError::UnconfiguredTargetInCquery(unconfigured_label).into()
                        )?;
                    }
                }
            }

            result
        }
    }

    async fn unpack_iterable<'c>(
        value: Value<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContextNoDice<'_>,
        dice: &mut DiceComputations,
        eval: &Evaluator<'v, 'c>,
        allow_unconfigured: bool,
    ) -> anyhow::Result<Option<TargetExpr<'v, ConfiguredTargetNode>>> {
        if let Some(s) = value.downcast_ref::<StarlarkTargetSet<ConfiguredTargetNode>>() {
            return Ok(Some(Self::TargetSet(Cow::Borrowed(s))));
        }

        #[allow(clippy::manual_map)] // `if else if` looks better here
        let items = if let Some(s) = value.downcast_ref::<StarlarkTargetSet<TargetNode>>() {
            Some(Either::Left(s.iter(eval.heap())))
        } else if let Some(iterable) = ListRef::from_value(value) {
            Some(Either::Right(iterable.iter()))
        } else {
            None
        }
        .ok_or_else(|| TargetExprError::NotAListOfTargets(value.to_repr()))?;

        let mut resolved = vec![];

        for item in items {
            let unpacked =
                Self::unpack_literal(item, target_platform, ctx, dice, allow_unconfigured).await?;

            match unpacked {
                Some(TargetExpr::Node(node)) => resolved.push(Either::Left(node)),
                Some(TargetExpr::Label(label)) => resolved.push(Either::Right(label)),
                Some(TargetExpr::TargetSet(set)) => match set {
                    Cow::Borrowed(s) => itertools::Either::Left(s.iter().duped()),
                    Cow::Owned(s) => itertools::Either::Right(s.into_iter()),
                }
                .for_each(|t| resolved.push(Either::Left(t))),
                _ => {
                    return Err(anyhow::anyhow!(TargetExprError::NotATarget(item.to_repr()))
                        .context(format!(
                            "Error resolving list `{}`",
                            truncate(&value.to_repr(), 150)
                        )));
                }
            }
        }

        Ok(Some(Self::Iterable(resolved)))
    }
}

impl<'v> TargetExpr<'v, TargetNode> {
    pub(crate) async fn unpack<'c>(
        value: Value<'v>,
        ctx: &BxlContextNoDice<'_>,
        dice: &mut DiceComputations,
        eval: &Evaluator<'v, 'c>,
    ) -> anyhow::Result<TargetExpr<'v, TargetNode>> {
        Ok(
            if let Some(resolved) = Self::unpack_literal(value, ctx, dice).await? {
                resolved
            } else if let Some(resolved) = Self::unpack_iterable(value, ctx, dice, eval).await? {
                resolved
            } else {
                return Err(anyhow::anyhow!(TargetExprError::NotAListOfTargets(
                    value.to_repr()
                )));
            },
        )
    }

    async fn unpack_literal(
        value: Value<'v>,
        ctx: &BxlContextNoDice<'_>,
        dice: &mut DiceComputations,
    ) -> anyhow::Result<Option<TargetExpr<'v, TargetNode>>> {
        match TargetNodeOrTargetLabelOrStr::unpack_value(value) {
            Some(TargetNodeOrTargetLabelOrStr::TargetNode(target)) => {
                Ok(Some(Self::Node(target.0.dupe())))
            }
            Some(TargetNodeOrTargetLabelOrStr::TargetLabel(target)) => {
                Ok(Some(Self::Label(Cow::Borrowed(target.label()))))
            }
            Some(TargetNodeOrTargetLabelOrStr::Str(s)) => {
                match ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                    &ctx.target_alias_resolver,
                    // TODO(nga): Parse relaxed relative to cell root is incorrect.
                    CellPathRef::new(ctx.cell_name, CellRelativePath::empty()),
                    s,
                    &ctx.cell_resolver,
                )? {
                    ParsedPattern::Target(pkg, name, TargetPatternExtra) => Ok(Some(Self::Label(
                        Cow::Owned(TargetLabel::new(pkg, name.as_ref())),
                    ))),
                    pattern => {
                        let loaded_patterns =
                            load_patterns(dice, vec![pattern], MissingTargetBehavior::Fail).await?;
                        let mut target_set = TargetSet::new();
                        for (_package, results) in loaded_patterns.into_iter() {
                            target_set.extend(results?.into_values());
                        }
                        Ok(Some(Self::TargetSet(Cow::Owned(target_set))))
                    }
                }
            }
            None => Ok(None),
        }
    }

    async fn unpack_iterable<'c>(
        value: Value<'v>,
        ctx: &BxlContextNoDice<'_>,
        dice: &mut DiceComputations,
        eval: &Evaluator<'v, 'c>,
    ) -> anyhow::Result<Option<TargetExpr<'v, TargetNode>>> {
        if let Some(s) = value.downcast_ref::<StarlarkTargetSet<TargetNode>>() {
            return Ok(Some(Self::TargetSet(Cow::Borrowed(s))));
        }

        #[allow(clippy::manual_map)] // `if else if` looks better here
        let items = if let Some(s) = value.downcast_ref::<StarlarkTargetSet<TargetNode>>() {
            Some(Either::Left(s.iter(eval.heap())))
        } else if let Some(iterable) = ListRef::from_value(value) {
            Some(Either::Right(iterable.iter()))
        } else {
            None
        }
        .ok_or_else(|| TargetExprError::NotAListOfTargets(value.to_repr()))?;

        let mut resolved = vec![];

        for item in items {
            let unpacked = Self::unpack_literal(item, ctx, dice).await?;

            match unpacked {
                Some(TargetExpr::Node(node)) => resolved.push(Either::Left(node)),
                Some(TargetExpr::Label(label)) => resolved.push(Either::Right(label)),
                Some(TargetExpr::TargetSet(set)) => match set {
                    Cow::Borrowed(s) => itertools::Either::Left(s.iter().duped()),
                    Cow::Owned(s) => itertools::Either::Right(s.into_iter()),
                }
                .for_each(|t| resolved.push(Either::Left(t))),
                _ => {
                    return Err(anyhow::anyhow!(TargetExprError::NotATarget(item.to_repr()))
                        .context(format!(
                            "Error resolving list `{}`",
                            truncate(&value.to_repr(), 150)
                        )));
                }
            }
        }
        Ok(Some(Self::Iterable(resolved)))
    }
}
