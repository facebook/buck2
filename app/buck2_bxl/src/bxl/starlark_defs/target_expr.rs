/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;

use buck2_build_api::calculation::load_patterns;
use buck2_build_api::calculation::Calculation;
use buck2_build_api::query::dice::get_compatible_targets;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::ParsedPattern;
use buck2_core::pattern::TargetPatternExtra;
use buck2_core::target::label::TargetLabel;
use buck2_core::truncate::truncate;
use buck2_interpreter::types::target_label::StarlarkConfiguredTargetLabel;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::compatibility::IncompatiblePlatformReason;
use buck2_query::query::compatibility::MaybeCompatible;
use buck2_query::query::environment::QueryEnvironment;
use buck2_query::query::environment::QueryTarget;
use buck2_query::query::syntax::simple::eval::set::TargetSet;
use dice::DiceComputations;
use dupe::Dupe;
use dupe::IterDupedExt;
use either::Either;
use futures::TryFutureExt;
use starlark::collections::SmallSet;
use starlark::eval::Evaluator;
use starlark::values::list::ListRef;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use thiserror::Error;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::nodes::configured::StarlarkConfiguredTargetNode;
use crate::bxl::starlark_defs::nodes::unconfigured::StarlarkTargetNode;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;

/// TargetExpr is just a simple type that can be used in starlark_module
/// functions for arguments that should be target sets. It will accept a
/// literal (like `//some:target`) or list of literals or a TargetSet Value (from one of the
/// BXL functions that return them). It can be resolved to a `&TargetSet` with
/// the help of the `targets!()` macro.
pub enum TargetExpr<'v, Node: QueryTarget> {
    Node(Node),
    Label(Cow<'v, Node::NodeRef>),
    Iterable(Vec<Either<Node, Cow<'v, Node::NodeRef>>>),
    TargetSet(Cow<'v, TargetSet<Node>>),
}

impl<'v> TargetExpr<'v, ConfiguredTargetNode> {
    /// Get a vector of maybe compatible `ConfiguredTargetNode`s from the `TargetExpr`.
    /// Any callers of this function will need to call `filter_incompatible()` on the result
    /// in order to get the `TargetSet<ConfiguredTargetNode>`.
    pub async fn get(
        self,
        dice: &'v DiceComputations,
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
    bxl_ctx: &BxlContext,
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
        let _result = bxl_ctx.print_to_error_stream(
            IncompatiblePlatformReason::skipping_message_for_multiple(incompatible_targets.iter()),
        )?;
    }

    Ok(target_set)
}

impl<'v> TargetExpr<'v, TargetNode> {
    /// Get a `TargetSet<TargetNode>` from the `TargetExpr`
    pub async fn get<QueryEnv: QueryEnvironment<Target = TargetNode>>(
        self,
        env: &QueryEnv,
    ) -> anyhow::Result<Cow<'v, TargetSet<TargetNode>>> {
        match self {
            TargetExpr::Node(val) => {
                let mut set = TargetSet::new();
                set.insert(val);
                Ok(Cow::Owned(set))
            }
            TargetExpr::Label(label) => {
                let node = env.get_node(&label).await?;
                let mut set = TargetSet::new();
                set.insert(node);
                Ok(Cow::Owned(set))
            }
            TargetExpr::Iterable(val) => {
                let mut set = TargetSet::new();
                let futs = val.into_iter().map(|node_or_ref| async {
                    match node_or_ref {
                        Either::Left(node) => Ok(node),
                        Either::Right(node_ref) => env.get_node(&node_ref).await,
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
pub enum TargetExprError {
    #[error(
        "Expected a list of target like items, but was `{0}`. If you have passed in a list of `label`s, make sure to call `configured_target()` to get the underlying configured target label."
    )]
    NotAListOfTargets(String),
    #[error(
        "Expected a single target like item, but was `{0}`. If you have passed in a `label`, make sure to call `configured_target()` to get the underlying configured target label."
    )]
    NotATarget(String),
}

impl<'v> TargetExpr<'v, ConfiguredTargetNode> {
    pub async fn unpack(
        value: Value<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContext<'v>,
        eval: &Evaluator<'v, '_>,
    ) -> anyhow::Result<TargetExpr<'v, ConfiguredTargetNode>> {
        Ok(
            if let Some(resolved) = Self::unpack_literal(value, target_platform, ctx).await? {
                resolved
            } else if let Some(resolved) =
                Self::unpack_iterable(value, target_platform, ctx, eval).await?
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
        ctx: &BxlContext<'v>,
    ) -> anyhow::Result<Option<TargetExpr<'v, ConfiguredTargetNode>>> {
        if let Some(configured_target) = value.downcast_ref::<StarlarkConfiguredTargetNode>() {
            Ok(Some(Self::Node(configured_target.0.dupe())))
        } else if let Some(configured_target) =
            value.downcast_ref::<StarlarkConfiguredTargetLabel>()
        {
            Ok(Some(Self::Label(Cow::Borrowed(configured_target.label()))))
        } else if let Some(s) = value.unpack_str() {
            match ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                &ctx.target_alias_resolver,
                ctx.cell.cell_alias_resolver(),
                PackageLabel::new(ctx.cell.name(), CellRelativePath::empty()),
                s,
            )? {
                ParsedPattern::Target(pkg, name, TargetPatternExtra) => {
                    Ok(Some(Self::Label(Cow::Owned(
                        ctx.async_ctx
                            .0
                            .get_configured_target(
                                &TargetLabel::new(pkg, name.as_ref()),
                                target_platform.as_ref(),
                            )
                            .await?,
                    ))))
                }
                pattern => {
                    let loaded_patterns = load_patterns(ctx.async_ctx.0, vec![pattern]).await?;
                    Ok(Some(Self::TargetSet(Cow::Owned(
                        get_compatible_targets(
                            ctx.async_ctx.0,
                            loaded_patterns.iter_loaded_targets_by_package(),
                            target_platform.dupe(),
                        )
                        .await?,
                    ))))
                }
            }
        } else {
            #[allow(clippy::manual_map)] // `if else if` looks better here
            let maybe_unconfigured =
                if let Some(target) = value.downcast_ref::<StarlarkTargetLabel>() {
                    Some(Cow::Borrowed(target.label()))
                } else if let Some(node) = value.downcast_ref::<StarlarkTargetNode>() {
                    Some(Cow::Borrowed(node.0.label()))
                } else {
                    None
                };

            match maybe_unconfigured {
                None => Ok(None),
                Some(label) => Ok(Some(Self::Label(Cow::Owned(
                    ctx.async_ctx
                        .0
                        .get_configured_target(&*label, target_platform.as_ref())
                        .await?,
                )))),
            }
        }
    }

    async fn unpack_iterable(
        value: Value<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContext<'v>,
        eval: &Evaluator<'v, '_>,
    ) -> anyhow::Result<Option<TargetExpr<'v, ConfiguredTargetNode>>> {
        if let Some(s) = value.downcast_ref::<StarlarkTargetSet<ConfiguredTargetNode>>() {
            return Ok(Some(Self::TargetSet(Cow::Borrowed(s))));
        }

        #[allow(clippy::manual_map)] // `if else if` looks better here
        let items = if let Some(s) = value.downcast_ref::<StarlarkTargetSet<TargetNode>>() {
            Some(Either::Left(s.iterate(eval.heap())?))
        } else if let Some(iterable) = ListRef::from_value(value) {
            Some(Either::Right(iterable.iter()))
        } else {
            None
        }
        .ok_or_else(|| TargetExprError::NotAListOfTargets(value.to_repr()))?;

        let mut resolved = vec![];

        for item in items {
            let unpacked = Self::unpack_literal(item, target_platform, ctx).await?;

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
    pub async fn unpack(
        value: Value<'v>,
        ctx: &BxlContext<'v>,
        eval: &Evaluator<'v, '_>,
    ) -> anyhow::Result<TargetExpr<'v, TargetNode>> {
        Ok(
            if let Some(resolved) = Self::unpack_literal(value, ctx).await? {
                resolved
            } else if let Some(resolved) = Self::unpack_iterable(value, ctx, eval).await? {
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
        ctx: &BxlContext<'v>,
    ) -> anyhow::Result<Option<TargetExpr<'v, TargetNode>>> {
        if let Some(target) = value.downcast_ref::<StarlarkTargetNode>() {
            Ok(Some(Self::Node(target.0.dupe())))
        } else if let Some(label) = value.downcast_ref::<StarlarkTargetLabel>() {
            Ok(Some(Self::Label(Cow::Borrowed(label.label()))))
        } else if let Some(s) = value.unpack_str() {
            match ParsedPattern::<TargetPatternExtra>::parse_relaxed(
                &ctx.target_alias_resolver,
                ctx.cell.cell_alias_resolver(),
                PackageLabel::new(ctx.cell.name(), CellRelativePath::empty()),
                s,
            )? {
                ParsedPattern::Target(pkg, name, TargetPatternExtra) => Ok(Some(Self::Label(
                    Cow::Owned(TargetLabel::new(pkg, name.as_ref())),
                ))),
                pattern => {
                    let loaded_patterns = load_patterns(ctx.async_ctx.0, vec![pattern]).await?;
                    let mut target_set = TargetSet::new();
                    for (_package, results) in loaded_patterns.into_iter() {
                        target_set.extend(results?.into_values());
                    }
                    Ok(Some(Self::TargetSet(Cow::Owned(target_set))))
                }
            }
        } else {
            #[allow(clippy::manual_map)] // `if else if` looks better here
            let maybe_unconfigured =
                if let Some(target) = value.downcast_ref::<StarlarkTargetLabel>() {
                    Some(Cow::Borrowed(target.label()))
                } else if let Some(node) = value.downcast_ref::<StarlarkTargetNode>() {
                    Some(Cow::Borrowed(node.0.label()))
                } else {
                    None
                };

            match maybe_unconfigured {
                None => Ok(None),
                Some(label) => Ok(Some(Self::Label(label))),
            }
        }
    }

    async fn unpack_iterable(
        value: Value<'v>,
        ctx: &BxlContext<'v>,
        eval: &Evaluator<'v, '_>,
    ) -> anyhow::Result<Option<TargetExpr<'v, TargetNode>>> {
        if let Some(s) = value.downcast_ref::<StarlarkTargetSet<TargetNode>>() {
            return Ok(Some(Self::TargetSet(Cow::Borrowed(s))));
        }

        #[allow(clippy::manual_map)] // `if else if` looks better here
        let items = if let Some(s) = value.downcast_ref::<StarlarkTargetSet<TargetNode>>() {
            Some(Either::Left(s.iterate(eval.heap())?))
        } else if let Some(iterable) = ListRef::from_value(value) {
            Some(Either::Right(iterable.iter()))
        } else {
            None
        }
        .ok_or_else(|| TargetExprError::NotAListOfTargets(value.to_repr()))?;

        let mut resolved = vec![];

        for item in items {
            let unpacked = Self::unpack_literal(item, ctx).await?;

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
