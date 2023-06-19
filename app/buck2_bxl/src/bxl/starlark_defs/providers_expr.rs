/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::pattern::ParsedPattern;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersLabelMaybeConfigured;
use buck2_core::provider::label::ProvidersName;
use buck2_core::target::label::TargetLabel;
use buck2_interpreter::types::label::Label;
use buck2_interpreter::types::label::StarlarkProvidersLabel;
use buck2_interpreter::types::target_label::StarlarkConfiguredTargetLabel;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use dupe::Dupe;
use itertools::Either;
use starlark::eval::Evaluator;
use starlark::values::list::ListRef;
use starlark::values::Value;
use starlark::values::ValueLike;
use thiserror::Error;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::nodes::configured::StarlarkConfiguredTargetNode;
use crate::bxl::starlark_defs::nodes::unconfigured::StarlarkTargetNode;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;
use crate::bxl::value_as_starlark_target_label::ValueAsStarlarkTargetLabel;

#[derive(Debug, Error)]
enum ProviderExprError {
    #[error("Expected a list of target like items, but was `{0}`")]
    NotAListOfTargets(String),
    #[error("Expected a single target like item, but was `{0}`")]
    NotATarget(String),
}

/// ProvidersExpr is just a simple type that can be used in starlark_module
/// functions for arguments that should be a set of provider labels. It will accept a
/// literal (like `//some:target[subtarget]`) or list of literals or a single provider label
pub(crate) enum ProvidersExpr<P: ProvidersLabelMaybeConfigured> {
    Literal(P),
    Iterable(Vec<P>),
}

impl ProvidersExpr<ConfiguredProvidersLabel> {
    pub fn unpack<'v>(
        value: Value<'v>,
        target_platform: Value<'v>,
        ctx: &BxlContext,
        eval: &Evaluator<'v, '_>,
    ) -> anyhow::Result<Self> {
        let target_platform = target_platform.parse_target_platforms(
            &ctx.target_alias_resolver,
            &ctx.cell_resolver,
            ctx.cell_name,
            &ctx.global_target_platform,
        )?;

        Ok(
            if let Some(resolved) = Self::unpack_literal(value, &target_platform, ctx)? {
                resolved
            } else if let Some(resolved) = Self::unpack_iterable(value, ctx, eval, |v, ctx| {
                Self::unpack_literal(v, &target_platform, ctx)
            })? {
                resolved
            } else {
                return Err(anyhow::anyhow!(ProviderExprError::NotAListOfTargets(
                    value.to_repr()
                )));
            },
        )
    }

    fn unpack_literal<'v>(
        value: Value<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContext,
    ) -> anyhow::Result<Option<Self>> {
        if let Some(configured_target) = value.downcast_ref::<StarlarkConfiguredTargetNode>() {
            Ok(Some(Self::Literal(ConfiguredProvidersLabel::new(
                configured_target.0.label().dupe(),
                ProvidersName::Default,
            ))))
        } else if let Some(configured_target) =
            value.downcast_ref::<StarlarkConfiguredTargetLabel>()
        {
            Ok(Some(Self::Literal(ConfiguredProvidersLabel::new(
                configured_target.label().dupe(),
                ProvidersName::Default,
            ))))
        } else if let Some(configured_target) = value.downcast_ref::<Label>() {
            Ok(Some(Self::Literal(configured_target.label().clone())))
        } else {
            Self::unpack_providers_label(value, ctx)?.map_or(Ok(None), |label| {
                let result: anyhow::Result<_> = try {
                    Self::Literal(ctx.async_ctx.via_dice(|ctx| {
                        ctx.get_configured_provider_label(&label, target_platform.as_ref())
                    })?)
                };
                result.map(Some)
            })
        }
    }
}

impl ProvidersExpr<ProvidersLabel> {
    pub fn unpack<'v>(
        value: Value<'v>,
        ctx: &BxlContext<'_>,
        eval: &Evaluator<'v, '_>,
    ) -> anyhow::Result<Self> {
        Ok(if let Some(resolved) = Self::unpack_literal(value, ctx)? {
            resolved
        } else if let Some(resolved) =
            Self::unpack_iterable(value, ctx, eval, Self::unpack_literal)?
        {
            resolved
        } else {
            return Err(anyhow::anyhow!(ProviderExprError::NotAListOfTargets(
                value.to_repr()
            )));
        })
    }

    fn unpack_literal<'v>(value: Value<'v>, ctx: &BxlContext<'_>) -> anyhow::Result<Option<Self>> {
        Self::unpack_providers_label(value, ctx)?
            .map_or(Ok(None), |label| Ok(Some(Self::Literal(label))))
    }
}

impl<P: ProvidersLabelMaybeConfigured> ProvidersExpr<P> {
    pub fn labels(&self) -> impl Iterator<Item = &P> {
        match &self {
            ProvidersExpr::Literal(item) => Either::Left(std::iter::once(item)),
            ProvidersExpr::Iterable(iter) => Either::Right(iter.iter()),
        }
    }

    fn unpack_providers_label<'v>(
        value: Value<'v>,
        ctx: &BxlContext<'_>,
    ) -> anyhow::Result<Option<ProvidersLabel>> {
        #[allow(clippy::manual_map)] // `if else if` looks better here
        Ok(if let Some(s) = value.unpack_str() {
            Some(
                ParsedPattern::<ProvidersPatternExtra>::parse_relaxed(
                    &ctx.target_alias_resolver,
                    // TODO(nga): Parse relaxed relative to cell root is incorrect.
                    CellPathRef::new(ctx.cell_name, CellRelativePath::empty()),
                    s,
                    &ctx.cell_resolver,
                )?
                .as_providers_label(s)?,
            )
        } else if let Some(target) = value.downcast_ref::<StarlarkTargetLabel>() {
            Some(ProvidersLabel::new(
                target.label().dupe(),
                ProvidersName::Default,
            ))
        } else if let Some(label) = value.downcast_ref::<StarlarkProvidersLabel>() {
            Some(label.label().clone())
        } else if let Some(node) = value.downcast_ref::<StarlarkTargetNode>() {
            Some(ProvidersLabel::new(
                node.0.label().dupe(),
                ProvidersName::Default,
            ))
        } else {
            None
        })
    }

    fn unpack_iterable<'v>(
        value: Value<'v>,
        ctx: &BxlContext<'_>,
        eval: &Evaluator<'v, '_>,
        unpack_literal: impl for<'a> Fn(
            Value<'v>,
            &BxlContext<'a>,
        ) -> anyhow::Result<Option<ProvidersExpr<P>>>,
    ) -> anyhow::Result<Option<ProvidersExpr<P>>> {
        Ok(Some(Self::Iterable(
            #[allow(clippy::manual_map)] // `if else if` looks better here
            if let Some(s) = value.downcast_ref::<StarlarkTargetSet<TargetNode>>() {
                Some(Either::Left(Either::Left(s.iter(eval.heap()))))
            } else if let Some(s) = value.downcast_ref::<StarlarkTargetSet<ConfiguredTargetNode>>()
            {
                Some(Either::Left(Either::Right(s.iter(eval.heap()))))
            } else if let Some(iterable) = ListRef::from_value(value) {
                Some(Either::Right(iterable.iter()))
            } else {
                None
            }
            .ok_or_else(|| ProviderExprError::NotATarget(value.to_repr()))?
            .map(|val| {
                if let Some(ProvidersExpr::Literal(resolved_val)) = unpack_literal(val, ctx)? {
                    Ok(resolved_val)
                } else {
                    Err(anyhow::anyhow!(ProviderExprError::NotATarget(
                        val.to_repr()
                    )))
                }
            })
            .collect::<anyhow::Result<_>>()?,
        )))
    }
}
