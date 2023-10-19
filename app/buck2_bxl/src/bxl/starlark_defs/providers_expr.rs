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
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use buck2_interpreter::types::configured_providers_label::StarlarkProvidersLabel;
use buck2_interpreter::types::target_label::StarlarkConfiguredTargetLabel;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_node::target_calculation::ConfiguredTargetCalculation;
use dice::DiceComputations;
use dupe::Dupe;
use futures::FutureExt;
use itertools::Either;
use starlark::eval::Evaluator;
use starlark::values::list::ListRef;
use starlark::values::list::UnpackList;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use thiserror::Error;

use crate::bxl::starlark_defs::context::BxlContextNoDice;
use crate::bxl::starlark_defs::nodes::configured::StarlarkConfiguredTargetNode;
use crate::bxl::starlark_defs::nodes::unconfigured::StarlarkTargetNode;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;

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

#[derive(StarlarkTypeRepr, UnpackValue)]
enum ProvidersLabelArg<'v> {
    Str(&'v str),
    StarlarkTargetLabel(&'v StarlarkTargetLabel),
    StarlarkProvidersLabel(&'v StarlarkProvidersLabel),
    StarlarkTargetNode(&'v StarlarkTargetNode),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
enum ConfiguredProvidersLabelArg<'v> {
    ConfiguredTargetNode(&'v StarlarkConfiguredTargetNode),
    ConfiguredTargetLabel(&'v StarlarkConfiguredTargetLabel),
    ConfiguredProvidersLabel(&'v StarlarkConfiguredProvidersLabel),
    Unconfigured(ProvidersLabelArg<'v>),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
enum ProviderLabelListArg<'v> {
    List(UnpackList<ProvidersLabelArg<'v>>),
    TargetSet(&'v StarlarkTargetSet<TargetNode>),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
enum ProviderExprArg<'v> {
    One(ProvidersLabelArg<'v>),
    List(ProviderLabelListArg<'v>),
}

impl ProvidersExpr<ConfiguredProvidersLabel> {
    pub(crate) async fn unpack_opt<'v, 'c>(
        value: Value<'v>,
        target_platform: Option<TargetLabel>,
        ctx: &BxlContextNoDice<'_>,
        dice: &'c DiceComputations,
        eval: &Evaluator<'v, '_>,
    ) -> anyhow::Result<Option<Self>> {
        Ok(
            if let Some(arg) = ConfiguredProvidersLabelArg::unpack_value(value) {
                Some(Self::unpack_literal(arg, &target_platform, ctx, dice).await?)
            } else {
                Self::unpack_iterable(value, &target_platform, ctx, dice, eval).await?
            },
        )
    }

    pub(crate) async fn unpack<'v, 'c>(
        value: Value<'v>,
        target_platform: Option<TargetLabel>,
        ctx: &BxlContextNoDice<'_>,
        dice: &'c DiceComputations,
        eval: &Evaluator<'v, '_>,
    ) -> anyhow::Result<Self> {
        Ok(
            if let Some(resolved) =
                Self::unpack_opt(value, target_platform, ctx, dice, eval).await?
            {
                resolved
            } else {
                return Err(anyhow::anyhow!(ProviderExprError::NotAListOfTargets(
                    value.to_repr()
                )));
            },
        )
    }

    async fn unpack_literal<'v, 'c>(
        arg: ConfiguredProvidersLabelArg<'v>,
        target_platform: &'c Option<TargetLabel>,
        ctx: &BxlContextNoDice<'_>,
        dice: &'c DiceComputations,
    ) -> anyhow::Result<Self> {
        match arg {
            ConfiguredProvidersLabelArg::ConfiguredTargetNode(configured_target) => {
                Ok(Self::Literal(ConfiguredProvidersLabel::new(
                    configured_target.0.label().dupe(),
                    ProvidersName::Default,
                )))
            }
            ConfiguredProvidersLabelArg::ConfiguredTargetLabel(configured_target) => {
                Ok(Self::Literal(ConfiguredProvidersLabel::new(
                    configured_target.label().dupe(),
                    ProvidersName::Default,
                )))
            }
            ConfiguredProvidersLabelArg::ConfiguredProvidersLabel(configured_target) => {
                Ok(Self::Literal(configured_target.label().clone()))
            }
            ConfiguredProvidersLabelArg::Unconfigured(arg) => {
                let label = Self::unpack_providers_label(arg, ctx)?;
                dice.get_configured_provider_label(&label, target_platform.as_ref())
                    .map(|res| res.map(Self::Literal))
                    .await
            }
        }
    }

    async fn unpack_iterable<'c, 'v: 'c>(
        value: Value<'v>,
        target_platform: &'c Option<TargetLabel>,
        ctx: &'c BxlContextNoDice<'_>,
        dice: &'c DiceComputations,
        eval: &Evaluator<'v, '_>,
    ) -> anyhow::Result<Option<ProvidersExpr<ConfiguredProvidersLabel>>> {
        #[allow(clippy::manual_map)] // `if else if` looks better here
        let iterable = if let Some(s) = value.downcast_ref::<StarlarkTargetSet<TargetNode>>() {
            Either::Left(Either::Left(s.iter(eval.heap())))
        } else if let Some(s) = value.downcast_ref::<StarlarkTargetSet<ConfiguredTargetNode>>() {
            Either::Left(Either::Right(s.iter(eval.heap())))
        } else if let Some(iterable) = ListRef::from_value(value) {
            Either::Right(iterable.iter())
        } else {
            return Err(ProviderExprError::NotATarget(value.to_repr()).into());
        };

        let mut res = Vec::new();
        for val in iterable {
            let Some(arg) = ConfiguredProvidersLabelArg::unpack_value(val) else {
                return Err(anyhow::anyhow!(ProviderExprError::NotATarget(
                    val.to_repr()
                )));
            };
            if let ProvidersExpr::Literal(resolved_val) =
                Self::unpack_literal(arg, target_platform, ctx, dice).await?
            {
                res.push(resolved_val)
            } else {
                return Err(anyhow::anyhow!(ProviderExprError::NotATarget(
                    val.to_repr()
                )));
            }
        }

        Ok(Some(Self::Iterable(res)))
    }
}

impl ProvidersExpr<ProvidersLabel> {
    pub(crate) async fn unpack<'v>(
        value: Value<'v>,
        ctx: &BxlContextNoDice<'_>,
    ) -> anyhow::Result<Self> {
        let Some(arg) = ProviderExprArg::unpack_value(value) else {
            return Err(anyhow::anyhow!(ProviderExprError::NotAListOfTargets(
                value.to_repr()
            )));
        };
        match arg {
            ProviderExprArg::One(arg) => Self::unpack_literal(arg, ctx),
            ProviderExprArg::List(arg) => Self::unpack_iterable(arg, ctx).await,
        }
    }

    fn unpack_literal<'v>(
        value: ProvidersLabelArg<'v>,
        ctx: &BxlContextNoDice<'_>,
    ) -> anyhow::Result<Self> {
        Ok(Self::Literal(Self::unpack_providers_label(value, ctx)?))
    }

    async fn unpack_iterable<'c, 'v: 'c>(
        arg: ProviderLabelListArg<'v>,
        ctx: &'c BxlContextNoDice<'_>,
    ) -> anyhow::Result<ProvidersExpr<ProvidersLabel>> {
        match arg {
            ProviderLabelListArg::TargetSet(s) => Ok(ProvidersExpr::Iterable(
                s.0.iter()
                    .map(|node| ProvidersLabel::default_for(node.label().dupe()))
                    .collect(),
            )),
            ProviderLabelListArg::List(iterable) => {
                let mut res = Vec::new();
                for val in iterable.items {
                    res.push(Self::unpack_providers_label(val, ctx)?)
                }
                Ok(ProvidersExpr::Iterable(res))
            }
        }
    }
}

impl<P: ProvidersLabelMaybeConfigured> ProvidersExpr<P> {
    pub(crate) fn labels(&self) -> impl Iterator<Item = &P> {
        match &self {
            ProvidersExpr::Literal(item) => Either::Left(std::iter::once(item)),
            ProvidersExpr::Iterable(iter) => Either::Right(iter.iter()),
        }
    }

    fn unpack_providers_label<'v>(
        arg: ProvidersLabelArg<'v>,
        ctx: &BxlContextNoDice<'_>,
    ) -> anyhow::Result<ProvidersLabel> {
        match arg {
            ProvidersLabelArg::Str(s) => {
                Ok(ParsedPattern::<ProvidersPatternExtra>::parse_relaxed(
                    &ctx.target_alias_resolver,
                    // TODO(nga): Parse relaxed relative to cell root is incorrect.
                    CellPathRef::new(ctx.cell_name, CellRelativePath::empty()),
                    s,
                    &ctx.cell_resolver,
                )?
                .as_providers_label(s)?)
            }
            ProvidersLabelArg::StarlarkTargetLabel(target) => Ok(ProvidersLabel::new(
                target.label().dupe(),
                ProvidersName::Default,
            )),
            ProvidersLabelArg::StarlarkProvidersLabel(label) => Ok(label.label().clone()),
            ProvidersLabelArg::StarlarkTargetNode(node) => Ok(ProvidersLabel::new(
                node.0.label().dupe(),
                ProvidersName::Default,
            )),
        }
    }
}
