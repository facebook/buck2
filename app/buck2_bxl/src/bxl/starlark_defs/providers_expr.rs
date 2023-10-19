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
use futures::future;
use itertools::Either;
use starlark::values::list::UnpackList;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::UnpackValue;

use crate::bxl::starlark_defs::context::BxlContextNoDice;
use crate::bxl::starlark_defs::nodes::configured::StarlarkConfiguredTargetNode;
use crate::bxl::starlark_defs::nodes::unconfigured::StarlarkTargetNode;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;

/// ProvidersExpr is just a simple type that can be used in starlark_module
/// functions for arguments that should be a set of provider labels. It will accept a
/// literal (like `//some:target[subtarget]`) or list of literals or a single provider label
pub(crate) enum ProvidersExpr<P: ProvidersLabelMaybeConfigured> {
    Literal(P),
    Iterable(Vec<P>),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum ProvidersLabelArg<'v> {
    Str(&'v str),
    StarlarkTargetLabel(&'v StarlarkTargetLabel),
    StarlarkProvidersLabel(&'v StarlarkProvidersLabel),
    StarlarkTargetNode(&'v StarlarkTargetNode),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum ConfiguredProvidersLabelArg<'v> {
    ConfiguredTargetNode(&'v StarlarkConfiguredTargetNode),
    ConfiguredTargetLabel(&'v StarlarkConfiguredTargetLabel),
    ConfiguredProvidersLabel(&'v StarlarkConfiguredProvidersLabel),
    Unconfigured(ProvidersLabelArg<'v>),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum ProviderLabelListArg<'v> {
    List(UnpackList<ProvidersLabelArg<'v>>),
    TargetSet(&'v StarlarkTargetSet<TargetNode>),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum ProviderExprArg<'v> {
    One(ProvidersLabelArg<'v>),
    List(ProviderLabelListArg<'v>),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum ConfiguredProvidersLabelListArg<'v> {
    StarlarkTargetSet(&'v StarlarkTargetSet<TargetNode>),
    StarlarkConfiguredTargetSet(&'v StarlarkTargetSet<ConfiguredTargetNode>),
    List(UnpackList<ConfiguredProvidersLabelArg<'v>>),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum ConfiguredProvidersExprArg<'v> {
    One(ConfiguredProvidersLabelArg<'v>),
    List(ConfiguredProvidersLabelListArg<'v>),
}

impl ProvidersExpr<ConfiguredProvidersLabel> {
    pub(crate) async fn unpack<'v, 'c>(
        arg: ConfiguredProvidersExprArg<'v>,
        target_platform: Option<TargetLabel>,
        ctx: &BxlContextNoDice<'_>,
        dice: &'c DiceComputations,
    ) -> anyhow::Result<Self> {
        match arg {
            ConfiguredProvidersExprArg::One(arg) => Ok(ProvidersExpr::Literal(
                Self::unpack_literal(arg, &target_platform, ctx, dice).await?,
            )),
            ConfiguredProvidersExprArg::List(arg) => {
                Ok(Self::unpack_iterable(arg, &target_platform, ctx, dice).await?)
            }
        }
    }

    async fn unpack_literal<'v, 'c>(
        arg: ConfiguredProvidersLabelArg<'v>,
        target_platform: &'c Option<TargetLabel>,
        ctx: &BxlContextNoDice<'_>,
        dice: &'c DiceComputations,
    ) -> anyhow::Result<ConfiguredProvidersLabel> {
        match arg {
            ConfiguredProvidersLabelArg::ConfiguredTargetNode(configured_target) => {
                Ok(ConfiguredProvidersLabel::new(
                    configured_target.0.label().dupe(),
                    ProvidersName::Default,
                ))
            }
            ConfiguredProvidersLabelArg::ConfiguredTargetLabel(configured_target) => {
                Ok(ConfiguredProvidersLabel::new(
                    configured_target.label().dupe(),
                    ProvidersName::Default,
                ))
            }
            ConfiguredProvidersLabelArg::ConfiguredProvidersLabel(configured_target) => {
                Ok(configured_target.label().clone())
            }
            ConfiguredProvidersLabelArg::Unconfigured(arg) => {
                let label = Self::unpack_providers_label(arg, ctx)?;
                dice.get_configured_provider_label(&label, target_platform.as_ref())
                    .await
            }
        }
    }

    async fn unpack_iterable<'c, 'v: 'c>(
        arg: ConfiguredProvidersLabelListArg<'v>,
        target_platform: &'c Option<TargetLabel>,
        ctx: &'c BxlContextNoDice<'_>,
        dice: &'c DiceComputations,
    ) -> anyhow::Result<ProvidersExpr<ConfiguredProvidersLabel>> {
        match arg {
            ConfiguredProvidersLabelListArg::StarlarkTargetSet(s) => Ok(ProvidersExpr::Iterable(
                future::try_join_all(s.0.iter().map(|node| async {
                    let providers_label = ProvidersLabel::default_for(node.label().dupe());
                    dice.get_configured_provider_label(&providers_label, target_platform.as_ref())
                        .await
                }))
                .await?,
            )),
            ConfiguredProvidersLabelListArg::StarlarkConfiguredTargetSet(s) => {
                Ok(ProvidersExpr::Iterable(
                    s.0.iter()
                        .map(|node| ConfiguredProvidersLabel::default_for(node.label().dupe()))
                        .collect(),
                ))
            }
            ConfiguredProvidersLabelListArg::List(iterable) => {
                let mut res = Vec::new();
                for arg in iterable.items {
                    res.push(Self::unpack_literal(arg, target_platform, ctx, dice).await?);
                }

                Ok(Self::Iterable(res))
            }
        }
    }
}

impl ProvidersExpr<ProvidersLabel> {
    pub(crate) fn unpack<'v>(
        arg: ProviderExprArg<'v>,
        ctx: &BxlContextNoDice<'_>,
    ) -> anyhow::Result<Self> {
        match arg {
            ProviderExprArg::One(arg) => Self::unpack_literal(arg, ctx),
            ProviderExprArg::List(arg) => Self::unpack_iterable(arg, ctx),
        }
    }

    fn unpack_literal<'v>(
        value: ProvidersLabelArg<'v>,
        ctx: &BxlContextNoDice<'_>,
    ) -> anyhow::Result<Self> {
        Ok(Self::Literal(Self::unpack_providers_label(value, ctx)?))
    }

    fn unpack_iterable<'c, 'v: 'c>(
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
