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
use buck2_core::global_cfg_options::GlobalCfgOptions;
use buck2_core::pattern::pattern::ParsedPattern;
use buck2_core::pattern::pattern_type::ProvidersPatternExtra;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::provider::label::ProvidersLabel;
use buck2_core::provider::label::ProvidersLabelMaybeConfigured;
use buck2_core::provider::label::ProvidersName;
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

/// ProvidersLabelArg is a type that can be used as an argument in starlark api for
/// an unconfigured provider label
#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum ProvidersLabelArg<'v> {
    Str(&'v str),
    StarlarkTargetLabel(&'v StarlarkTargetLabel),
    StarlarkProvidersLabel(&'v StarlarkProvidersLabel),
    StarlarkTargetNode(&'v StarlarkTargetNode),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum ProvidersLabelListArg<'v> {
    List(UnpackList<ProvidersLabelArg<'v>>),
    TargetSet(&'v StarlarkTargetSet<TargetNode>),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum ProvidersExprArg<'v> {
    One(ProvidersLabelArg<'v>),
    List(ProvidersLabelListArg<'v>),
}

/// ConfiguredProvidersLabelArg is a type that can be used as an argument in starlark api for
/// a configured provider label
#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum ConfiguredProvidersLabelArg<'v> {
    Node(&'v StarlarkConfiguredTargetNode),
    Label(&'v StarlarkConfiguredTargetLabel),
    ProvidersLabel(&'v StarlarkConfiguredProvidersLabel),
}

/// AnyProvidersLabelArg is a type that can be used as an argument in stalark api for
/// a configured provider label or an unconfigured provider label
#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum AnyProvidersLabelArg<'v> {
    Configured(ConfiguredProvidersLabelArg<'v>),
    Unconfigured(ProvidersLabelArg<'v>),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum AnyProvidersLabelListArg<'v> {
    StarlarkTargetSet(&'v StarlarkTargetSet<TargetNode>),
    StarlarkConfiguredTargetSet(&'v StarlarkTargetSet<ConfiguredTargetNode>),
    List(UnpackList<AnyProvidersLabelArg<'v>>),
}

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum AnyProvidersExprArg<'v> {
    One(AnyProvidersLabelArg<'v>),
    List(AnyProvidersLabelListArg<'v>),
}

impl<'v> ConfiguredProvidersLabelArg<'v> {
    pub(crate) fn configured_providers_label(&self) -> ConfiguredProvidersLabel {
        match self {
            ConfiguredProvidersLabelArg::Node(node) => {
                ConfiguredProvidersLabel::default_for(node.0.label().dupe())
            }
            ConfiguredProvidersLabelArg::Label(label) => {
                ConfiguredProvidersLabel::default_for(label.label().dupe())
            }
            ConfiguredProvidersLabelArg::ProvidersLabel(providers_label) => {
                providers_label.label().dupe()
            }
        }
    }
}

impl<'v> AnyProvidersExprArg<'v> {
    pub(crate) fn contains_unconfigured(&self) -> bool {
        match self {
            AnyProvidersExprArg::One(arg) => arg.is_unconfigured(),
            AnyProvidersExprArg::List(arg) => arg.contains_unconfigured(),
        }
    }
}

impl<'v> AnyProvidersLabelArg<'v> {
    fn is_unconfigured(&self) -> bool {
        matches!(self, AnyProvidersLabelArg::Unconfigured(_))
    }
}

impl<'v> AnyProvidersLabelListArg<'v> {
    fn contains_unconfigured(&self) -> bool {
        match self {
            AnyProvidersLabelListArg::List(args) => {
                args.items.iter().any(|arg| arg.is_unconfigured())
            }
            AnyProvidersLabelListArg::StarlarkTargetSet(_) => true,
            _ => false,
        }
    }
}

impl ProvidersExpr<ConfiguredProvidersLabel> {
    pub(crate) async fn unpack<'v, 'c>(
        arg: AnyProvidersExprArg<'v>,
        global_cfg_options_override: &GlobalCfgOptions,
        ctx: &BxlContextNoDice<'_>,
        dice: &'c mut DiceComputations<'_>,
    ) -> buck2_error::Result<Self> {
        match arg {
            AnyProvidersExprArg::One(arg) => Ok(ProvidersExpr::Literal(
                Self::unpack_literal(arg, global_cfg_options_override, ctx, dice).await?,
            )),
            AnyProvidersExprArg::List(arg) => {
                Ok(Self::unpack_iterable(arg, global_cfg_options_override, ctx, dice).await?)
            }
        }
    }

    async fn unpack_literal<'v, 'c>(
        arg: AnyProvidersLabelArg<'v>,
        global_cfg_options_override: &'c GlobalCfgOptions,
        ctx: &BxlContextNoDice<'_>,
        dice: &'c mut DiceComputations<'_>,
    ) -> buck2_error::Result<ConfiguredProvidersLabel> {
        match arg {
            AnyProvidersLabelArg::Configured(arg) => Ok(arg.configured_providers_label()),
            AnyProvidersLabelArg::Unconfigured(arg) => {
                let label = Self::unpack_providers_label(arg, ctx)?;
                Ok(dice
                    .get_configured_provider_label(&label, global_cfg_options_override)
                    .await?)
            }
        }
    }

    async fn unpack_iterable<'c, 'v: 'c>(
        arg: AnyProvidersLabelListArg<'v>,
        global_cfg_options_override: &'c GlobalCfgOptions,
        ctx: &'c BxlContextNoDice<'_>,
        dice: &'c mut DiceComputations<'_>,
    ) -> buck2_error::Result<ProvidersExpr<ConfiguredProvidersLabel>> {
        match arg {
            AnyProvidersLabelListArg::StarlarkTargetSet(s) => Ok(ProvidersExpr::Iterable(
                dice.try_compute_join(s.0.iter(), |dice, node| {
                    async move {
                        let providers_label = ProvidersLabel::default_for(node.label().dupe());
                        dice.get_configured_provider_label(
                            &providers_label,
                            global_cfg_options_override,
                        )
                        .await
                    }
                    .boxed()
                })
                .await?,
            )),
            AnyProvidersLabelListArg::StarlarkConfiguredTargetSet(s) => {
                Ok(ProvidersExpr::Iterable(
                    s.0.iter()
                        .map(|node| ConfiguredProvidersLabel::default_for(node.label().dupe()))
                        .collect(),
                ))
            }
            AnyProvidersLabelListArg::List(iterable) => {
                let mut res = Vec::new();
                for arg in iterable.items {
                    res.push(
                        Self::unpack_literal(arg, global_cfg_options_override, ctx, dice).await?,
                    );
                }

                Ok(Self::Iterable(res))
            }
        }
    }
}

impl ProvidersExpr<ProvidersLabel> {
    pub(crate) fn unpack<'v>(
        arg: ProvidersExprArg<'v>,
        ctx: &BxlContextNoDice<'_>,
    ) -> buck2_error::Result<Self> {
        match arg {
            ProvidersExprArg::One(arg) => Self::unpack_literal(arg, ctx),
            ProvidersExprArg::List(arg) => Self::unpack_iterable(arg, ctx),
        }
    }

    fn unpack_literal<'v>(
        value: ProvidersLabelArg<'v>,
        ctx: &BxlContextNoDice<'_>,
    ) -> buck2_error::Result<Self> {
        Ok(Self::Literal(Self::unpack_providers_label(value, ctx)?))
    }

    fn unpack_iterable<'c, 'v: 'c>(
        arg: ProvidersLabelListArg<'v>,
        ctx: &'c BxlContextNoDice<'_>,
    ) -> buck2_error::Result<ProvidersExpr<ProvidersLabel>> {
        match arg {
            ProvidersLabelListArg::TargetSet(s) => Ok(ProvidersExpr::Iterable(
                s.0.iter()
                    .map(|node| ProvidersLabel::default_for(node.label().dupe()))
                    .collect(),
            )),
            ProvidersLabelListArg::List(iterable) => {
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
    ) -> buck2_error::Result<ProvidersLabel> {
        match arg {
            ProvidersLabelArg::Str(s) => {
                Ok(ParsedPattern::<ProvidersPatternExtra>::parse_relaxed(
                    ctx.target_alias_resolver(),
                    // TODO(nga): Parse relaxed relative to cell root is incorrect.
                    CellPathRef::new(ctx.cell_name(), CellRelativePath::empty()),
                    s,
                    ctx.cell_resolver(),
                    ctx.cell_alias_resolver(),
                )?
                .as_providers_label(s)?)
            }
            ProvidersLabelArg::StarlarkTargetLabel(target) => Ok(ProvidersLabel::new(
                target.label().dupe(),
                ProvidersName::Default,
            )),
            ProvidersLabelArg::StarlarkProvidersLabel(label) => Ok(label.label().dupe()),
            ProvidersLabelArg::StarlarkTargetNode(node) => Ok(ProvidersLabel::new(
                node.0.label().dupe(),
                ProvidersName::Default,
            )),
        }
    }
}
