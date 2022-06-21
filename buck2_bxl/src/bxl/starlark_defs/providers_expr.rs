use buck2_build_api::calculation::Calculation;
use buck2_build_api::nodes::configured::ConfiguredTargetNode;
use buck2_build_api::nodes::unconfigured::TargetNode;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::package::Package;
use buck2_core::provider::ConfiguredProvidersLabel;
use buck2_core::provider::ProvidersLabel;
use buck2_core::provider::ProvidersName;
use buck2_core::target::TargetLabel;
use buck2_interpreter::pattern::ParsedPattern;
use buck2_interpreter::pattern::ProvidersPattern;
use buck2_interpreter::types::label::Label;
use buck2_interpreter::types::label::StarlarkProvidersLabel;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use gazebo::dupe::Dupe;
use itertools::Either;
use starlark::eval::Evaluator;
use starlark::values::list::List;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use thiserror::Error;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::nodes::configured::StarlarkConfiguredTargetNode;
use crate::bxl::starlark_defs::nodes::unconfigured::StarlarkTargetNode;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;
use crate::bxl::value_as_starlak_target_label::ValueAsStarlarkTargetLabel;

#[derive(Debug, Error)]
enum ProviderExprError {
    #[error("Expected a list of target like items, but was `{0}`")]
    NotAListOfTargets(String),
    #[error("Expected a single target like ite, but was `{0}`")]
    NotATarget(String),
}

/// ProvidersExpr is just a simple type that can be used in starlark_module
/// functions for arguments that should be a set of provider labels. It will accept a
/// literal (like `//some:target[subtarget]`) or list of literals or a single provider label
pub(crate) enum ProvidersExpr {
    Literal(ConfiguredProvidersLabel),
    Iterable(Vec<ConfiguredProvidersLabel>),
}

impl ProvidersExpr {
    pub fn unpack<'v>(
        value: Value<'v>,
        target_platform: Value<'v>,
        ctx: &BxlContext,
        eval: &Evaluator<'v, '_>,
    ) -> anyhow::Result<Self> {
        let target_platform =
            target_platform.parse_target_platforms(&ctx.target_alias_resolver, &ctx.cell)?;

        Ok(
            if let Some(resolved) = Self::unpack_literal(value, &target_platform, ctx)? {
                resolved
            } else if let Some(resolved) =
                Self::unpack_iterable(value, &target_platform, ctx, eval)?
            {
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
                configured_target.0.name().dupe(),
                ProvidersName::Default,
            ))))
        } else if let Some(configured_target) = value.downcast_ref::<Label>() {
            Ok(Some(Self::Literal(configured_target.label().clone())))
            // TODO one more case here to handle configured target label
        } else {
            #[allow(clippy::manual_map)] // `if else if` looks better here
            if let Some(s) = value.unpack_str() {
                Some(
                    ParsedPattern::<ProvidersPattern>::parse_relaxed(
                        &ctx.target_alias_resolver,
                        ctx.cell.cell_alias_resolver(),
                        &Package::new(ctx.cell.name(), CellRelativePath::unchecked_new("")),
                        s,
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
            }
            .map_or(Ok(None), |label| {
                let result: anyhow::Result<_> = try {
                    Self::Literal(ctx.async_ctx.via_dice(|ctx| {
                        ctx.get_configured_target(&label, target_platform.as_ref())
                    })?)
                };
                result.map(Some)
            })
        }
    }

    fn unpack_iterable<'v>(
        value: Value<'v>,
        target_platform: &Option<TargetLabel>,
        ctx: &BxlContext,
        eval: &Evaluator<'v, '_>,
    ) -> anyhow::Result<Option<Self>> {
        Ok(Some(Self::Iterable(
            #[allow(clippy::manual_map)] // `if else if` looks better here
            if let Some(s) = value.downcast_ref::<StarlarkTargetSet<TargetNode>>() {
                Some(Either::Left(s.iterate(eval.heap())?))
            } else if let Some(s) = value.downcast_ref::<StarlarkTargetSet<ConfiguredTargetNode>>()
            {
                Some(Either::Left(s.iterate(eval.heap())?))
            } else if let Some(iterable) = List::from_value(value) {
                Some(Either::Right(iterable.iter()))
            } else {
                None
            }
            .ok_or_else(|| ProviderExprError::NotATarget(value.to_repr()))?
            .map(|val| {
                if let Some(ProvidersExpr::Literal(resolved_val)) =
                    Self::unpack_literal(val, target_platform, ctx)?
                {
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

    pub fn labels(&self) -> impl Iterator<Item = &ConfiguredProvidersLabel> {
        match &self {
            ProvidersExpr::Literal(item) => itertools::Either::Left(std::iter::once(item)),
            ProvidersExpr::Iterable(iter) => itertools::Either::Right(iter.iter()),
        }
    }
}
