//! Common utilities for bxl

use std::sync::Arc;

use buck2_common::target_aliases::TargetAliasResolver;
use buck2_core::{
    cells::{paths::CellRelativePath, CellAliasResolver, CellInstance},
    configuration::Configuration,
    package::Package,
    target::TargetLabel,
};
use buck2_interpreter::pattern::{ParsedPattern, TargetPattern};
use gazebo::dupe::Dupe;
use once_cell::sync::Lazy;
use starlark::values::{Value, ValueLike};

use crate::{
    bxl::starlark_defs::providers_expr::ProviderExprError,
    configuration::execution::{ExecutionPlatform, ExecutionPlatformResolution},
    execute::{CommandExecutorConfig, LocalExecutorOptions},
    interpreter::rule_defs::target_label::StarlarkTargetLabel,
};

// TODO(bobyf) this should be configured by the bxl function similar to
// execution groups, which we don't have yet. Currently we hard code this to
// local since IDE will mostly be invoking local tools anyways
pub(crate) static EXECUTION_PLATFORM: Lazy<ExecutionPlatformResolution> = Lazy::new(|| {
    ExecutionPlatformResolution::new(
        Some(Arc::new(ExecutionPlatform::LegacyExecutionPlatform {
            executor_config: CommandExecutorConfig::Local(LocalExecutorOptions {}),
            cfg: Configuration::unspecified(),
        })),
        vec![],
    )
});

pub struct CliResolutionCtx {
    pub target_alias_resolver: TargetAliasResolver,
    pub cell_resolver: CellAliasResolver,
    pub relative_dir: Package,
}

pub(crate) trait ValueAsStarlarkTargetLabel {
    fn parse_target_platforms(
        self,
        target_alias_resolver: &TargetAliasResolver,
        cell: &CellInstance,
    ) -> anyhow::Result<Option<TargetLabel>>;
}

impl<'v> ValueAsStarlarkTargetLabel for Value<'v> {
    fn parse_target_platforms(
        self,
        target_alias_resolver: &TargetAliasResolver,
        cell: &CellInstance,
    ) -> anyhow::Result<Option<TargetLabel>> {
        let target_platform = if self.is_none() {
            None
        } else if let Some(s) = self.unpack_str() {
            Some(
                ParsedPattern::<TargetPattern>::parse_relaxed(
                    target_alias_resolver,
                    cell.cell_alias_resolver(),
                    &Package::new(cell.name(), CellRelativePath::unchecked_new("")),
                    s,
                )?
                .as_target_label(s)?,
            )
        } else if let Some(target) = self.downcast_ref::<StarlarkTargetLabel>() {
            Some(target.label().dupe())
        } else {
            return Err(anyhow::anyhow!(ProviderExprError::NotATarget(
                self.to_repr()
            )));
        };

        Ok(target_platform)
    }
}
