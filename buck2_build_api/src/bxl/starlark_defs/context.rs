/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! The context containing the available buck commands and query operations for `bxl` functions.
//!

use std::sync::Arc;

use buck2_common::{
    dice::{cells::HasCellResolver, data::HasIoProvider},
    package_boundary::HasPackageBoundaryExceptions,
    target_aliases::{HasTargetAliasResolver, TargetAliasResolver},
};
use buck2_core::{
    cells::CellInstance,
    fs::{paths::AbsPathBuf, project::ProjectFilesystem},
    target::TargetLabel,
};
use derivative::Derivative;
use derive_more::Display;
use dice::DiceComputations;
use gazebo::any::AnyLifetime;
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    values::{
        dict::Dict, none::NoneType, AllocValue, Freeze, Freezer, Heap, NoSerialize, NoSimpleValue,
        StarlarkValue, Trace, UnpackValue, Value, ValueLike,
    },
};

use crate::{
    bxl::{
        starlark_defs::{
            analysis_result::StarlarkAnalysisResult,
            context::{
                actions::BxlActionsCtx, fs::BxlFilesystem, starlark_async::BxlSafeDiceComputations,
            },
            cquery::StarlarkCQueryCtx,
            providers_expr::ProvidersExpr,
            uquery::StarlarkUQueryCtx,
            BxlError,
        },
        BxlKey,
    },
    query::dice::DiceQueryDelegate,
};

pub mod actions;
pub mod analyze;
pub mod build;
pub mod fs;
pub mod starlark_async;

#[derive(AnyLifetime, Derivative, Display, Trace, NoSerialize)]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub struct BxlContext<'v> {
    #[trace(unsafe_ignore)]
    pub(crate) current_bxl: BxlKey,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    pub(crate) target_alias_resolver: TargetAliasResolver,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    pub(crate) cell: CellInstance,
    cli_args: Value<'v>, // Struct of the cli args
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    pub(crate) async_ctx: BxlSafeDiceComputations<'v>,
}

impl<'v> BxlContext<'v> {
    pub fn new(
        current_bxl: BxlKey,
        cli_args: Value<'v>,
        target_alias_resolver: TargetAliasResolver,
        cell: CellInstance,
        async_ctx: BxlSafeDiceComputations<'v>,
    ) -> Self {
        Self {
            current_bxl,
            target_alias_resolver,
            cell,
            cli_args,
            async_ctx,
        }
    }

    pub(crate) async fn dice_query_delegate(
        ctx: &DiceComputations,
        global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<DiceQueryDelegate<'_>> {
        let cwd = AbsPathBuf::try_from(std::env::current_dir()?)?;
        let working_dir = {
            let fs = ProjectFilesystem::new(cwd.clone());
            fs.relativize(&cwd)?.as_ref().to_owned()
        };
        let cell_resolver = ctx.get_cell_resolver().await;
        let project_root = cwd;
        let package_boundary_exceptions = ctx.get_package_boundary_exceptions().await?;
        let target_alias_resolver = ctx
            .target_alias_resolver_for_working_dir(&working_dir)
            .await?;
        DiceQueryDelegate::new(
            ctx,
            working_dir,
            project_root,
            cell_resolver,
            global_target_platform,
            package_boundary_exceptions,
            target_alias_resolver,
        )
    }

    pub(crate) fn sync_dice_query_delegate(
        &self,
        global_target_platform: Option<TargetLabel>,
    ) -> anyhow::Result<DiceQueryDelegate<'_>> {
        self.async_ctx
            .via_dice(|dice| Self::dice_query_delegate(dice, global_target_platform))
    }
}

impl<'v> StarlarkValue<'v> for BxlContext<'v> {
    starlark_type!("context");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(register_context)
    }
}

impl<'v> Freeze for BxlContext<'v> {
    type Frozen = NoSimpleValue;
    fn freeze(self, _freezer: &Freezer) -> anyhow::Result<Self::Frozen> {
        Err(BxlError::NoFreeze("BxlContext").into())
    }
}

impl<'v> AllocValue<'v> for BxlContext<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex(self)
    }
}

impl<'v> UnpackValue<'v> for &'v BxlContext<'v> {
    fn expected() -> String {
        BxlContext::get_type_value_static().as_str().to_owned()
    }

    fn unpack_value(x: Value<'v>) -> Option<&'v BxlContext<'v>> {
        x.downcast_ref()
    }
}

#[starlark_module]
fn register_context(builder: &mut MethodsBuilder) {
    /// Returns the absolute path to the root of the repository
    fn root(this: &BxlContext) -> anyhow::Result<String> {
        Ok(this
            .async_ctx
            .0
            .global_data()
            .get_io_provider()
            .fs()
            .root
            .to_str()
            .ok_or_else(|| anyhow::anyhow!("Non utf-8 path"))?
            .to_owned())
    }

    #[starlark(attribute)]
    fn uquery<'v>(this: &BxlContext) -> anyhow::Result<StarlarkUQueryCtx<'v>> {
        let delegate = this.sync_dice_query_delegate(None)?;
        StarlarkUQueryCtx::new(Arc::new(delegate))
    }

    fn cquery<'v>(
        this: &BxlContext,
        // TODO(brasselsprouts): I would like to strongly type this.
        global_target_platform @ NoneType: Value,
    ) -> anyhow::Result<StarlarkCQueryCtx<'v>> {
        this.async_ctx
            .via(|| StarlarkCQueryCtx::new(this, global_target_platform))
    }

    #[starlark(attribute)]
    fn bxl_actions<'v>(this: &BxlContext) -> anyhow::Result<BxlActionsCtx<'v>> {
        Ok(BxlActionsCtx::new(&this.async_ctx))
    }

    fn analysis<'v>(
        this: &BxlContext,
        labels: Value<'v>,
        global_target_platform @ NoneType: Value<'v>,
        skip_incompatible @ true: bool,
    ) -> anyhow::Result<Vec<StarlarkAnalysisResult>> {
        let providers = ProvidersExpr::unpack(labels, global_target_platform, this, eval)?;

        let res: anyhow::Result<_> = this.async_ctx.via_dice(|ctx| async {
            analyze::analyze(ctx, providers.labels(), skip_incompatible).await
        });

        res
    }

    fn build<'v>(
        this: &BxlContext,
        spec: Value<'v>,
        target_platform @ NoneType: Value<'v>,
    ) -> anyhow::Result<Value<'v>> {
        Ok(eval
            .heap()
            .alloc(Dict::new(build::build(this, spec, target_platform, eval)?)))
    }

    #[starlark(attribute)]
    fn cli_args<'v>(this: &BxlContext) -> anyhow::Result<Value<'v>> {
        Ok(this.cli_args)
    }

    #[starlark(attribute)]
    fn unstable_fs<'v>(this: &BxlContext) -> anyhow::Result<BxlFilesystem<'v>> {
        Ok(BxlFilesystem::new(&this.async_ctx))
    }
}
