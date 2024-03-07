/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use anyhow::Context as _;
use buck2_build_api::audit_cell::audit_cell;
use buck2_build_api::audit_output::audit_output;
use buck2_build_api::audit_output::AuditOutputResult;
use buck2_common::global_cfg_options::GlobalCfgOptions;
use buck2_core::cells::CellResolver;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use futures::FutureExt;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::values::dict::AllocDict;
use starlark::values::list_or_tuple::UnpackListOrTuple;
use starlark::values::starlark_value;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::nodes::action::StarlarkAction;
use crate::bxl::value_as_starlark_target_label::ValueAsStarlarkTargetLabel;

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    Allocative,
    StarlarkDocs
)]
#[starlark_docs(directory = "bxl")]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
#[allocative(skip)]
pub(crate) struct StarlarkAuditCtx<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    ctx: &'v BxlContext<'v>,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    working_dir: ProjectRelativePathBuf,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    cell_resolver: CellResolver,
}

#[starlark_value(type = "audit_ctx", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkAuditCtx<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(audit_methods)
    }
}

impl<'v> AllocValue<'v> for StarlarkAuditCtx<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkAuditCtx<'v> {
    pub(crate) fn new(
        ctx: &'v BxlContext<'v>,
        working_dir: ProjectRelativePathBuf,
        cell_resolver: CellResolver,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            ctx,
            working_dir,
            cell_resolver,
        })
    }
}

/// The context for performing `audit` operations in bxl. The functions offered on this ctx are
/// the same behaviour as the audit functions available within audit command.
#[starlark_module]
fn audit_methods(builder: &mut MethodsBuilder) {
    /// Returns either:
    ///  - The `action` which created the buck-out path, if exists.
    ///  - The `unconfigured_target_label` constructed from the buck-out path, if the configuration hashes do not match.
    ///  - None, if the configuration hash of the buck-out path matches the one passed into this function, or the default target
    /// configuration, but no action could be found that generated the buck-out path.
    ///
    /// Takes in an optional target platform, otherwise will use the default target platform.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_audit_output(ctx):
    ///     target_platform = "foo"
    ///     result = ctx.audit().output("buck-out/v2/gen/fbcode/some_cfg_hash/path/to/__target__/artifact", target_platform)
    ///     ctx.output.print(result)
    /// ```
    fn output<'v>(
        this: &StarlarkAuditCtx<'v>,
        // TODO(nga): parameters should be either positional or named, not both.
        output_path: &'v str,
        #[starlark(default = ValueAsStarlarkTargetLabel::NONE)]
        target_platform: ValueAsStarlarkTargetLabel<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<Option<Value<'v>>> {
        let target_platform = target_platform.parse_target_platforms(
            this.ctx.target_alias_resolver(),
            this.ctx.cell_resolver(),
            this.ctx.cell_alias_resolver(),
            this.ctx.cell_name(),
            &this.ctx.global_cfg_options().target_platform,
        )?;

        this.ctx.async_ctx.borrow_mut().via(|ctx| {
            async move {
                audit_output(
                    output_path,
                    &this.working_dir,
                    &this.cell_resolver,
                    ctx,
                    &GlobalCfgOptions {
                        target_platform,
                        cli_modifiers: vec![].into(),
                    },
                )
                .await?
                .map(|result| {
                    anyhow::Ok(match result {
                        AuditOutputResult::Match(action) => heap.alloc(StarlarkAction(
                            action
                                .action()
                                .context("audit_output did not return an action")?
                                .dupe(),
                        )),
                        AuditOutputResult::MaybeRelevant(label) => {
                            heap.alloc(StarlarkTargetLabel::new(label))
                        }
                    })
                })
                .transpose()
            }
            .boxed_local()
        })
    }

    /// Query information about the [repositories] list in .buckconfig.
    ///
    /// Takes the following parameters:
    /// * `aliases_to_resolve` - list of cell aliases to query. These aliases will be resolved in the root cell of the BXL script.
    /// * optional `aliases` flag - if enabled, and no explicit aliases are passed, will query for all aliases in the root cell of the BXL script.
    ///
    /// Returns a dict of cell name to absolute path mappings.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_audit_cell(ctx):
    ///     result = ctx.audit().cell(aliases = True)
    ///     ctx.output.print(result)
    /// ```
    fn cell<'v>(
        this: &StarlarkAuditCtx<'v>,
        // TODO(nga): parameters should be either positional or named, not both.
        #[starlark(default = UnpackListOrTuple::default())] aliases_to_resolve: UnpackListOrTuple<
            String,
        >,
        #[starlark(require = named, default = false)] aliases: bool,
    ) -> anyhow::Result<AllocDict<impl Iterator<Item = (String, String)>>> {
        let result = audit_cell(
            &aliases_to_resolve.items,
            aliases,
            &this.cell_resolver,
            &this.working_dir,
            this.ctx.project_root(),
        )?;
        Ok(AllocDict(
            result.into_iter().map(|(k, v)| (k, v.to_string())),
        ))
    }
}
