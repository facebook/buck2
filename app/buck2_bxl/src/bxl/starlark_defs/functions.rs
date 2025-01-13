/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::time::Instant;

use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsArtifactLikeUnpack;
use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use buck2_query::query::syntax::simple::eval::file_set::FileSet;
use dupe::Dupe;
use futures::FutureExt;
use indexmap::IndexSet;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::list::UnpackList;
use starlark::values::none::NoneType;
use starlark::values::tuple::UnpackTuple;
use starlark::values::Heap;
use starlark::values::StringValue;
use starlark::values::Value;
use starlark::values::ValueLike;

use super::artifacts::visit_artifact_path_without_associated_deduped;
use super::context::output::get_artifact_path_display;
use super::context::output::get_cmd_line_inputs;
use super::nodes::unconfigured::StarlarkTargetNode;
use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::eval_extra::BxlEvalExtra;
use crate::bxl::starlark_defs::file_set::StarlarkFileSet;
use crate::bxl::starlark_defs::nodes::configured::StarlarkConfiguredTargetNode;
use crate::bxl::starlark_defs::targetset::StarlarkTargetSet;
use crate::bxl::starlark_defs::time::StarlarkInstant;

/// Global methods on the target set.
#[starlark_module]
pub(crate) fn register_target_function(builder: &mut GlobalsBuilder) {
    /// Creates a target set from a list of configured nodes.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_ctarget_set(ctx):
    ///     targets = bxl.ctarget_set([cnode_a, cnode_b])
    ///     ctx.output.print(type(targets))
    ///     ctx.output.print(len(targets))
    /// ```
    fn ctarget_set(
        nodes: Option<UnpackList<StarlarkConfiguredTargetNode>>,
    ) -> starlark::Result<StarlarkTargetSet<ConfiguredTargetNode>> {
        Ok(StarlarkTargetSet::from_iter(
            nodes
                .unwrap_or(UnpackList::default())
                .items
                .into_iter()
                .map(|node| node.0),
        ))
    }

    /// Creates a target set from a list of unconfigured nodes.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_utarget_set(ctx):
    ///     targets = bxl.utarget_set([unode_a, unode_b])
    ///     ctx.output.print(type(targets))
    ///     ctx.output.print(len(targets))
    /// ```
    fn utarget_set(
        nodes: Option<UnpackList<StarlarkTargetNode>>,
    ) -> starlark::Result<StarlarkTargetSet<TargetNode>> {
        Ok(StarlarkTargetSet::from_iter(
            nodes
                .unwrap_or(UnpackList::default())
                .items
                .into_iter()
                .map(|node| node.0),
        ))
    }
}

/// Global methods on the file set.
#[starlark_module]
pub(crate) fn register_file_set_function(builder: &mut GlobalsBuilder) {
    /// Creates an empty file set for configured nodes.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_file_set(ctx):
    ///     files = file_set()
    ///     ctx.output.print(type(files))
    ///     ctx.output.print(len(files))
    /// ```
    fn file_set() -> starlark::Result<StarlarkFileSet> {
        Ok(StarlarkFileSet(FileSet::new(IndexSet::new())))
    }
}

#[derive(Debug, buck2_error::Error, Clone)]
#[error("Promise artifacts are not supported in `get_path_without_materialization()`")]
#[buck2(tag = Input)]
pub(crate) struct PromiseArtifactsNotSupported;

/// Global methods on artifacts.
#[starlark_module]
pub(crate) fn register_artifact_function(builder: &mut GlobalsBuilder) {
    /// The output path of an artifact-like (source, build, declared). Takes an optional boolean to print the
    /// absolute or relative path. Note that this method returns an artifact path without asking for the artifact
    /// to be materialized (i.e. it may not actually exist on the disk yet).
    ///
    /// This is a risky function to call because you may accidentally pass this path to further BXL actions
    /// that expect the artifact to be materialized. If this happens, the BXL script will error out.
    /// If you want the path without materialization for other uses that don’t involve passing them into
    /// further actions, then it’s safe.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_get_path_without_materialization(ctx):
    ///     owner = ctx.cquery().owner("cell//path/to/file")[0]
    ///     artifact = owner.get_source("cell//path/to/file", ctx)
    ///     source_artifact_project_rel_path = get_path_without_materialization(artifact, ctx)
    ///     ctx.output.print(source_artifact_project_rel_path) # Note this artifact is NOT ensured or materialized
    /// ```
    fn get_path_without_materialization<'v>(
        #[starlark(require=pos)] this: ValueAsArtifactLikeUnpack<'v>,
        #[starlark(require=pos)] ctx: &'v BxlContext<'v>,
        #[starlark(require = named, default = false)] abs: bool,
        heap: &'v Heap,
    ) -> starlark::Result<StringValue<'v>> {
        let path = match this {
            ValueAsArtifactLikeUnpack::Artifact(a) => {
                let artifact = a.artifact();
                get_artifact_path_display(
                    artifact.get_path(),
                    abs,
                    ctx.project_fs(),
                    ctx.artifact_fs(),
                )?
            }
            ValueAsArtifactLikeUnpack::DeclaredArtifact(a) => get_artifact_path_display(
                a.get_artifact_path(),
                abs,
                ctx.project_fs(),
                ctx.artifact_fs(),
            )?,
            _ => return Err(buck2_error::Error::from(PromiseArtifactsNotSupported).into()),
        };

        Ok(heap.alloc_str(&path))
    }

    /// The output paths of a `cmd_args()` inputs. The output paths will be returned as a list.
    /// Takes an optional boolean to print the absolute or relative path.
    /// Note that this method returns an artifact path without asking for the artifact to be materialized,
    /// (i.e. it may not actually exist on the disk yet).
    ///
    /// This is a risky function to call because you may accidentally pass this path to further BXL actions
    /// that expect the artifact to be materialized. If this happens, the BXL script will error out.
    /// If you want the path without materialization for other uses that don’t involve passing them into
    /// further actions, then it’s safe.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_get_paths_without_materialization(ctx):
    ///     node = ctx.configured_targets("root//bin:the_binary")
    ///     providers = ctx.analysis(node).providers()
    ///     path = get_paths_without_materialization(providers[RunInfo], abs=True) # Note this artifact is NOT ensured or materialized
    ///     ctx.output.print(path)
    /// ```
    fn get_paths_without_materialization<'v>(
        #[starlark(require=pos)] cmd_line: ValueAsCommandLineLike<'v>,
        #[starlark(require=pos)] ctx: &'v BxlContext<'v>,
        #[starlark(require = named, default = false)] abs: bool,
        heap: &'v Heap,
    ) -> starlark::Result<Value<'v>> {
        let inputs = get_cmd_line_inputs(cmd_line.0)?;
        let mut result = Vec::new();

        for artifact_group in &inputs.inputs {
            result.push(artifact_group.dupe());
        }

        let mut paths = Vec::new();

        ctx.via_dice(|dice_ctx, bxl_ctx| {
            dice_ctx.via(|dice_ctx| {
                visit_artifact_path_without_associated_deduped(
                    &result,
                    abs,
                    |artifact_path, abs| {
                        let path = get_artifact_path_display(
                            artifact_path,
                            abs,
                            bxl_ctx.project_fs(),
                            bxl_ctx.artifact_fs(),
                        )?;

                        paths.push(path);
                        Ok(())
                    },
                    dice_ctx,
                )
                .boxed_local()
            })
        })?;
        Ok(heap.alloc(paths))
    }
}

/// Global methods for Instant.
#[starlark_module]
pub(crate) fn register_instant_function(builder: &mut GlobalsBuilder) {
    /// Creates an Instant at the current time.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_elapsed_millis(ctx):
    ///     instant = now()
    ///     time_a = instant.elapsed_millis()
    ///     # do something that takes a long time
    ///     time_b = instant.elapsed_millis()
    ///
    ///     ctx.output.print(time_a)
    ///     ctx.output.print(time_b)
    /// ```
    ///
    /// This function is only accessible through Bxl.
    fn now(eval: &mut Evaluator) -> starlark::Result<StarlarkInstant> {
        BxlEvalExtra::from_context(eval)?;
        Ok(StarlarkInstant(Instant::now()))
    }
}

/// This is used to mark the error returned by `fail_no_stacktrace()` (via context chaining).
/// We check if this marker is present after finishing BXL evaluation. If this marker is present,
/// then we hide the stacktrace. Otherwise, we emit the stacktrace to users.
#[derive(Debug, buck2_error::Error, Clone)]
#[error("fail:{0}")]
#[buck2(tag = Tier0)]
pub(crate) struct BxlErrorWithoutStacktrace(String);

impl std::error::Error for BxlErrorWithoutStacktrace {}

/// Global method for error handling.
#[starlark_module]
pub(crate) fn register_error_handling_function(builder: &mut GlobalsBuilder) {
    fn fail_no_stacktrace(#[starlark(args)] args: UnpackTuple<Value>) -> anyhow::Result<NoneType> {
        let mut s = String::new();
        for x in args {
            s.push(' ');
            match x.unpack_str() {
                Some(x) => s.push_str(x),
                None => x.collect_repr(&mut s),
            }
        }
        Err(BxlErrorWithoutStacktrace(s).into())
    }
}
