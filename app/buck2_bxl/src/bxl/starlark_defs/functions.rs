/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::time::Instant;

use buck2_build_api::actions::query::ActionQueryNode;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact_like::ValueAsInputArtifactLikeUnpack;
use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_core::cells::CellAliasResolver;
use buck2_core::package::PackageLabel;
use buck2_core::pattern::parse_package::parse_package;
use buck2_interpreter::types::package_path::StarlarkPackagePath;
use buck2_interpreter_for_build::interpreter::package_file_calculation::EvalPackageFile;
use buck2_interpreter_for_build::super_package::package_value::SuperPackageValuesImpl;
use buck2_node::metadata::key::MetadataKeyRef;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use buck2_node::nodes::unconfigured::TargetNode;
use dupe::Dupe;
use futures::FutureExt;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::Heap;
use starlark::values::StringValue;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::list::UnpackList;
use starlark::values::none::NoneType;
use starlark::values::tuple::UnpackTuple;
use starlark::values::type_repr::StarlarkTypeRepr;

use super::artifacts::visit_artifact_path_without_associated_deduped;
use super::context::output::get_artifact_path_display;
use super::context::output::get_cmd_line_inputs;
use super::nodes::unconfigured::StarlarkTargetNode;
use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::eval_extra::BxlEvalExtra;
use crate::bxl::starlark_defs::nodes::action::StarlarkActionQueryNode;
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
    ///     unode_a = ctx.unconfigured_targets("root//bin:foo")
    ///     unode_b = ctx.unconfigured_targets("root//bin:bar")
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

    /// Creates a target set from a list of action query nodes.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_atarget_set(ctx):
    ///     actions = ctx.aquery().all_actions("//target")
    ///     action_a = actions[0]
    ///     action_b = actions[1]
    ///     action_set = bxl.atarget_set([action_a, action_b])
    ///     # Now can use in further queries
    ///     deps = ctx.aquery().deps(action_set)
    /// ```
    fn atarget_set(
        nodes: Option<UnpackList<StarlarkActionQueryNode>>,
    ) -> starlark::Result<StarlarkTargetSet<ActionQueryNode>> {
        Ok(StarlarkTargetSet::from_iter(
            nodes
                .unwrap_or(UnpackList::default())
                .items
                .into_iter()
                .map(|node| node.0),
        ))
    }
}

#[derive(Debug, buck2_error::Error, Clone)]
#[buck2(tag = Input)]
enum GetPathWithMaterializationError {
    #[error("Promise artifacts are not supported in `get_path_without_materialization()`")]
    PromiseArtifactsNotSupported,
}

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
        #[starlark(require=pos)] this: ValueAsInputArtifactLikeUnpack<'v>,
        #[starlark(require=pos)] ctx: &'v BxlContext<'v>,
        #[starlark(require = named, default = false)] abs: bool,
        heap: Heap<'v>,
    ) -> starlark::Result<StringValue<'v>> {
        let path = match this {
            ValueAsInputArtifactLikeUnpack::Artifact(a) => {
                let artifact = a.artifact();
                get_artifact_path_display(
                    artifact.get_path(),
                    abs,
                    ctx.project_fs(),
                    ctx.artifact_fs(),
                )?
            }
            ValueAsInputArtifactLikeUnpack::DeclaredArtifact(a) => get_artifact_path_display(
                a.get_artifact_path(),
                abs,
                ctx.project_fs(),
                ctx.artifact_fs(),
            )?,
            _ => {
                return Err(buck2_error::Error::from(
                    GetPathWithMaterializationError::PromiseArtifactsNotSupported,
                )
                .into());
            }
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
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<Value<'v>> {
        let inputs = get_cmd_line_inputs(cmd_line.0)?;
        let mut result = Vec::new();

        for artifact_group in &inputs.inputs {
            result.push(artifact_group.dupe());
        }

        let mut paths = Vec::new();

        let heap = eval.heap();

        ctx.via_dice(eval, |dice_ctx| {
            dice_ctx.via(|dice_ctx| {
                visit_artifact_path_without_associated_deduped(
                    &result,
                    abs,
                    |artifact_path, abs| {
                        let path = get_artifact_path_display(
                            artifact_path,
                            abs,
                            ctx.project_fs(),
                            ctx.artifact_fs(),
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

#[derive(StarlarkTypeRepr, UnpackValue)]
pub(crate) enum PackagePathArg<'v> {
    PackagePath(&'v StarlarkPackagePath),
    Str(&'v str),
}

impl<'v> PackagePathArg<'v> {
    fn pkg(&self, cell_alias_resolver: &CellAliasResolver) -> buck2_error::Result<PackageLabel> {
        match self {
            PackagePathArg::PackagePath(package_path) => Ok(package_path.pkg().dupe()),
            PackagePathArg::Str(pkg_str) => {
                // TODO(nero): add support for relative path
                parse_package(pkg_str, cell_alias_resolver)
            }
        }
    }
}

#[starlark_module]
pub(crate) fn register_read_package_value_function(builder: &mut GlobalsBuilder) {
    /// Read package value from the specified package path.
    ///
    /// Returns the value specified in the PACKAGE file for the given package path and key,
    /// or None if not found.
    ///
    /// This function returns the nearest `name` value registered per `PACKAGE` based on the given
    /// `PackagePath` or str, or None if such value does not exist.
    ///
    /// The `package` parameter accepts any of the following:
    /// - A `PackagePath`
    /// - A string representing a package path (e.g., "root//some/package")
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_read_package_value(ctx):
    ///     # Get a unconfigured target from the package we want to read metadata from
    ///     node = ctx.unconfigured_targets("root//some/package:target")
    ///     pkg_path = node.label.package_path
    ///
    ///     # Read a package value with the given key from the unconfigured target
    ///     pkg_value1 = bxl.read_package_value(pkg_path, "aaa.bbb")
    ///
    ///     pkg_value2 = bxl.read_package_value("root//path/to/pkg", "aaa.ccc")
    /// ```
    // FIXME(JakobDegen): This ought to require a `BxlContext`
    fn read_package_value<'v>(
        #[starlark(require = pos)] package_path: PackagePathArg<'v>,
        #[starlark(require = pos)] key: &str,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<Value<'v>> {
        let metadata_key = MetadataKeyRef::new(key).map_err(buck2_error::Error::from)?;

        let bxl_eval_extra = BxlEvalExtra::from_context(eval)?;
        let package = package_path.pkg(bxl_eval_extra.core.cell_alias_resolver())?;

        let super_package = bxl_eval_extra
            .dice
            .via(|dice| async { dice.eval_package_file(package).await }.boxed_local())?;

        // Use this instead of `get_package_value_json()`` to get the native Starlark value directly,
        // rather than converting the Starlark value to JSON first
        match SuperPackageValuesImpl::get(&**super_package.package_values())?
            .get_package_value(metadata_key)
        {
            Some(value) => Ok(eval
                .heap()
                .access_owned_frozen_value(value.owned_frozen_value())),
            None => Ok(Value::new_none()),
        }
    }
}
