/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use async_recursion::async_recursion;
use buck2_artifact::artifact::source_artifact::SourceArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_common::dice::file_ops::DiceFileOps;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::file_ops::FileOps;
use buck2_common::file_ops::PathMetadataOrRedirection;
use buck2_common::package_listing::dice::HasPackageListingResolver;
use buck2_common::package_listing::resolver::PackageListingResolver;
use buck2_core::buck_path::path::BuckPath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::cell_path::CellPathRef;
use buck2_core::cells::instance::CellInstance;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_core::package::PackageLabel;
use buck2_node::nodes::unconfigured::TargetNode;
use derivative::Derivative;
use derive_more::Display;
use futures::FutureExt;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::none::NoneType;
use starlark::values::starlark_value;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::StarlarkDocs;
use thiserror::Error;

use super::BxlContext;
use crate::bxl::starlark_defs::file_expr::FileExpr;
use crate::bxl::starlark_defs::file_set::StarlarkReadDirSet;
use crate::bxl::starlark_defs::target_expr::TargetExpr;

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    StarlarkDocs,
    Allocative
)]
#[derivative(Debug)]
#[starlark_docs(directory = "bxl")]
#[display(fmt = "{:?}", self)]
#[allocative(skip)]
pub(crate) struct BxlFilesystem<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    ctx: &'v BxlContext<'v>,
}

impl<'v> BxlFilesystem<'v> {
    pub(crate) fn new(ctx: &'v BxlContext<'v>) -> Self {
        Self { ctx }
    }

    fn artifact_fs(&self) -> &ArtifactFs {
        &self.ctx.artifact_fs
    }

    fn project_fs(&self) -> &ProjectRoot {
        &self.ctx.project_fs
    }

    fn cell(&self) -> anyhow::Result<&CellInstance> {
        self.ctx.cell_resolver.get(self.ctx.cell_name)
    }
}

#[starlark_value(type = "fs", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for BxlFilesystem<'v> {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(fs_operations)
    }
}

impl<'v> AllocValue<'v> for BxlFilesystem<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

#[derive(Debug, Error)]
pub(crate) enum BxlFilesystemError {
    #[error("Inferred package path `{0}` is not a valid package within the given file path `{1}`")]
    PackageMismatch(PackageLabel, CellPath),
    #[error("Expected a single target hint, not an iterable: `{0}`")]
    MultipleTargetHintsNotSupported(String),
}

impl<'v> BxlFilesystem<'v> {
    /// Returns the absolute path for a FileExpr.
    fn resolve(&'v self, expr: FileExpr<'v>) -> anyhow::Result<AbsNormPathBuf> {
        let project_rel_path = self.project_relative_path(expr)?;
        Ok(self.project_fs().resolve(&project_rel_path))
    }

    /// Returns the project relative path for a cellpath.
    fn project_relative_path(
        &'v self,
        expr: FileExpr<'v>,
    ) -> anyhow::Result<ProjectRelativePathBuf> {
        let cell_path = self
            .ctx
            .async_ctx
            .via_dice(|dice| async { expr.get(dice, self.cell()?).await }.boxed_local())?;
        self.artifact_fs().resolve_cell_path(cell_path.as_ref())
    }
}

#[async_recursion]
async fn try_exists<'v>(file_ops: &DiceFileOps<'v>, path: CellPathRef<'v>) -> anyhow::Result<bool> {
    match file_ops.read_path_metadata_if_exists(path).await? {
        Some(path) => match PathMetadataOrRedirection::from(path) {
            PathMetadataOrRedirection::PathMetadata(_) => Ok(true),
            PathMetadataOrRedirection::Redirection(r) => {
                try_exists(file_ops, r.as_ref().as_ref()).await
            }
        },
        None => Ok(false),
    }
}

/// Provides some basic tracked filesystem access for bxl functions so that they can meaningfully
/// detect simple properties of artifacts, and source directories.
#[starlark_module]
fn fs_operations(builder: &mut MethodsBuilder) {
    /// Check if a path exists on disk, taking advantage of Buck's cached filesystem.
    /// Takes in a literal, a source artifact (via `[StarlarkArtifact]`), or a `[StarlarkFileNode]`.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_exists(ctx):
    ///     ctx.output.print(ctx.fs.exists("bin"))
    /// ```
    fn exists<'v>(this: &'v BxlFilesystem<'v>, expr: FileExpr<'v>) -> anyhow::Result<bool> {
        this.ctx.async_ctx.via_dice(|dice| {
            async {
                let path = expr.get(dice, this.cell()?).await;

                match path {
                    Ok(p) => try_exists(&dice.file_ops(), p.as_ref()).await,
                    Err(e) => Err(e),
                }
            }
            .boxed_local()
        })
    }

    /// Returns all the contents of the given input that points to a directory.
    /// Errors if the given path is a file. Takes an optional boolean `dirs_only` to only return directories, defaults to false.
    ///
    /// The input is a either a literal, a source artifact (via `[StarlarkArtifact]`), or a `[StarlarkFileNode]`.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_list(ctx):
    ///     list_results = ctx.fs.list("bin")
    ///     for result in list_results:
    ///         ctx.output.print(result)
    /// ```
    fn list<'v>(
        this: &'v BxlFilesystem<'v>,
        expr: FileExpr<'v>,
        #[starlark(require = named, default = false)] dirs_only: bool,
    ) -> anyhow::Result<StarlarkReadDirSet> {
        this.ctx.async_ctx.via_dice(|dice| {
            async {
                let path = expr.get(dice, this.cell()?).await;

                match path {
                    Ok(path) => {
                        let read_dir_output = dice.file_ops().read_dir(path.as_ref()).await?;
                        Ok(StarlarkReadDirSet {
                            cell_path: path,
                            included: read_dir_output.included,
                            dirs_only,
                        })
                    }
                    Err(e) => Err(e),
                }
            }
            .boxed_local()
        })
    }

    /// Returns whether the provided path is a dir. Returns false is the dir does not exist.
    /// The input is a either a literal, a source artifact (via `[StarlarkArtifact]`), or a `[StarlarkFileNode]`.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_is_dir(ctx):
    ///     ctx.output.print(ctx.fs.is_dir("bin"))
    /// ```
    fn is_dir<'v>(this: &'v BxlFilesystem<'v>, expr: FileExpr<'v>) -> anyhow::Result<bool> {
        Ok(std::path::Path::is_dir(this.resolve(expr)?.as_ref()))
    }

    /// Returns whether the provided path is a file. Returns false is the file does not exist.
    /// The input is a either a literal, a source artifact (via `[StarlarkArtifact]`), or a `[StarlarkFileNode]`.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_is_file(ctx):
    ///     ctx.output.print(ctx.fs.is_dir("bin"))
    /// ```
    fn is_file<'v>(this: &'v BxlFilesystem<'v>, expr: FileExpr<'v>) -> anyhow::Result<bool> {
        Ok(std::path::Path::is_file(this.resolve(expr)?.as_ref()))
    }

    /// Returns the relative path to the project root, given the file expression.
    ///
    /// Sample usage:
    /// ```text
    /// def project_rel_path(ctx):
    ///     ctx.output.print(ctx.fs.project_rel_path("bin"))
    /// ```
    fn project_rel_path<'v>(
        this: &'v BxlFilesystem<'v>,
        expr: FileExpr<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<StringValue<'v>> {
        Ok(heap.alloc_str(this.project_relative_path(expr)?.as_str()))
    }

    /// Returns the absolute path, given the file expression. Use at your own risk, as the current working directory
    /// may have been changed when this function is called. In addition, passing the absolute path into actions that
    /// are run remotely will most likely result in failures since the absolute path most likely differs locally vs remotely.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_abs_path_unsafe(ctx):
    ///     ctx.output.print(ctx.fs.abs_path_unsafe("bin"))
    /// ```
    fn abs_path_unsafe<'v>(
        this: &'v BxlFilesystem<'v>,
        expr: FileExpr<'v>,
        heap: &'v Heap,
    ) -> anyhow::Result<StringValue<'v>> {
        let abs_norm_path = this.resolve(expr)?;
        Ok(heap.alloc_str(abs_norm_path.as_abs_path().to_str()?))
    }

    /// Returns the source artifact for a path and an optional target hint (unconfigured target label or node)
    /// which points to the owning package. If no target hint is given, the nearest package will be used to
    /// guess the desired artifact. The path should be either an absolute path, or a project relative path.
    fn source<'v>(
        this: &'v BxlFilesystem<'v>,
        expr: FileExpr<'v>,
        #[starlark(default = NoneType)] target_hint: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let buck_path = this.ctx.async_ctx.via_dice(|dice| {
            async {
                let file_path_as_cell_path = expr.get(dice, this.cell()?).await?;
                let file_path = this
                    .artifact_fs()
                    .resolve_cell_path(file_path_as_cell_path.as_ref())?;

                let package_label = if target_hint.is_none() {
                    dice.get_package_listing_resolver()
                        .get_enclosing_package(file_path_as_cell_path.as_ref())
                        .await
                } else {
                    let target_expr =
                        TargetExpr::<'v, TargetNode>::unpack(target_hint, this.ctx, dice, eval)
                            .await?;
                    match target_expr {
                        TargetExpr::Node(node) => Ok(node.label().pkg()),
                        TargetExpr::Label(label) => Ok(label.as_ref().pkg()),
                        _ => Err(anyhow::anyhow!(
                            BxlFilesystemError::MultipleTargetHintsNotSupported(
                                target_hint.to_repr()
                            )
                        )),
                    }
                }?;

                let package_path = this
                    .artifact_fs()
                    .resolve_cell_path(package_label.as_cell_path())?;

                let forward_relative_path = match file_path.strip_prefix(&package_path) {
                    Ok(path) => path,
                    Err(_) => {
                        return Err(anyhow::anyhow!(BxlFilesystemError::PackageMismatch(
                            package_label,
                            file_path_as_cell_path,
                        )));
                    }
                };

                let package_relative_path =
                    PackageRelativePath::new(forward_relative_path.as_path())?;
                Ok(BuckPath::new(package_label, package_relative_path.into()))
            }
            .boxed_local()
        })?;

        Ok(eval
            .heap()
            .alloc(StarlarkArtifact::new(SourceArtifact::new(buck_path).into())))
    }
}
