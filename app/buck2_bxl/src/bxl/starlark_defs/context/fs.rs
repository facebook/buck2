/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_common::dice::file_ops::HasFileOps;
use buck2_common::file_ops::FileOps;
use buck2_core::cells::CellInstance;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_execute::artifact::fs::ArtifactFs;
use derivative::Derivative;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_type;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::context::starlark_async::BxlSafeDiceComputations;
use crate::bxl::starlark_defs::file_expr::FileExpr;
use crate::bxl::starlark_defs::file_set::StarlarkReadDirSet;

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
pub struct BxlFilesystem<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    dice: &'v BxlSafeDiceComputations<'v>,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    project_fs: &'v ProjectRoot,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    artifact_fs: &'v ArtifactFs,
    #[trace(unsafe_ignore)]
    cell: &'v CellInstance,
}

impl<'v> BxlFilesystem<'v> {
    pub(crate) fn new(
        dice: &'v BxlSafeDiceComputations<'v>,
        project_fs: &'v ProjectRoot,
        artifact_fs: &'v ArtifactFs,
        cell: &'v CellInstance,
    ) -> Self {
        Self {
            dice,
            project_fs,
            artifact_fs,
            cell,
        }
    }
}

impl<'v> StarlarkValue<'v> for BxlFilesystem<'v> {
    starlark_type!("fs");

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

impl<'v> StarlarkTypeRepr for &'v BxlFilesystem<'v> {
    fn starlark_type_repr() -> String {
        BxlFilesystem::get_type_starlark_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v BxlFilesystem<'v> {
    fn unpack_value(x: Value<'v>) -> Option<&'v BxlFilesystem<'v>> {
        x.downcast_ref()
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
    fn exists<'v>(this: &BxlFilesystem<'v>, expr: FileExpr<'v>) -> anyhow::Result<bool> {
        let path = expr.get(this.dice, this.cell);

        match path {
            Ok(p) => this.dice.via_dice(async move |ctx| {
                <dyn FileOps>::try_exists(&ctx.file_ops(), p.as_ref()).await
            }),
            Err(e) => Err(e),
        }
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
        this: &BxlFilesystem<'v>,
        expr: FileExpr<'v>,
        #[starlark(require = named, default = false)] dirs_only: bool,
    ) -> anyhow::Result<StarlarkReadDirSet> {
        let path = expr.get(this.dice, this.cell);

        match path {
            Ok(path) => this.dice.via_dice(async move |ctx| {
                let read_dir_output = ctx.file_ops().read_dir_with_ignores(path.as_ref()).await?;
                Ok(StarlarkReadDirSet {
                    cell_path: path,
                    included: read_dir_output.included,
                    dirs_only,
                })
            }),
            Err(e) => Err(e),
        }
    }

    /// Returns whether the provided path is a dir. Returns false is the dir does not exist.
    /// The input is a either a literal, a source artifact (via `[StarlarkArtifact]`), or a `[StarlarkFileNode]`.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_is_dir(ctx):
    ///     ctx.output.print(ctx.fs.is_dir("bin"))
    /// ```
    fn is_dir<'v>(this: &BxlFilesystem<'v>, expr: FileExpr<'v>) -> anyhow::Result<bool> {
        Ok(std::path::Path::is_dir(resolve(this, expr)?.as_ref()))
    }

    /// Returns whether the provided path is a file. Returns false is the file does not exist.
    /// The input is a either a literal, a source artifact (via `[StarlarkArtifact]`), or a `[StarlarkFileNode]`.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_is_file(ctx):
    ///     ctx.output.print(ctx.fs.is_dir("bin"))
    /// ```
    fn is_file<'v>(this: &BxlFilesystem<'v>, expr: FileExpr<'v>) -> anyhow::Result<bool> {
        Ok(std::path::Path::is_file(resolve(this, expr)?.as_ref()))
    }
}

/// Returns the absolute path for a FileExpr.
fn resolve<'v>(bxl_fs: &BxlFilesystem<'v>, expr: FileExpr<'v>) -> anyhow::Result<AbsNormPathBuf> {
    let cell_path = expr.get(bxl_fs.dice, bxl_fs.cell)?;
    let project_rel_path = bxl_fs.artifact_fs.resolve_cell_path(cell_path.as_ref())?;
    Ok(bxl_fs.project_fs.resolve(&project_rel_path))
}
