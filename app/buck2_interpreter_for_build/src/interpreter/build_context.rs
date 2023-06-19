/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Debug;

use buck2_common::legacy_configs::view::LegacyBuckConfigView;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::CellResolver;
use buck2_core::package::PackageLabel;
use buck2_interpreter::build_context::STARLARK_PATH_FROM_BUILD_CONTEXT;
use buck2_interpreter::file_type::StarlarkFileType;
use buck2_interpreter::path::BxlFilePath;
use buck2_interpreter::path::PackageFilePath;
use buck2_interpreter::path::StarlarkModulePath;
use buck2_interpreter::path::StarlarkPath;
use starlark::any::ProvidesStaticType;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use thiserror::Error;

use crate::interpreter::buckconfig::LegacyBuckConfigForStarlark;
use crate::interpreter::cell_info::InterpreterCellInfo;
use crate::interpreter::functions::host_info::HostInfo;
use crate::interpreter::module_internals::ModuleInternals;
use crate::super_package::eval_ctx::PackageFileEvalCtx;

#[derive(Error, Debug)]
enum BuildContextError {
    #[error(
        "This function is unavailable during analysis (usual solution is to place the information on a toolchain)"
    )]
    UnavailableDuringAnalysis,
    #[error("`{0}()` can only be called from a build file context; current file context is {1:?}")]
    NotBuildFile(String, StarlarkFileType),
    #[error("Expecting build file context, but current file context is {0:?}")]
    NotBuildFileNoFunction(StarlarkFileType),
    #[error(
        "`{0}()` can only be called from a package file context; current file context is {1:?}"
    )]
    NotPackageFile(String, StarlarkFileType),
    #[error("Expecting package file context, but current file context is {0:?}")]
    NotPackageFileNoFunction(StarlarkFileType),
    #[error("Package can only be fetched from a build file")]
    PackageOnlyFromBuildFile,
}

#[derive(Debug)]
pub(crate) enum PerFileTypeContext {
    /// Context for evaluating `BUCK` files.
    Build(BuildFilePath, ModuleInternals),
    /// Context for evaluating `PACKAGE` files.
    Package(PackageFilePath, PackageFileEvalCtx),
    Bzl(ImportPath),
    Bxl(BxlFilePath),
}

impl PerFileTypeContext {
    pub(crate) fn starlark_path(&self) -> StarlarkPath {
        match self {
            PerFileTypeContext::Build(path, _) => StarlarkPath::BuildFile(path),
            PerFileTypeContext::Package(path, _) => StarlarkPath::PackageFile(path),
            PerFileTypeContext::Bzl(path) => StarlarkPath::LoadFile(path),
            PerFileTypeContext::Bxl(path) => StarlarkPath::BxlFile(path),
        }
    }

    pub(crate) fn file_type(&self) -> StarlarkFileType {
        self.starlark_path().file_type()
    }

    pub(crate) fn require_build(&self, function_name: &str) -> anyhow::Result<&ModuleInternals> {
        match self {
            PerFileTypeContext::Build(_, internals) => Ok(internals),
            x => {
                Err(BuildContextError::NotBuildFile(function_name.to_owned(), x.file_type()).into())
            }
        }
    }

    pub(crate) fn into_build(self) -> anyhow::Result<ModuleInternals> {
        match self {
            PerFileTypeContext::Build(_, internals) => Ok(internals),
            x => Err(BuildContextError::NotBuildFileNoFunction(x.file_type()).into()),
        }
    }

    pub(crate) fn require_package_file(
        &self,
        function_name: &str,
    ) -> anyhow::Result<&PackageFileEvalCtx> {
        match self {
            PerFileTypeContext::Package(_, ctx) => Ok(ctx),
            x => Err(
                BuildContextError::NotPackageFile(function_name.to_owned(), x.file_type()).into(),
            ),
        }
    }

    pub(crate) fn into_package_file(self) -> anyhow::Result<PackageFileEvalCtx> {
        match self {
            PerFileTypeContext::Package(_, ctx) => Ok(ctx),
            x => Err(BuildContextError::NotPackageFileNoFunction(x.file_type()).into()),
        }
    }

    pub(crate) fn for_module(path: StarlarkModulePath) -> PerFileTypeContext {
        match path {
            StarlarkModulePath::LoadFile(bzl) => PerFileTypeContext::Bzl(bzl.clone()),
            StarlarkModulePath::BxlFile(bxl) => PerFileTypeContext::Bxl(bxl.clone()),
        }
    }
}

/// Buck-specific information exposed to the starlark environment via the context's extra field.
/// This would include things like the current cell or package name or the package listing. It's
/// used for quite a few other things, including recording declared rules.
///
/// Many of the functions available in build files reach out to this as part of
/// their implementation.
#[derive(ProvidesStaticType, Debug)]
pub struct BuildContext<'a> {
    /// The CellInfo for the top-level module being interpreted. Note that this
    /// is not necessarily the same as the file currently being processed
    /// (i.e. if `cell1//BUCK` loads `cell2//defs.bzl` the cell info when
    /// loading `cell2//defs.bzl` will be based on `cell1`).
    ///
    /// This cell_info is used only for things that should be resolved in the
    /// context of the top-level module. The most important of these is the
    /// `read_config()` implementation.
    ///
    /// For example of where this isn't used, it is not used for resolving
    /// `load()` statements.
    cell_info: &'a InterpreterCellInfo,

    /// Current cell file buckconfig.
    pub(crate) buckconfig: LegacyBuckConfigForStarlark<'a>,
    /// Buckconfig of the root cell.
    pub(crate) root_buckconfig: LegacyBuckConfigForStarlark<'a>,

    pub host_info: &'a HostInfo,

    /// Context specific to type type.
    pub(crate) additional: PerFileTypeContext,

    /// When true, rule function is no-op.
    pub ignore_attrs_for_profiling: bool,
}

impl<'a> BuildContext<'a> {
    /// Create a build context for the given module.
    pub(crate) fn new_for_module(
        module: &'a Module,
        cell_info: &'a InterpreterCellInfo,
        buckconfig: &'a (dyn LegacyBuckConfigView + 'a),
        root_buckconfig: &'a (dyn LegacyBuckConfigView + 'a),
        host_info: &'a HostInfo,
        additional: PerFileTypeContext,
        ignore_attrs_for_profiling: bool,
    ) -> BuildContext<'a> {
        let buckconfig = LegacyBuckConfigForStarlark::new(module, buckconfig);
        let root_buckconfig = LegacyBuckConfigForStarlark::new(module, root_buckconfig);
        BuildContext {
            cell_info,
            buckconfig,
            root_buckconfig,
            host_info,
            additional,
            ignore_attrs_for_profiling,
        }
    }

    pub fn from_context<'v>(eval: &Evaluator<'v, 'a>) -> anyhow::Result<&'a BuildContext<'a>> {
        match eval.extra {
            None => Err(BuildContextError::UnavailableDuringAnalysis.into()),
            Some(extra) => Ok(extra
                .downcast_ref::<BuildContext>()
                .unwrap_or_else(|| panic!("Unable to access context extra. Wrong type."))),
        }
    }

    pub(crate) fn cell_info(&self) -> &InterpreterCellInfo {
        self.cell_info
    }

    pub fn build_file_cell(&self) -> BuildFileCell {
        self.cell_info.name()
    }

    pub fn cell_resolver(&self) -> &CellResolver {
        self.cell_info.cell_resolver()
    }

    pub fn require_package(&self) -> anyhow::Result<PackageLabel> {
        match &self.additional {
            PerFileTypeContext::Build(path, _) => Ok(path.package()),
            _ => Err(BuildContextError::PackageOnlyFromBuildFile.into()),
        }
    }

    pub fn starlark_path(&self) -> StarlarkPath {
        self.additional.starlark_path()
    }
}

pub(crate) fn init_starlark_path_from_build_context() {
    STARLARK_PATH_FROM_BUILD_CONTEXT
        .init(|eval| Ok(BuildContext::from_context(eval)?.starlark_path()))
}

/// Arbitrary object made available to the execution context. Converted to
/// EvalResult at the end of interpreting
impl ModuleInternals {
    /// Try to get this inner context from the `ctx.extra` property.
    pub fn from_context<'a>(ctx: &'a Evaluator, function_name: &str) -> anyhow::Result<&'a Self> {
        BuildContext::from_context(ctx)?
            .additional
            .require_build(function_name)
    }
}
