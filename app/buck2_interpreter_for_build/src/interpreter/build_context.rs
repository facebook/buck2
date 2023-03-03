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
use buck2_core::package::PackageLabel;
use buck2_interpreter::extra::buckconfig::LegacyBuckConfigForStarlark;
use buck2_interpreter::extra::cell_info::InterpreterCellInfo;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_interpreter::file_type::StarlarkFileType;
use buck2_interpreter::path::StarlarkModulePath;
use buck2_interpreter::path::StarlarkPath;
use starlark::any::ProvidesStaticType;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use thiserror::Error;

use crate::interpreter::functions::host_info::HostInfo;
use crate::interpreter::module_internals::ModuleInternals;
use crate::super_package::eval_ctx::PackageFileEvalCtx;

#[derive(Error, Debug)]
enum BuildContextError {
    #[error(
        "This function is unavailable during analysis (usual solution is to place the information on a toolchain)"
    )]
    UnavailableDuringAnalysis,
    #[error("Expecting a build file; current file type context is {0:?}")]
    NotBuildFile(StarlarkFileType),
    #[error("Expecting a package file; current file type context is {0:?}")]
    NotPackageFile(StarlarkFileType),
}

#[derive(Debug)]
pub(crate) enum PerFileTypeContext {
    /// Context for evaluating `BUCK` files.
    Build(ModuleInternals),
    /// Context for evaluating `PACKAGE` files.
    Package(PackageFileEvalCtx),
    Bzl,
    Bxl,
}

impl PerFileTypeContext {
    fn file_type(&self) -> StarlarkFileType {
        match self {
            PerFileTypeContext::Build(..) => StarlarkFileType::Buck,
            PerFileTypeContext::Package(..) => StarlarkFileType::Package,
            PerFileTypeContext::Bzl => StarlarkFileType::Bzl,
            PerFileTypeContext::Bxl => StarlarkFileType::Bxl,
        }
    }

    pub(crate) fn require_build(&self) -> anyhow::Result<&ModuleInternals> {
        match self {
            PerFileTypeContext::Build(internals) => Ok(internals),
            x => Err(BuildContextError::NotBuildFile(x.file_type()).into()),
        }
    }

    pub(crate) fn into_build(self) -> anyhow::Result<ModuleInternals> {
        match self {
            PerFileTypeContext::Build(internals) => Ok(internals),
            x => Err(BuildContextError::NotBuildFile(x.file_type()).into()),
        }
    }

    pub(crate) fn require_package_file(&self) -> anyhow::Result<&PackageFileEvalCtx> {
        match self {
            PerFileTypeContext::Package(ctx) => Ok(ctx),
            x => Err(BuildContextError::NotPackageFile(x.file_type()).into()),
        }
    }

    pub(crate) fn into_package_file(self) -> anyhow::Result<PackageFileEvalCtx> {
        match self {
            PerFileTypeContext::Package(ctx) => Ok(ctx),
            x => Err(BuildContextError::NotPackageFile(x.file_type()).into()),
        }
    }

    pub(crate) fn for_module(path: StarlarkModulePath) -> PerFileTypeContext {
        match path {
            StarlarkModulePath::LoadFile(_) => PerFileTypeContext::Bzl,
            StarlarkModulePath::BxlFile(_) => PerFileTypeContext::Bxl,
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
    pub buckconfig: LegacyBuckConfigForStarlark<'a>,
    /// Buckconfig of the root cell.
    pub root_buckconfig: LegacyBuckConfigForStarlark<'a>,

    /// The import path that is being evaluated
    pub starlark_path: StarlarkPath<'a>,

    pub host_platform: InterpreterHostPlatform,

    pub host_architecture: InterpreterHostArchitecture,

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
        starlark_path: StarlarkPath<'a>,
        host_platform: InterpreterHostPlatform,
        host_architecture: InterpreterHostArchitecture,
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
            starlark_path,
            host_platform,
            host_architecture,
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

    pub fn cell_info(&self) -> &InterpreterCellInfo {
        self.cell_info
    }

    pub fn require_package(&self) -> anyhow::Result<PackageLabel> {
        match self.starlark_path {
            StarlarkPath::BuildFile(b) => Ok(b.package()),
            _ => Err(anyhow::anyhow!(
                "package can only be fetched from a build file"
            )),
        }
    }
}

/// Arbitrary object made available to the execution context. Converted to
/// EvalResult at the end of interpeting
impl ModuleInternals {
    /// Try to get this inner context from the `ctx.extra` property.
    pub fn from_context<'a>(ctx: &'a Evaluator) -> anyhow::Result<&'a Self> {
        BuildContext::from_context(ctx)?.additional.require_build()
    }
}
