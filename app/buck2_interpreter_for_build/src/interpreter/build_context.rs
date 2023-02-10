/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::any::Any;
use std::fmt;
use std::fmt::Debug;

use buck2_common::legacy_configs::view::LegacyBuckConfigView;
use buck2_common::package_listing::listing::PackageListing;
use buck2_core::package::package_relative_path::PackageRelativePath;
use buck2_core::package::PackageLabel;
use buck2_interpreter::extra::buckconfig::LegacyBuckConfigForStarlark;
use buck2_interpreter::extra::cell_info::InterpreterCellInfo;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_interpreter::globspec::GlobSpec;
use buck2_interpreter::path::StarlarkPath;
use starlark::any::ProvidesStaticType;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use thiserror::Error;

#[derive(Error, Debug)]
enum BuildContextError {
    #[error(
        "This function is unavailable during analysis (usual solution is to place the information on a toolchain)"
    )]
    UnavailableDuringAnalysis,
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

    /// The files owned by this directory. Is `None` for .bzl files.
    pub listing: Option<PackageListing>,

    pub host_platform: InterpreterHostPlatform,

    pub host_architecture: InterpreterHostArchitecture,

    /// Additional dynamic information passed in via the interpreter
    /// configurator
    pub additional: Option<Box<dyn ExtraContextDyn>>,

    /// When true, rule function is no-op.
    pub ignore_attrs_for_profiling: bool,
}

impl<'a> BuildContext<'a> {
    /// Create a build context for the given module.
    pub fn new_for_module(
        module: &'a Module,
        cell_info: &'a InterpreterCellInfo,
        buckconfig: &'a (dyn LegacyBuckConfigView + 'a),
        root_buckconfig: &'a (dyn LegacyBuckConfigView + 'a),
        starlark_path: StarlarkPath<'a>,
        listing: Option<PackageListing>,
        host_platform: InterpreterHostPlatform,
        host_architecture: InterpreterHostArchitecture,
        additional: Option<Box<dyn ExtraContextDyn>>,
        ignore_attrs_for_profiling: bool,
    ) -> BuildContext<'a> {
        let buckconfig = LegacyBuckConfigForStarlark::new(module, buckconfig);
        let root_buckconfig = LegacyBuckConfigForStarlark::new(module, root_buckconfig);
        BuildContext {
            cell_info,
            buckconfig,
            root_buckconfig,
            starlark_path,
            listing,
            host_platform,
            host_architecture,
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

    pub fn resolve_glob(
        &'a self,
        spec: &'a GlobSpec,
    ) -> anyhow::Result<impl Iterator<Item = &'a PackageRelativePath> + 'a> {
        match &self.listing {
            Some(listing) => Ok(spec.resolve_glob(listing.files())),
            None => Err(anyhow::anyhow!(
                "glob() can only be called from a build file"
            )),
        }
    }
}

/// Arbitrary object made available to the execution context. Converted to
/// EvalResult at the end of interpeting
pub trait ExtraContext: Sized
where
    Self: 'static,
{
    type EvalResult: From<Self>;

    fn get<'a>(untyped: &'a dyn ExtraContextDyn) -> Option<&'a Self> {
        untyped.as_any().downcast_ref::<Self>()
    }

    /// Try to get this inner context from the `ctx.extra` property.
    fn from_context<'a>(ctx: &'a starlark::eval::Evaluator) -> anyhow::Result<&'a Self> {
        match &BuildContext::from_context(ctx)?.additional {
            None => Err(anyhow::anyhow!(
                "Unable to access module internals. This could be due to accessing it in the context of interpreting a .bzl file."
            )),
            Some(v) => Ok(Self::get(&**v).unwrap()),
        }
    }

    /// Convert the untyped, boxed version of this context into a final
    /// interpreter result
    fn into_eval_result(untyped: Box<dyn ExtraContextDyn>) -> anyhow::Result<Self::EvalResult> {
        match untyped.into_any().downcast::<Self>() {
            Ok(inner) => Ok(Self::EvalResult::from(*inner)),
            Err(_) => Err(anyhow::anyhow!(
                "Unable to access module internals. This could be due to accessing it in the context of interpreting a .bzl file."
            )),
        }
    }
}

/// Unsized version of `ExtraContext`.
pub trait ExtraContextDyn: Any {
    fn as_any(&self) -> &dyn Any;
    fn into_any(self: Box<Self>) -> Box<dyn Any>;
}

impl<E: ExtraContext> ExtraContextDyn for E {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn into_any(self: Box<Self>) -> Box<dyn Any> {
        self
    }
}

impl Debug for Box<dyn ExtraContextDyn> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.as_any(), f)
    }
}
