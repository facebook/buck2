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
use std::sync::Arc;

use anyhow::anyhow;
use buck2_common::legacy_configs::view::LegacyBuckConfigView;
use buck2_common::package_listing::listing::PackageListing;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::package::Package;
use buck2_core::package::PackageRelativePath;
use buck2_core::result::SharedResult;
use gazebo::any::ProvidesStaticType;
use gazebo::cmp::PartialEqAny;
use gazebo::dupe::Dupe;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use thiserror::Error;

use crate::common::StarlarkPath;
use crate::extra::buckconfig::LegacyBuckConfigForStarlark;
use crate::extra::cell_info::InterpreterCellInfo;
use crate::file_loader::LoadedModules;
use crate::globspec::GlobSpec;
use crate::package_imports::ImplicitImport;

pub mod buckconfig;
pub mod cell_info;

#[derive(Error, Debug)]
enum BuildContextError {
    #[error(
        "This function is unavailable during analysis (usual solution is to place the information on a toolchain)"
    )]
    UnavailableDuringAnalysis,
}

#[derive(Copy, Clone, Dupe, Debug, PartialEq)]
pub enum InterpreterHostPlatform {
    Linux,
    MacOS,
    Windows,
}

#[derive(Copy, Clone, Dupe, Debug, PartialEq)]
pub enum InterpreterHostArchitecture {
    AArch64,
    X86_64,
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

    pub(crate) buckconfig: LegacyBuckConfigForStarlark<'a>,

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
        starlark_path: StarlarkPath<'a>,
        listing: Option<PackageListing>,
        host_platform: InterpreterHostPlatform,
        host_architecture: InterpreterHostArchitecture,
        additional: Option<Box<dyn ExtraContextDyn>>,
        ignore_attrs_for_profiling: bool,
    ) -> BuildContext<'a> {
        let buckconfig = LegacyBuckConfigForStarlark::new(module, buckconfig);
        BuildContext {
            cell_info,
            buckconfig,
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

    pub fn require_package(&self) -> anyhow::Result<&Package> {
        match self.starlark_path {
            StarlarkPath::BuildFile(b) => Ok(b.package()),
            _ => Err(anyhow!("package can only be fetched from a build file")),
        }
    }

    pub fn resolve_glob(
        &'a self,
        spec: &'a GlobSpec,
    ) -> anyhow::Result<impl Iterator<Item = &'a PackageRelativePath> + 'a> {
        match &self.listing {
            Some(listing) => Ok(spec.resolve_glob(listing.files())),
            None => Err(anyhow!("glob() can only be called from a build file")),
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
            None => Err(anyhow!(
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
            Err(_) => Err(anyhow!(
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

/// Used to configure the interperter's global functions and state
pub trait InterpreterConfiguror: Sync + Send {
    /// Add additional global values for build files
    fn configure_build_file_globals(&self, globals_builder: &mut GlobalsBuilder);

    /// Add additional global values for extension files
    fn configure_extension_file_globals(&self, globals_builder: &mut GlobalsBuilder);

    /// Add additional global values for bxl files
    fn configure_bxl_file_globals(&self, globals_builder: &mut GlobalsBuilder);

    /// Add any functions necessary to the 'native' struct
    fn configure_native_struct(&self, native_module: &mut GlobalsBuilder);

    fn host_platform(&self) -> InterpreterHostPlatform;

    fn host_architecture(&self) -> InterpreterHostArchitecture;

    /// Creates an 'extra' object that can be used in implementation functions
    fn new_extra_context(
        &self,
        cell_info: &InterpreterCellInfo,
        buildfile_path: BuildFilePath,
        package_listing: PackageListing,
        package_boundary_exception: bool,
        loaded_modules: &LoadedModules,
        implicit_import: Option<&Arc<ImplicitImport>>,
    ) -> SharedResult<Box<dyn ExtraContextDyn>>;

    /// Returns the "prelude" import. This allows the configuror to inject symbols defined in
    /// starlark code into all modules. Buck uses this to add symbols to the "native" module
    /// (by replacing it in the prelude). The shimmed native module includes starlark-defined
    /// "native" rules.
    ///
    /// The configuror needs to be careful to ensure that shimming doesn't introduce any
    /// cycles.
    fn get_prelude_import(&self, import: StarlarkPath) -> Option<&ImportPath>;

    /// Indicates whether a given path is part of the prelude. When a path is part of the prelude,
    /// it will always be loaded using the prelude's cell configuration (and not the loading cell's
    /// configuration).
    fn is_prelude_path(&self, path: &CellPath) -> bool;

    fn eq_token(&self) -> PartialEqAny;
}

impl PartialEq for dyn InterpreterConfiguror {
    fn eq(&self, other: &dyn InterpreterConfiguror) -> bool {
        self.eq_token() == other.eq_token()
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use std::borrow::BorrowMut;
    use std::collections::HashMap;
    use std::sync::Arc;
    use std::sync::Mutex;

    use buck2_common::package_listing::listing::PackageListing;
    use buck2_core::build_file_path::BuildFilePath;
    use buck2_core::cells::cell_path::CellPath;
    use buck2_core::package::Package;
    use buck2_core::result::SharedResult;
    use gazebo::cmp::PartialEqAny;
    use gazebo::prelude::*;
    use serde_json::Map;
    use serde_json::Value;
    use starlark::collections::SmallMap;
    use starlark::environment::GlobalsBuilder;
    use starlark::eval::ParametersParser;
    use starlark::eval::ParametersSpec;
    use starlark::values;
    use starlark::values::function::NativeFunction;

    use crate::common::StarlarkPath;
    use crate::extra::cell_info::InterpreterCellInfo;
    use crate::extra::ExtraContext;
    use crate::extra::ExtraContextDyn;
    use crate::extra::InterpreterConfiguror;
    use crate::extra::InterpreterHostArchitecture;
    use crate::extra::InterpreterHostPlatform;
    use crate::file_loader::LoadedModules;
    use crate::package_imports::ImplicitImport;

    #[derive(Clone, Debug)]
    pub struct TesterEvalResult {
        pub package: Package,
        pub targets: HashMap<String, HashMap<String, String>>,
    }

    #[derive(Clone)]
    pub struct TesterExtraContext {
        package: Package,
        recorder: Arc<Mutex<HashMap<String, HashMap<String, String>>>>,
    }

    #[derive(Clone)]
    pub struct TesterConfiguror {
        pub rules: Vec<String>,
    }

    impl TesterEvalResult {
        pub fn to_json(&self) -> Value {
            let values = self
                .targets
                .iter()
                .map(|(k, v)| {
                    let mapped_v = v
                        .iter()
                        .map(|(inner_k, inner_v)| (inner_k.clone(), Value::from(inner_v.clone())))
                        .collect::<Map<_, _>>();
                    (k.clone(), Value::from(mapped_v))
                })
                .collect::<Map<_, _>>();
            Value::from(values)
        }
    }

    impl From<TesterExtraContext> for TesterEvalResult {
        fn from(context: TesterExtraContext) -> Self {
            let targets = context.recorder.lock().unwrap().clone();
            TesterEvalResult {
                package: context.package,
                targets,
            }
        }
    }

    impl ExtraContext for TesterExtraContext {
        type EvalResult = TesterEvalResult;
    }

    impl TesterConfiguror {
        pub fn new(rules: Vec<String>) -> Arc<TesterConfiguror> {
            Arc::new(TesterConfiguror { rules })
        }
    }

    fn add_builtin(registry: &mut GlobalsBuilder, name: &str) {
        let mut params = ParametersSpec::with_capacity(name.to_owned(), 1);
        let inner_name = name.to_owned();
        params.kwargs();
        let params = params.finish();

        let func = NativeFunction::new(
            move |ctx, mut args: ParametersParser| -> anyhow::Result<values::Value> {
                let map: SmallMap<String, starlark::values::Value<'_>> = args.next("kwargs")?;
                let recorder = TesterExtraContext::from_context(ctx)?;

                let mut vals = HashMap::new();
                for (k, v) in map.into_iter() {
                    vals.insert(k, v.to_str());
                }

                vals.insert("__type__".to_owned(), inner_name.clone());

                let name = vals
                    .get("name")
                    .unwrap_or_else(|| panic!("no name found in {:#?}", vals));

                recorder
                    .recorder
                    .lock()
                    .unwrap()
                    .borrow_mut()
                    .insert(name.clone(), vals);
                Ok(values::Value::new_none())
            },
            params.signature(),
            params,
        );
        registry.set(name, func);
    }

    fn add_builtins(registry: &mut GlobalsBuilder, names: &[String]) {
        for rule in names {
            add_builtin(registry, rule);
        }
    }

    impl InterpreterConfiguror for TesterConfiguror {
        fn configure_build_file_globals(&self, globals_builder: &mut GlobalsBuilder) {
            add_builtins(globals_builder, &self.rules)
        }

        fn configure_extension_file_globals(&self, globals_builder: &mut GlobalsBuilder) {
            add_builtins(globals_builder, &self.rules)
        }

        fn configure_bxl_file_globals(&self, globals_builder: &mut GlobalsBuilder) {
            add_builtins(globals_builder, &self.rules)
        }

        fn configure_native_struct<'a>(&self, native_module: &mut GlobalsBuilder) {
            add_builtins(native_module, &self.rules)
        }

        fn host_platform(&self) -> InterpreterHostPlatform {
            InterpreterHostPlatform::Linux
        }

        fn host_architecture(&self) -> InterpreterHostArchitecture {
            InterpreterHostArchitecture::X86_64
        }

        fn new_extra_context(
            &self,
            _cell_info: &InterpreterCellInfo,
            buildfile_path: BuildFilePath,
            _package_listing: PackageListing,
            _package_boundary_exception: bool,
            _loaded_modules: &LoadedModules,
            _implicit_import: Option<&Arc<ImplicitImport>>,
        ) -> SharedResult<Box<dyn ExtraContextDyn>> {
            Ok(box TesterExtraContext {
                package: buildfile_path.package().dupe(),
                recorder: Arc::new(Mutex::new(HashMap::new())),
            })
        }

        fn get_prelude_import(
            &self,
            _import: StarlarkPath<'_>,
        ) -> Option<&buck2_core::bzl::ImportPath> {
            None
        }

        fn is_prelude_path(&self, _path: &CellPath) -> bool {
            false
        }

        fn eq_token(&self) -> PartialEqAny {
            PartialEqAny::always_false()
        }
    }
}
