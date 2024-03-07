/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Implements the core skylark interpreter. This encodes the primitive
//! operations of converting file content to ASTs and evaluating import and
//! build files.

use std::cell::OnceCell;
use std::cell::RefCell;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_common::package_listing::listing::PackageListing;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::soft_error;
use buck2_error::BuckErrorContext;
use buck2_event_observer::humanized::HumanizedBytes;
use buck2_events::dispatch::get_dispatcher;
use buck2_interpreter::error::BuckStarlarkError;
use buck2_interpreter::factory::StarlarkEvaluatorProvider;
use buck2_interpreter::file_loader::InterpreterFileLoader;
use buck2_interpreter::file_loader::LoadResolver;
use buck2_interpreter::file_loader::LoadedModules;
use buck2_interpreter::file_type::StarlarkFileType;
use buck2_interpreter::import_paths::ImplicitImportPaths;
use buck2_interpreter::package_imports::ImplicitImport;
use buck2_interpreter::parse_import::parse_import;
use buck2_interpreter::paths::bxl::BxlFilePath;
use buck2_interpreter::paths::module::OwnedStarlarkModulePath;
use buck2_interpreter::paths::module::StarlarkModulePath;
use buck2_interpreter::paths::package::PackageFilePath;
use buck2_interpreter::paths::path::OwnedStarlarkPath;
use buck2_interpreter::paths::path::StarlarkPath;
use buck2_interpreter::prelude_path::PreludePath;
use buck2_interpreter::print_handler::EventDispatcherPrintHandler;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::eval_result::EvaluationResultWithStats;
use buck2_node::super_package::SuperPackage;
use buck2_util::per_thread_instruction_counter::PerThreadInstructionCounter;
use dupe::Dupe;
use gazebo::prelude::*;
use starlark::codemap::FileSpan;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::syntax::AstModule;
use starlark::values::OwnedFrozenRef;

use crate::interpreter::buckconfig::BuckConfigsViewForStarlark;
use crate::interpreter::build_context::BuildContext;
use crate::interpreter::build_context::PerFileTypeContext;
use crate::interpreter::bzl_eval_ctx::BzlEvalCtx;
use crate::interpreter::cell_info::InterpreterCellInfo;
use crate::interpreter::extra_value::InterpreterExtraValue;
use crate::interpreter::global_interpreter_state::GlobalInterpreterState;
use crate::interpreter::module_internals::ModuleInternals;
use crate::interpreter::package_file_extra::FrozenPackageFileExtra;
use crate::super_package::eval_ctx::PackageFileEvalCtx;

#[derive(Debug, buck2_error::Error)]
#[error("Tabs are not allowed in Buck files: `{0}`")]
struct StarlarkTabsError(OwnedStarlarkPath);

#[derive(Debug, buck2_error::Error)]
enum StarlarkPeakMemoryError {
    #[error(
        "Starlark peak memory usage for {0} is {1} which exceeds the limit {2}! Please reduce memory usage to prevent OOMs. See {3} for debugging tips."
    )]
    #[buck2(user)]
    ExceedsThreshold(BuildFilePath, HumanizedBytes, HumanizedBytes, String),
}

#[derive(Debug, buck2_error::Error)]
enum StarlarkPeakMemorySoftError {
    #[error(
        "Starlark peak memory usage for {0} is {1} which is over 50% of the limit {2}! Consider investigating what takes too much memory: {3}."
    )]
    CloseToThreshold(BuildFilePath, HumanizedBytes, HumanizedBytes, String),
}

#[derive(Debug, buck2_error::Error)]
#[error("Error parsing: `{1}`")]
pub struct ParseError(#[source] pub BuckStarlarkError, OwnedStarlarkPath);

/// A ParseData includes the parsed AST and a list of the imported files.
///
/// The imports are under a separate Arc so that that can be shared with
/// the evaluation result (which needs the imports but no longer needs the AST).
pub struct ParseData(
    pub AstModule,
    pub Arc<Vec<(Option<FileSpan>, OwnedStarlarkModulePath)>>,
);

pub type ParseResult = Result<ParseData, ParseError>;

impl ParseData {
    fn new(
        ast: AstModule,
        implicit_imports: Vec<OwnedStarlarkModulePath>,
        resolver: &dyn LoadResolver,
    ) -> anyhow::Result<Self> {
        let mut loads = implicit_imports.into_map(|x| (None, x));
        for x in ast.loads() {
            let path = resolver
                .resolve_load(x.module_id, Some(&x.span))
                .with_context(|| {
                    format!(
                        "Error loading `load` of `{}` from `{}`",
                        x.module_id, x.span
                    )
                })?;
            loads.push((Some(x.span), path));
        }
        Ok(Self(ast, Arc::new(loads)))
    }

    pub fn ast(&self) -> &AstModule {
        &self.0
    }

    pub fn imports(&self) -> &Arc<Vec<(Option<FileSpan>, OwnedStarlarkModulePath)>> {
        &self.1
    }
}

pub fn get_starlark_warning_link() -> &'static str {
    if buck2_core::is_open_source() {
        "https://buck2.build/docs/users/faq/starlark_peak_mem"
    } else {
        "https://fburl.com/starlark_peak_mem_warning"
    }
}
/// Interpreter for build files.
///
/// The Interpreter is responsible for parsing files to an AST and then
/// evaluating that AST. The Interpreter doesn't maintain state or cache results
/// of parsing or loading imports.
#[derive(Allocative)]
pub(crate) struct InterpreterForCell {
    /// Non-cell-specific information.
    global_state: Arc<GlobalInterpreterState>,
    /// Cell-specific alias resolver.
    cell_info: InterpreterCellInfo,
    /// Log GC.
    verbose_gc: bool,
    /// When true, rule function creates a node with no attributes.
    /// (Which won't work correctly, but useful for profiling of starlark).
    ignore_attrs_for_profiling: bool,
    /// Implicit imports. These are only used for build files (e.g. `BUCK`),
    /// not for `bzl` or other files, because we only have implicit imports for build files.
    implicit_import_paths: Arc<ImplicitImportPaths>,
}

struct InterpreterLoadResolver {
    config: Arc<InterpreterForCell>,
    loader_path: CellPath,
    loader_file_type: StarlarkFileType,
    build_file_cell: BuildFileCell,
}

#[derive(Debug, buck2_error::Error)]
enum LoadResolutionError {
    #[error(
        "Cannot load `{0}`. Bxl loads are not allowed from within this context. bxl files can only be loaded from other bxl files."
    )]
    BxlLoadNotAllowed(CellPath),
    #[error("The `load` at {location} of `{got}` should use the canonical name `{wanted}`")]
    WrongCell {
        got: CellPath,
        wanted: CellPath,
        location: String,
    },
}

impl LoadResolver for InterpreterLoadResolver {
    fn resolve_load(
        &self,
        path: &str,
        location: Option<&FileSpan>,
    ) -> anyhow::Result<OwnedStarlarkModulePath> {
        // This is to be removed when we finish migration to Buck2.
        let path = path.trim_end_match("?v2_only");

        let path = parse_import(
            &self.config.cell_info.cell_alias_resolver(),
            &self.loader_path,
            path,
        )?;

        // check for bxl files first before checking for prelude.
        // All bxl imports are parsed the same regardless of prelude or not.
        if path.path().extension() == Some("bxl") {
            match self.loader_file_type {
                StarlarkFileType::Bzl | StarlarkFileType::Buck | StarlarkFileType::Package => {
                    return Err(LoadResolutionError::BxlLoadNotAllowed(path).into());
                }
                StarlarkFileType::Bxl => {
                    return Ok(OwnedStarlarkModulePath::BxlFile(BxlFilePath::new(path)?));
                }
            }
        }

        // If you load the same .bzl file twice via different aliases (e.g. fbcode//buck2/prelude/foo.bzl and prelude.bzl)
        // then anything doing pointer equality (t-sets, provider identities) will go wrong.
        let project_path = self
            .config
            .global_state
            .cell_resolver
            .resolve_path(path.as_ref())?;
        let reformed_path = self
            .config
            .global_state
            .cell_resolver
            .get_cell_path(&project_path)?;
        if reformed_path.cell() != path.cell() {
            // We actually call resolve_load twice for each loadable - once with all load's up front,
            // then again on each one when we are loading. The second time we don't have a location,
            // so just omit the soft_error that time. Once it is a real error, we should real error on either.
            if let Some(location) = location {
                return Err(LoadResolutionError::WrongCell {
                    got: path,
                    wanted: reformed_path,
                    location: location.to_string(),
                }
                .into());
            }
        }

        // If importing from the prelude, then do not let that inherit the configuration. This
        // ensures that if you define a UDR outside of the prelude's cell, it gets the same prelude
        // as using the exported rules from the prelude would. This matters notably for identity
        // checks in t-sets, which would fail if we had > 1 copy of the prelude.
        if let Some(prelude_import) = self.config.global_state.configuror.prelude_import() {
            if prelude_import.is_prelude_path(&path) {
                return Ok(OwnedStarlarkModulePath::LoadFile(
                    ImportPath::new_same_cell(path)?,
                ));
            }
        }

        Ok(OwnedStarlarkModulePath::LoadFile(
            ImportPath::new_with_build_file_cells(path, self.build_file_cell)?,
        ))
    }
}

struct EvalResult {
    additional: PerFileTypeContext,
    starlark_peak_allocated_byte_limit: OnceCell<Option<u64>>,
    is_profiling_enabled: bool,
    cpu_instruction_count: Option<u64>,
}

impl InterpreterForCell {
    fn verbose_gc() -> anyhow::Result<bool> {
        match std::env::var_os("BUCK2_STARLARK_VERBOSE_GC") {
            Some(val) => Ok(!val.is_empty()),
            None => Ok(false),
        }
    }

    fn is_ignore_attrs_for_profiling() -> anyhow::Result<bool> {
        // If unsure, feel free to break this code or just delete it.
        // It is intended only for profiling of very specific use cases.
        let ignore_attrs_for_profiling = match std::env::var_os("BUCK2_IGNORE_ATTRS_FOR_PROFILING")
        {
            Some(val) => !val.is_empty(),
            None => false,
        };
        if ignore_attrs_for_profiling {
            // This messages is printed in each run once per cell.
            // Somewhat inconvenient, but it is safe.
            eprintln!("Ignoring rule attributes");
        }
        Ok(ignore_attrs_for_profiling)
    }

    //, configuror: Arc<dyn InterpreterConfigurer>
    pub(crate) fn new(
        cell_info: InterpreterCellInfo,
        global_state: Arc<GlobalInterpreterState>,
        implicit_import_paths: Arc<ImplicitImportPaths>,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            global_state,
            cell_info,
            verbose_gc: Self::verbose_gc()?,
            ignore_attrs_for_profiling: Self::is_ignore_attrs_for_profiling()?,
            implicit_import_paths,
        })
    }

    fn create_env(
        &self,
        starlark_path: StarlarkPath<'_>,
        loaded_modules: &LoadedModules,
    ) -> anyhow::Result<Module> {
        let env = Module::new();

        if let Some(prelude_import) = self.prelude_import(starlark_path) {
            let prelude_env = loaded_modules
                .map
                .get(&StarlarkModulePath::LoadFile(prelude_import.import_path()))
                .with_internal_error(|| {
                    format!(
                        "Should've had an env for the prelude import `{}`",
                        prelude_import,
                    )
                })?;
            env.import_public_symbols(prelude_env.env());
            if let StarlarkPath::BuildFile(_) = starlark_path {
                for (name, value) in prelude_env.extra_globals_from_prelude_for_buck_files()? {
                    env.set(name, value.to_value());
                }
            }
        }

        env.set_extra_value_no_overwrite(
            env.heap().alloc_complex(InterpreterExtraValue::default()),
        )?;

        Ok(env)
    }

    // The environment for evaluating a build file contains additional information
    // to support the extra things available in that context. For example, rule
    // functions can be invoked when evaluating a build file, the package (cell
    // + path) is available. It also includes the implicit root include and
    // implicit package include.
    fn create_build_env(
        &self,
        build_file: &BuildFilePath,
        package_listing: &PackageListing,
        super_package: SuperPackage,
        package_boundary_exception: bool,
        loaded_modules: &LoadedModules,
    ) -> anyhow::Result<(Module, ModuleInternals)> {
        let internals = self.global_state.configuror.new_extra_context(
            &self.cell_info,
            build_file.clone(),
            package_listing.dupe(),
            super_package,
            package_boundary_exception,
            loaded_modules,
            self.package_import(build_file),
        )?;
        let env = self.create_env(StarlarkPath::BuildFile(build_file), loaded_modules)?;

        if let Some(root_import) = self.root_import() {
            let root_env = loaded_modules
                .map
                .get(&StarlarkModulePath::LoadFile(&root_import))
                .with_internal_error(|| {
                    format!("Should've had an env for the root import `{}`", root_import,)
                })?
                .env();
            env.import_public_symbols(root_env);
        }

        Ok((env, internals))
    }

    fn load_resolver(
        self: &Arc<Self>,
        current_file_path: StarlarkPath<'_>,
    ) -> InterpreterLoadResolver {
        InterpreterLoadResolver {
            config: self.dupe(),
            loader_path: current_file_path
                .path()
                .parent()
                .expect("loading file should have parent directory")
                .to_owned(),
            loader_file_type: current_file_path.file_type(),
            build_file_cell: current_file_path.build_file_cell(),
        }
    }

    fn package_import(&self, build_file_import: &BuildFilePath) -> Option<&Arc<ImplicitImport>> {
        self.implicit_import_paths
            .package_imports
            .get(build_file_import.package())
    }

    fn root_import(&self) -> Option<ImportPath> {
        self.implicit_import_paths.root_import.clone()
    }

    fn prelude_import(&self, import: StarlarkPath) -> Option<&PreludePath> {
        let prelude_import = self.global_state.configuror.prelude_import();
        if let Some(prelude_import) = prelude_import {
            let import_path = import.path();

            match import {
                StarlarkPath::BuildFile(_)
                | StarlarkPath::PackageFile(_)
                | StarlarkPath::BxlFile(_) => return Some(prelude_import),
                StarlarkPath::LoadFile(_) => {
                    if !prelude_import.is_prelude_path(&import_path) {
                        return Some(prelude_import);
                    }
                }
            }
        }

        None
    }

    /// Parses skylark code to an AST.
    pub(crate) fn parse(
        self: &Arc<Self>,
        import: StarlarkPath,
        content: String,
    ) -> anyhow::Result<ParseResult> {
        // Indentation with tabs is prohibited by starlark spec and configured starlark dialect.
        // This check also prohibits tabs even where spaces are not significant,
        // for example inside parentheses in function call arguments,
        // which restricts what the spec allows.
        if content.contains('\t') {
            return Err(StarlarkTabsError(OwnedStarlarkPath::new(import)).into());
        }

        let project_relative_path = self
            .global_state
            .cell_resolver
            .resolve_path(import.path().as_ref().as_ref())?;

        let disable_starlark_types = self.global_state.disable_starlark_types;
        let ast = match AstModule::parse(
            project_relative_path.as_str(),
            content,
            &import.file_type().dialect(disable_starlark_types),
        ) {
            Ok(ast) => ast,
            Err(e) => {
                return Ok(Err(ParseError(
                    BuckStarlarkError::new(e),
                    OwnedStarlarkPath::new(import),
                )));
            }
        };
        let mut implicit_imports = Vec::new();
        if let Some(i) = self.prelude_import(import) {
            implicit_imports.push(OwnedStarlarkModulePath::LoadFile(i.import_path().clone()));
        }
        if let StarlarkPath::BuildFile(build_file) = import {
            if let Some(i) = self.package_import(build_file) {
                implicit_imports.push(OwnedStarlarkModulePath::LoadFile(i.import().clone()));
            }
            if let StarlarkPath::BuildFile(build_file) = import {
                if let Some(i) = self.package_import(build_file) {
                    implicit_imports.push(OwnedStarlarkModulePath::LoadFile(i.import().clone()));
                }
                if let Some(i) = self.root_import() {
                    implicit_imports.push(OwnedStarlarkModulePath::LoadFile(i));
                }
            }
        }
        ParseData::new(ast, implicit_imports, &self.load_resolver(import)).map(Ok)
    }

    pub(crate) fn resolve_path(
        self: &Arc<Self>,
        import: StarlarkPath<'_>,
        import_string: &str,
    ) -> anyhow::Result<OwnedStarlarkModulePath> {
        self.load_resolver(import).resolve_load(import_string, None)
    }

    fn eval(
        self: &Arc<Self>,
        env: &Module,
        ast: AstModule,
        buckconfigs: &mut dyn BuckConfigsViewForStarlark,
        loaded_modules: LoadedModules,
        extra_context: PerFileTypeContext,
        eval_provider: &mut dyn StarlarkEvaluatorProvider,
        unstable_typecheck: bool,
    ) -> anyhow::Result<EvalResult> {
        let import = extra_context.starlark_path();
        let globals = self
            .global_state
            .globals_for_file_type(extra_context.file_type());
        let file_loader =
            InterpreterFileLoader::new(loaded_modules, Arc::new(self.load_resolver(import)));
        let host_info = self.global_state.configuror.host_info();
        let extra = BuildContext::new_for_module(
            env,
            &self.cell_info,
            buckconfigs,
            host_info,
            extra_context,
            self.ignore_attrs_for_profiling,
        );
        let is_profiling_enabled;
        let print = EventDispatcherPrintHandler(get_dispatcher());
        let cpu_instruction_count = {
            let (mut eval, is_profiling_enabled_by_provider) = eval_provider.make(env)?;
            is_profiling_enabled = is_profiling_enabled_by_provider;
            eval.enable_static_typechecking(unstable_typecheck);
            eval.set_print_handler(&print);
            eval.set_loader(&file_loader);
            eval.extra = Some(&extra);
            if self.verbose_gc {
                eval.verbose_gc();
            }

            // Ignore error if failed to initialize instruction counter.
            let instruction_counter: Option<PerThreadInstructionCounter> =
                PerThreadInstructionCounter::init().ok().unwrap_or_default();

            match eval.eval_module(ast, globals) {
                Ok(_) => {
                    let cpu_instruction_count = instruction_counter.and_then(|c| c.collect().ok());

                    eval_provider
                        .evaluation_complete(&mut eval)
                        .context("Profiler finalization failed")?;
                    eval_provider
                        .visit_frozen_module(None)
                        .context("Profiler heap visitation failed")?;

                    cpu_instruction_count
                }
                Err(p) => return Err(BuckStarlarkError::new(p).into()),
            }
        };
        Ok(EvalResult {
            additional: extra.additional,
            is_profiling_enabled,
            starlark_peak_allocated_byte_limit: extra.starlark_peak_allocated_byte_limit,
            cpu_instruction_count,
        })
    }

    /// Evaluates the AST for a parsed module. Loaded modules must contain the loaded
    /// environment for all (transitive) required imports.
    /// Returns the FrozenModule for the module.
    pub(crate) fn eval_module(
        self: &Arc<Self>,
        starlark_path: StarlarkModulePath<'_>,
        buckconfigs: &mut dyn BuckConfigsViewForStarlark,
        ast: AstModule,
        loaded_modules: LoadedModules,
        eval_provider: &mut dyn StarlarkEvaluatorProvider,
    ) -> anyhow::Result<FrozenModule> {
        let env = self.create_env(starlark_path.into(), &loaded_modules)?;
        let extra_context = match starlark_path {
            StarlarkModulePath::LoadFile(bzl) => PerFileTypeContext::Bzl(BzlEvalCtx {
                bzl_path: bzl.clone(),
            }),
            StarlarkModulePath::BxlFile(bxl) => PerFileTypeContext::Bxl(bxl.clone()),
        };
        let typecheck = self.global_state.unstable_typecheck
            || matches!(starlark_path, StarlarkModulePath::BxlFile(..))
            || match self.global_state.configuror.prelude_import() {
                Some(prelude_import) => {
                    prelude_import.prelude_cell()
                        == self.cell_info.cell_alias_resolver().resolve_self()
                }
                None => false,
            };
        self.eval(
            &env,
            ast,
            buckconfigs,
            loaded_modules,
            extra_context,
            eval_provider,
            typecheck,
        )?;
        env.freeze()
    }

    pub(crate) fn eval_package_file(
        self: &Arc<Self>,
        package_file_path: &PackageFilePath,
        ast: AstModule,
        parent: SuperPackage,
        buckconfigs: &mut dyn BuckConfigsViewForStarlark,
        loaded_modules: LoadedModules,
        eval_provider: &mut dyn StarlarkEvaluatorProvider,
    ) -> anyhow::Result<SuperPackage> {
        let env = self.create_env(
            StarlarkPath::PackageFile(package_file_path),
            &loaded_modules,
        )?;

        let extra_context = PerFileTypeContext::Package(PackageFileEvalCtx {
            path: package_file_path.clone(),
            parent,
            visibility: RefCell::new(None),
        });

        let per_file_context = self
            .eval(
                &env,
                ast,
                buckconfigs,
                loaded_modules,
                extra_context,
                eval_provider,
                false,
            )?
            .additional;

        let extra: Option<OwnedFrozenRef<FrozenPackageFileExtra>> =
            if InterpreterExtraValue::get(&env)?
                .package_extra
                .get()
                .is_some()
            {
                // Only freeze if there's something to freeze, otherwise we will needlessly freeze
                // globals. TODO(nga): add API to only freeze extra.
                let env = env.freeze()?;
                FrozenPackageFileExtra::get(&env)?
            } else {
                None
            };

        let package_file_eval_ctx = per_file_context.into_package_file()?;

        package_file_eval_ctx.build_super_package(extra)
    }

    /// Evaluates the AST for a parsed build file. Loaded modules must contain the
    /// loaded environment for all (transitive) required imports.
    /// Returns the result of evaluation.
    pub(crate) fn eval_build_file(
        self: &Arc<Self>,
        build_file: &BuildFilePath,
        buckconfigs: &mut dyn BuckConfigsViewForStarlark,
        listing: PackageListing,
        super_package: SuperPackage,
        package_boundary_exception: bool,
        ast: AstModule,
        loaded_modules: LoadedModules,
        eval_provider: &mut dyn StarlarkEvaluatorProvider,
        unstable_typecheck: bool,
    ) -> anyhow::Result<EvaluationResultWithStats> {
        let (env, internals) = self.create_build_env(
            build_file,
            &listing,
            super_package,
            package_boundary_exception,
            &loaded_modules,
        )?;
        let eval_result = self.eval(
            &env,
            ast,
            buckconfigs,
            loaded_modules,
            PerFileTypeContext::Build(internals),
            eval_provider,
            unstable_typecheck,
        )?;

        let internals = eval_result.additional.into_build()?;
        let starlark_peak_allocated_bytes = env.heap().peak_allocated_bytes() as u64;
        let starlark_peak_mem_check_enabled = !eval_result.is_profiling_enabled
            && LegacyBuckConfig::parse_value(
                "buck2",
                "check_starlark_peak_memory",
                buckconfigs
                    .read_root_cell_config("buck2", "check_starlark_peak_memory")?
                    .as_deref(),
            )?
            .unwrap_or(false);
        let default_limit = 2 * (1 << 30);
        let starlark_mem_limit = eval_result
            .starlark_peak_allocated_byte_limit
            .get()
            .map_or(default_limit, |opt| opt.unwrap_or(default_limit));

        if starlark_peak_mem_check_enabled && starlark_peak_allocated_bytes > starlark_mem_limit {
            Err(StarlarkPeakMemoryError::ExceedsThreshold(
                build_file.to_owned(),
                HumanizedBytes::fixed_width(starlark_peak_allocated_bytes),
                HumanizedBytes::fixed_width(starlark_mem_limit),
                get_starlark_warning_link().to_owned(),
            )
            .into())
        } else if starlark_peak_mem_check_enabled
            && starlark_peak_allocated_bytes > starlark_mem_limit / 2
        {
            soft_error!(
                "starlark_memory_usage_over_soft_limit",
                StarlarkPeakMemorySoftError::CloseToThreshold(
                    build_file.clone(),
                    HumanizedBytes::fixed_width(starlark_peak_allocated_bytes),
                    HumanizedBytes::fixed_width(starlark_mem_limit),
                    get_starlark_warning_link().to_owned()
                ).into(), quiet: true
            )?;

            Ok(EvaluationResultWithStats {
                result: EvaluationResult::from(internals),
                starlark_peak_allocated_bytes,
                cpu_instruction_count: eval_result.cpu_instruction_count,
            })
        } else {
            Ok(EvaluationResultWithStats {
                result: EvaluationResult::from(internals),
                starlark_peak_allocated_bytes,
                cpu_instruction_count: eval_result.cpu_instruction_count,
            })
        }
    }
}
