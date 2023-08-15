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

use std::cell::RefCell;
use std::sync::Arc;

use allocative::Allocative;
use anyhow::Context;
use buck2_common::legacy_configs::view::LegacyBuckConfigView;
use buck2_common::package_listing::listing::PackageListing;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellAliasResolver;
use buck2_events::dispatch::get_dispatcher;
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
use buck2_node::super_package::SuperPackage;
use dupe::Dupe;
use gazebo::prelude::*;
use starlark::codemap::FileSpan;
use starlark::environment::FrozenModule;
use starlark::environment::Module;
use starlark::syntax::AstModule;
use starlark_map::small_map::SmallMap;
use thiserror::Error;

use crate::interpreter::build_context::BuildContext;
use crate::interpreter::build_context::PerFileTypeContext;
use crate::interpreter::bzl_eval_ctx::BzlEvalCtx;
use crate::interpreter::cell_info::InterpreterCellInfo;
use crate::interpreter::global_interpreter_state::GlobalInterpreterState;
use crate::interpreter::module_internals::ModuleInternals;
use crate::super_package::eval_ctx::PackageFileEvalCtx;

#[derive(Debug, Error)]
enum StarlarkParseError {
    #[error("Error parsing: `{0}`")]
    InFile(OwnedStarlarkPath),
    #[error("Tabs are not allowed in Buck files: `{0}`")]
    Tabs(OwnedStarlarkPath),
}

/// A ParseResult includes the parsed AST and a list of the imported files.
///
/// The imports are under a separate Arc so that that can be shared with
/// the evaluation result (which needs the imports but no longer needs the AST).
#[derive(Debug)]
pub struct ParseResult(
    pub AstModule,
    pub Arc<Vec<(Option<FileSpan>, OwnedStarlarkModulePath)>>,
);

impl ParseResult {
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
    cell_names: CellAliasResolver,
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

#[derive(Debug, Error)]
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

        let path = parse_import(&self.config.cell_names, &self.loader_path, path)?;

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
        cell_names: CellAliasResolver,
        global_state: Arc<GlobalInterpreterState>,
        implicit_import_paths: Arc<ImplicitImportPaths>,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            global_state,
            cell_names,
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
                .with_context(|| {
                    format!(
                        "Should've had an env for the prelude import `{}` (internal error)",
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
            self.get_cell_config(build_file.build_file_cell()),
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
                .with_context(|| {
                    format!(
                        "Should've had an env for the root import `{}` (internal error)",
                        root_import,
                    )
                })?
                .env();
            env.import_public_symbols(root_env);
        }

        Ok((env, internals))
    }

    fn get_cell_config(&self, build_file_cell: BuildFileCell) -> &InterpreterCellInfo {
        self.global_state
            .cell_configs
            .get(&build_file_cell)
            .unwrap_or_else(|| panic!("Should've had cell config for {}", build_file_cell))
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
                StarlarkPath::BuildFile(_) => return Some(prelude_import),
                StarlarkPath::PackageFile(_)
                | StarlarkPath::BxlFile(_)
                | StarlarkPath::LoadFile(_) => {
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
            return Err(StarlarkParseError::Tabs(OwnedStarlarkPath::new(import)).into());
        }

        let project_relative_path = self
            .global_state
            .cell_resolver
            .resolve_path(import.path().as_ref().as_ref())?;
        let result: anyhow::Result<_> = try {
            let disable_starlark_types = self.global_state.disable_starlark_types;
            let ast = AstModule::parse(
                project_relative_path.as_str(),
                content,
                &import.file_type().dialect(disable_starlark_types),
            )?;
            let mut implicit_imports = Vec::new();
            if let Some(i) = self.prelude_import(import) {
                implicit_imports.push(OwnedStarlarkModulePath::LoadFile(i.import_path().clone()));
            }
            if let StarlarkPath::BuildFile(build_file) = import {
                if let Some(i) = self.package_import(build_file) {
                    implicit_imports.push(OwnedStarlarkModulePath::LoadFile(i.import().clone()));
                }
                if let Some(i) = self.root_import() {
                    implicit_imports.push(OwnedStarlarkModulePath::LoadFile(i));
                }
            }
            ParseResult::new(ast, implicit_imports, &self.load_resolver(import))?
        };
        result.with_context(|| StarlarkParseError::InFile(OwnedStarlarkPath::new(import)))
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
        buckconfig: &dyn LegacyBuckConfigView,
        root_buckconfig: &dyn LegacyBuckConfigView,
        loaded_modules: LoadedModules,
        extra_context: PerFileTypeContext,
        eval_provider: &mut dyn StarlarkEvaluatorProvider,
        unstable_typecheck: bool,
    ) -> anyhow::Result<PerFileTypeContext> {
        let import = extra_context.starlark_path();
        let globals = self
            .global_state
            .globals_for_file_type(extra_context.file_type());
        let file_loader =
            InterpreterFileLoader::new(loaded_modules, Arc::new(self.load_resolver(import)));
        let cell_info = self.get_cell_config(import.build_file_cell());
        let host_info = self.global_state.configuror.host_info();
        let extra = BuildContext::new_for_module(
            env,
            cell_info,
            buckconfig,
            root_buckconfig,
            host_info,
            extra_context,
            self.ignore_attrs_for_profiling,
        );
        let print = EventDispatcherPrintHandler(get_dispatcher());
        {
            let mut eval = eval_provider.make(env)?;
            eval.enable_static_typechecking(unstable_typecheck);
            eval.set_print_handler(&print);
            eval.set_loader(&file_loader);
            eval.extra = Some(&extra);
            if self.verbose_gc {
                eval.verbose_gc();
            }
            match eval.eval_module(ast, globals) {
                Ok(_) => {
                    eval_provider
                        .evaluation_complete(&mut eval)
                        .context("Profiler finalization failed")?;
                    eval_provider
                        .visit_frozen_module(None)
                        .context("Profiler heap visitation failed")?
                }
                Err(p) => return Err(p),
            }
        };
        Ok(extra.additional)
    }

    /// Evaluates the AST for a parsed module. Loaded modules must contain the loaded
    /// environment for all (transitive) required imports.
    /// Returns the FrozenModule for the module.
    pub(crate) fn eval_module(
        self: &Arc<Self>,
        starlark_path: StarlarkModulePath<'_>,
        buckconfig: &dyn LegacyBuckConfigView,
        root_buckconfig: &dyn LegacyBuckConfigView,
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
        self.eval(
            &env,
            ast,
            buckconfig,
            root_buckconfig,
            loaded_modules,
            extra_context,
            eval_provider,
            self.global_state.unstable_typecheck,
        )?;
        env.freeze()
    }

    pub(crate) fn eval_package_file(
        self: &Arc<Self>,
        package_file_path: &PackageFilePath,
        ast: AstModule,
        parent: SuperPackage,
        buckconfig: &dyn LegacyBuckConfigView,
        root_buckconfig: &dyn LegacyBuckConfigView,
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
            package_values: RefCell::new(SmallMap::new()),
            visibility: RefCell::new(None),
        });

        let per_file_context = self.eval(
            &env,
            ast,
            buckconfig,
            root_buckconfig,
            loaded_modules,
            extra_context,
            eval_provider,
            false,
        )?;

        let package_file_eval_ctx = per_file_context.into_package_file()?;

        Ok(package_file_eval_ctx.build_super_package())
    }

    /// Evaluates the AST for a parsed build file. Loaded modules must contain the
    /// loaded environment for all (transitive) required imports.
    /// Returns the result of evaluation.
    pub(crate) fn eval_build_file(
        self: &Arc<Self>,
        build_file: &BuildFilePath,
        buckconfig: &dyn LegacyBuckConfigView,
        root_buckconfig: &dyn LegacyBuckConfigView,
        listing: PackageListing,
        super_package: SuperPackage,
        package_boundary_exception: bool,
        ast: AstModule,
        loaded_modules: LoadedModules,
        eval_provider: &mut dyn StarlarkEvaluatorProvider,
    ) -> anyhow::Result<EvaluationResult> {
        let (env, internals) = self.create_build_env(
            build_file,
            &listing,
            super_package,
            package_boundary_exception,
            &loaded_modules,
        )?;
        let internals = self
            .eval(
                &env,
                ast,
                buckconfig,
                root_buckconfig,
                loaded_modules,
                PerFileTypeContext::Build(internals),
                eval_provider,
                false,
            )?
            .into_build()?;

        Ok(EvaluationResult::from(internals))
    }
}
