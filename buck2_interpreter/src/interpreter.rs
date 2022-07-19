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

use std::collections::HashMap;
use std::sync::Arc;

use anyhow::Context;
use buck2_common::legacy_configs::view::LegacyBuckConfigView;
use buck2_common::legacy_configs::view::LegacyBuckConfigsView;
use buck2_common::package_listing::listing::PackageListing;
use buck2_common::result::SharedResult;
use buck2_common::result::ToSharedResultExt;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::cells::CellAliasResolver;
use buck2_core::cells::CellResolver;
use gazebo::prelude::*;
use starlark::environment::FrozenModule;
use starlark::environment::Globals;
use starlark::environment::GlobalsBuilder;
use starlark::environment::LibraryExtension;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::syntax::AstModule;
use starlark::syntax::Dialect;
use starlark::syntax::DialectTypes;
use thiserror::Error;

use crate::build_defs::register_globals;
use crate::build_defs::register_natives;
use crate::common::BxlFilePath;
use crate::common::OwnedStarlarkModulePath;
use crate::common::StarlarkModulePath;
use crate::common::StarlarkPath;
use crate::extra::cell_info::InterpreterCellInfo;
use crate::extra::BuildContext;
use crate::extra::ExtraContext;
use crate::extra::ExtraContextDyn;
use crate::extra::InterpreterConfiguror;
use crate::file_loader::InterpreterFileLoader;
use crate::file_loader::LoadResolver;
use crate::file_loader::LoadedModules;
use crate::import_paths::ImportPaths;
use crate::package_imports::ImplicitImport;
use crate::parse_import::parse_import;
use crate::starlark_profiler;
use crate::starlark_profiler::StarlarkProfilerInstrumentation;
use crate::starlark_profiler::StarlarkProfilerOrInstrumentation;

/// What type of file are we parsing - a `.bzl` file, `.bxl` file, or a `BUCK`/`TARGETS` file.
impl<'a> StarlarkPath<'a> {
    fn dialect(&self, disable_starlark_types: bool) -> Dialect {
        let buck_dialect: Dialect = Dialect {
            enable_def: false,
            enable_lambda: true,
            enable_load: true,
            enable_keyword_only_arguments: false,
            enable_types: DialectTypes::Disable,
            // FIXME: Would like to make this false
            enable_tabs: true,
            enable_load_reexport: false,
            enable_top_level_stmt: false,
        };
        let bzl_dialect: Dialect = Dialect {
            enable_def: true,
            enable_lambda: true,
            enable_load: true,
            enable_keyword_only_arguments: true,
            enable_types: if disable_starlark_types {
                DialectTypes::ParseOnly
            } else {
                DialectTypes::Enable
            },
            // FIXME: Would like to make this false
            enable_tabs: true,
            enable_load_reexport: false,
            enable_top_level_stmt: true,
        };
        let bxl_dialect: Dialect = Dialect {
            enable_def: true,
            enable_lambda: true,
            enable_load: true,
            enable_keyword_only_arguments: true,
            enable_types: if disable_starlark_types {
                DialectTypes::ParseOnly
            } else {
                DialectTypes::Enable
            },
            enable_tabs: false,
            enable_load_reexport: false,
            enable_top_level_stmt: true,
        };

        match self {
            Self::LoadFile(_) => bzl_dialect,
            Self::BuildFile(_) => buck_dialect,
            Self::BxlFile(_) => bxl_dialect,
        }
    }
}

/// A ParseResult includes the parsed AST and a list of the imported files.
///
/// The imports are under a separate Arc so that that can be shared with
/// the evaluation result (which needs the imports but no longer needs the AST).
#[derive(Debug)]
pub struct ParseResult(pub AstModule, pub Arc<Vec<OwnedStarlarkModulePath>>);

impl ParseResult {
    fn new(
        ast: AstModule,
        implicit_imports: Vec<OwnedStarlarkModulePath>,
        resolver: &dyn LoadResolver,
    ) -> anyhow::Result<Self> {
        let mut loads = implicit_imports;
        for x in ast.loads() {
            loads.push(resolver.resolve_load(x)?);
        }
        Ok(Self(ast, Arc::new(loads)))
    }

    pub fn ast(&self) -> &AstModule {
        &self.0
    }

    pub fn imports(&self) -> &Arc<Vec<OwnedStarlarkModulePath>> {
        &self.1
    }
}

/// Interpreter for build files.
///
/// The Interpreter is responsible for parsing files to an AST and then
/// evaluating that AST. The Interpreter doesn't maintain state or cache results
/// of parsing or loading imports.
pub struct InterpreterForCell {
    config: Arc<InterpreterConfigForCell>,
    import_paths: Arc<ImportPaths>,
}

/// InterpreterConfig contains all the information necessary to interpret build
/// files.
pub struct InterpreterConfigForCell {
    /// Non-cell-specific information.
    global_state: Arc<GlobalInterpreterState>,
    /// Cell-specific alias resolver.
    cell_names: CellAliasResolver,
    /// Log GC.
    verbose_gc: bool,
    /// When true, rule function creates a node with no attributes.
    /// (Which won't work correctly, but useful for profiling of starlark).
    ignore_attrs_for_profiling: bool,
}

/// Information shared across interpreters. Contains no cell-specific
/// information.
pub struct GlobalInterpreterState {
    cell_resolver: CellResolver,

    cell_configs: HashMap<BuildFileCell, InterpreterCellInfo>,

    /// The GlobalEnvironment contains all the globally available symbols
    /// (primarily starlark stdlib and Buck-provided functions) that should
    /// be available in a build file.
    build_file_global_env: Globals,

    /// The GlobalEnvironment contains all the globally available symbols
    /// (primarily starlark stdlib and Buck-provided functions) that should
    /// be available in an extension file.
    extension_file_global_env: Globals,

    /// The GlobalEnvironment contains all the globally available symbols
    /// (primarily starlark stdlib and Buck-provided functions) that should
    /// be available in a bxl file.
    bxl_file_global_env: Globals,

    /// Interpreter Configurer
    configuror: Arc<dyn InterpreterConfiguror>,

    /// Check types in Starlark (or just parse and ignore).
    disable_starlark_types: bool,
}

impl GlobalInterpreterState {
    pub fn new(
        legacy_configs: &dyn LegacyBuckConfigsView,
        cell_resolver: CellResolver,
        interpreter_configuror: Arc<dyn InterpreterConfiguror>,
        disable_starlark_types: bool,
    ) -> anyhow::Result<Self> {
        fn base_env(interpreter_configuror: &Arc<dyn InterpreterConfiguror>) -> GlobalsBuilder {
            let starlark_extensions = [
                LibraryExtension::Abs,
                LibraryExtension::Breakpoint,
                LibraryExtension::Debug,
                LibraryExtension::Dedupe,
                LibraryExtension::EnumType,
                LibraryExtension::Filter,
                LibraryExtension::Json,
                LibraryExtension::Map,
                LibraryExtension::Partial,
                LibraryExtension::Pprint,
                LibraryExtension::Print,
                LibraryExtension::RecordType,
                LibraryExtension::ExperimentalRegex,
                LibraryExtension::StructType,
            ];
            let mut global_env = GlobalsBuilder::extended_by(&starlark_extensions)
                .with(register_globals)
                .with(register_natives);
            global_env.struct_("__internal__", |x| {
                register_natives(x);
                // If `native.` symbols need to be added to the global env, they should be done
                // in `configure_build_file_globals()` or
                // `configure_extension_file_globals()`
                for ext in starlark_extensions {
                    ext.add(x)
                }
                interpreter_configuror.configure_native_struct(x);
            });
            global_env
        }

        // TODO: There should be one of these that also does not have native functions
        // in the global       namespace so that it can be configured per-cell
        let build_file_global_env = base_env(&interpreter_configuror)
            .with(|x| interpreter_configuror.configure_build_file_globals(x))
            .build();
        let extension_file_global_env = base_env(&interpreter_configuror)
            .with(|x| interpreter_configuror.configure_extension_file_globals(x))
            .build();
        let bxl_file_global_env = base_env(&interpreter_configuror)
            .with(|x| interpreter_configuror.configure_bxl_file_globals(x))
            .build();

        let mut cell_configs = HashMap::new();
        for (cell_name, config) in legacy_configs.iter() {
            let cell_instance = cell_resolver.get(cell_name).expect("Should have cell.");
            cell_configs.insert(
                BuildFileCell::new(cell_name.clone()),
                InterpreterCellInfo::new(
                    BuildFileCell::new(cell_name.clone()),
                    config,
                    cell_instance.cell_alias_resolver().dupe(),
                )?,
            );
        }
        Ok(Self {
            cell_resolver,
            cell_configs,
            build_file_global_env,
            extension_file_global_env,
            bxl_file_global_env,
            configuror: interpreter_configuror,
            disable_starlark_types,
        })
    }
}

struct InterpreterLoadResolver {
    config: Arc<InterpreterConfigForCell>,
    loader_path: CellPath,
    loader_file_type: StarlarkFileType,
    build_file_cell: BuildFileCell,
}

enum StarlarkFileType {
    Bzl,
    Bxl,
    Buck,
}

#[derive(Debug, Error)]
enum LoadResolutionError {
    #[error(
        "Cannot load `{0}`. Bxl loads are not allowed from within this context. bxl files can only be loaded from other bxl files."
    )]
    BxlLoadNotAllowed(CellPath),
}

impl LoadResolver for InterpreterLoadResolver {
    fn resolve_load(&self, path: &str) -> anyhow::Result<OwnedStarlarkModulePath> {
        let path = parse_import(&self.config.cell_names, &self.loader_path, path)?;

        // check for bxl files first before checking for prelude.
        // All bxl imports are parsed the same regardless of prelude or not.
        if path.path().extension() == Some("bxl") {
            match self.loader_file_type {
                StarlarkFileType::Bzl | StarlarkFileType::Buck => {
                    return Err(LoadResolutionError::BxlLoadNotAllowed(path).into());
                }
                StarlarkFileType::Bxl => {
                    return Ok(OwnedStarlarkModulePath::BxlFile(BxlFilePath::new(path)?));
                }
            }
        }

        // If importing from the prelude, then do not let that inherit the configuration. This
        // ensures that if you define a UDR outside of the prelude's cell, it gets the same prelude
        // as using the exported rules from the prelude would. This matters notably for identity
        // checks in t-sets, which would fail if we had > 1 copy of the prelude.
        if self.config.global_state.configuror.is_prelude_path(&path) {
            let cell = path.cell().clone();
            return Ok(OwnedStarlarkModulePath::LoadFile(ImportPath::new(
                path,
                BuildFileCell::new(cell),
            )?));
        }

        Ok(OwnedStarlarkModulePath::LoadFile(ImportPath::new(
            path,
            self.build_file_cell.clone(),
        )?))
    }
}

impl InterpreterConfigForCell {
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
    pub fn new(
        cell_names: CellAliasResolver,
        global_state: Arc<GlobalInterpreterState>,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            global_state,
            cell_names,
            verbose_gc: Self::verbose_gc()?,
            ignore_attrs_for_profiling: Self::is_ignore_attrs_for_profiling()?,
        })
    }

    pub fn build_file_global_env(&self) -> &Globals {
        &self.global_state.build_file_global_env
    }

    pub fn extension_file_global_env(&self) -> &Globals {
        &self.global_state.extension_file_global_env
    }

    pub fn bxl_file_global_env(&self) -> &Globals {
        &self.global_state.bxl_file_global_env
    }
}

/// A starlark interpreter.
impl InterpreterForCell {
    pub fn new(
        config: Arc<InterpreterConfigForCell>,
        import_paths: Arc<ImportPaths>,
    ) -> InterpreterForCell {
        InterpreterForCell {
            config,
            import_paths,
        }
    }

    fn create_env(
        &self,
        starlark_path: StarlarkPath<'_>,
        loaded_modules: &LoadedModules,
    ) -> SharedResult<Module> {
        let env = Module::new();

        if let Some(prelude_import) = self.prelude_import(starlark_path) {
            let prelude_env = loaded_modules
                .map
                .get(prelude_import.id())
                .unwrap_or_else(|| {
                    panic!(
                        "Should've had an env for the root import (`{}`).",
                        prelude_import,
                    )
                })
                .env();
            env.import_public_symbols(prelude_env);
            if let StarlarkPath::BuildFile(_) = starlark_path {
                if let Some(native) = prelude_env.get("native") {
                    let native = native.value();
                    for attr in native.dir_attr() {
                        if attr == "to_json" {
                            // Skipping `struct` methods.
                            continue;
                        }
                        if let Some(value) = native.get_attr(&attr, env.heap())? {
                            env.set(&attr, value);
                        } else {
                            // Should not be possible.
                        }
                    }
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
        package_boundary_exception: bool,
        loaded_modules: &LoadedModules,
    ) -> SharedResult<(Module, Box<dyn ExtraContextDyn>)> {
        let internals = self.config.global_state.configuror.new_extra_context(
            self.get_cell_config(build_file.build_file_cell()),
            build_file.clone(),
            package_listing.dupe(),
            package_boundary_exception,
            loaded_modules,
            self.package_import(build_file),
        )?;
        let env = self.create_env(StarlarkPath::BuildFile(build_file), loaded_modules)?;

        if let Some(root_import) = self.root_import() {
            let root_env = loaded_modules
                .map
                .get(root_import.id())
                .unwrap_or_else(|| {
                    panic!(
                        "Should've had an env for the root import (`{}`).",
                        root_import,
                    )
                })
                .env();
            env.import_public_symbols(root_env);
        }

        Ok((env, internals))
    }

    fn get_cell_config(&self, build_file_cell: &BuildFileCell) -> &InterpreterCellInfo {
        self.config
            .global_state
            .cell_configs
            .get(build_file_cell)
            .unwrap_or_else(|| panic!("Should've had cell config for {}", build_file_cell))
    }

    fn load_resolver(&self, current_file_path: StarlarkPath<'_>) -> InterpreterLoadResolver {
        InterpreterLoadResolver {
            config: self.config.dupe(),
            loader_path: current_file_path
                .path()
                .parent()
                .expect("loading file should have parent directory"),
            loader_file_type: match current_file_path {
                StarlarkPath::BuildFile(_) => StarlarkFileType::Buck,
                StarlarkPath::LoadFile(_) => StarlarkFileType::Bzl,
                StarlarkPath::BxlFile(_) => StarlarkFileType::Bxl,
            },
            build_file_cell: current_file_path.build_file_cell().clone(),
        }
    }

    fn package_import(&self, build_file_import: &BuildFilePath) -> Option<&Arc<ImplicitImport>> {
        self.import_paths
            .package_imports
            .get(build_file_import.package())
    }

    fn root_import(&self) -> Option<ImportPath> {
        self.import_paths.root_import.clone()
    }

    fn prelude_import(&self, import: StarlarkPath) -> Option<&ImportPath> {
        self.config
            .global_state
            .configuror
            .get_prelude_import(import)
    }

    /// Parses skylark code to an AST.
    pub fn parse(&self, import: StarlarkPath, content: String) -> anyhow::Result<ParseResult> {
        let project_relative_path = self
            .config
            .global_state
            .cell_resolver
            .resolve_path(&import.path())?;
        let result: anyhow::Result<_> = try {
            let disable_starlark_types = self.config.global_state.disable_starlark_types;
            let ast = AstModule::parse(
                project_relative_path.as_str(),
                content,
                &import.dialect(disable_starlark_types),
            )?;
            let mut implicit_imports = Vec::new();
            if let Some(i) = self.prelude_import(import) {
                implicit_imports.push(OwnedStarlarkModulePath::LoadFile(i.clone()));
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
        result.with_context(|| format!("when parsing `{}`", import))
    }

    pub fn resolve_path(
        &self,
        import: StarlarkPath<'_>,
        import_string: &str,
    ) -> anyhow::Result<OwnedStarlarkModulePath> {
        self.load_resolver(import).resolve_load(import_string)
    }

    fn eval(
        &self,
        env: &Module,
        ast: AstModule,
        import: StarlarkPath<'_>,
        buckconfig: &dyn LegacyBuckConfigView,
        loaded_modules: LoadedModules,
        listing: Option<PackageListing>,
        extra_context: Option<Box<dyn ExtraContextDyn>>,
        profiler: &mut StarlarkProfilerOrInstrumentation,
    ) -> SharedResult<Option<Box<dyn ExtraContextDyn>>> {
        let globals = match import {
            StarlarkPath::BuildFile(_) => self.config.build_file_global_env(),
            StarlarkPath::LoadFile(_) => self.config.extension_file_global_env(),
            StarlarkPath::BxlFile(_) => self.config.bxl_file_global_env(),
        };
        let file_loader =
            InterpreterFileLoader::new(loaded_modules, Arc::new(self.load_resolver(import)));
        let cell_info = self.get_cell_config(import.build_file_cell());
        let host_platform = self.config.global_state.configuror.host_platform();
        let host_architecture = self.config.global_state.configuror.host_architecture();
        let extra = BuildContext::new_for_module(
            env,
            cell_info,
            buckconfig,
            import,
            listing,
            host_platform,
            host_architecture,
            extra_context,
            self.config.ignore_attrs_for_profiling,
        );
        let mut eval = Evaluator::new(env);
        eval.set_loader(&file_loader);
        eval.extra = Some(&extra);
        profiler.initialize(&mut eval);
        if self.config.verbose_gc {
            eval.verbose_gc();
        }
        match eval.eval_module(ast, globals) {
            Ok(_) => {
                profiler
                    .finalize(&mut eval)
                    .context("Profiler finalization failed")?;

                profiler
                    .visit_heap(None)
                    .context("Profiler heap visitation failed")?;

                Ok(extra.additional)
            }
            Err(p) => Err(p.into()),
        }
    }

    /// Evaluates the AST for a parsed module. Loaded modules must contain the loaded
    /// environment for all (transitive) required imports.
    /// Returns the FrozenModule for the module.
    pub fn eval_module(
        &self,
        starlark_path: StarlarkModulePath<'_>,
        buckconfig: &dyn LegacyBuckConfigView,
        ast: AstModule,
        loaded_modules: LoadedModules,
        starlark_profiler_instrumentation: StarlarkProfilerInstrumentation,
    ) -> SharedResult<FrozenModule> {
        let env = self.create_env(starlark_path.into(), &loaded_modules)?;
        self.eval(
            &env,
            ast,
            StarlarkPath::from(starlark_path),
            buckconfig,
            loaded_modules,
            None,
            None,
            &mut StarlarkProfilerOrInstrumentation::new(
                &mut starlark_profiler::Disabled,
                starlark_profiler_instrumentation,
            ),
        )?;
        env.freeze().shared_error()
    }

    /// Evaluates the AST for a parsed build file. Loaded modules must contain the
    /// loaded environment for all (transitive) required imports.
    /// Returns the result of evaluation.
    pub fn eval_build_file<T: ExtraContext + Sized + 'static>(
        &self,
        build_file: &BuildFilePath,
        buckconfig: &dyn LegacyBuckConfigView,
        listing: PackageListing,
        package_boundary_exception: bool,
        ast: AstModule,
        loaded_modules: LoadedModules,
        profiler: &mut StarlarkProfilerOrInstrumentation,
    ) -> SharedResult<T::EvalResult> {
        let (env, internals) = self.create_build_env(
            build_file,
            &listing,
            package_boundary_exception,
            &loaded_modules,
        )?;
        let internals = self
            .eval(
                &env,
                ast,
                StarlarkPath::BuildFile(build_file),
                buckconfig,
                loaded_modules,
                Some(listing),
                Some(internals),
                profiler,
            )?
            .expect("We sent a context, expect one back");

        Ok(T::into_eval_result(internals).expect("The result to match the context type"))
    }
}

#[cfg(test)]
mod tests {
    use buck2_common::legacy_configs::testing::TestConfigParserFileOps;
    use buck2_common::legacy_configs::BuckConfigBasedCells;
    use buck2_common::legacy_configs::LegacyBuckConfig;
    use buck2_common::legacy_configs::LegacyBuckConfigs;
    use buck2_common::package_listing::listing::testing::PackageListingExt;
    use buck2_core::cells::CellName;
    use buck2_core::fs::paths::AbsPathBuf;
    use buck2_core::fs::project::ProjectFilesystem;
    use indexmap::map::IndexMap;
    use indoc::indoc;
    use serde_json::json;

    use super::*;
    use crate::common::OwnedStarlarkModulePath;
    use crate::common::StarlarkModulePath;
    use crate::extra::testing::TesterConfiguror;
    use crate::extra::testing::TesterEvalResult;
    use crate::extra::testing::TesterExtraContext;
    use crate::file_loader::LoadedModule;

    fn cross_cell_import(
        cell: &str,
        package: &str,
        filename: &str,
        build_file_cell: &str,
    ) -> ImportPath {
        ImportPath::unchecked_new_cross_cell(cell, package, filename, build_file_cell)
    }

    fn import(cell: &str, package: &str, filename: &str) -> ImportPath {
        ImportPath::unchecked_new(cell, package, filename)
    }

    fn build(cell: &str, package: &str, filename: &str) -> BuildFilePath {
        BuildFilePath::unchecked_new(cell, package, filename)
    }

    struct Tester {
        cell_alias_resolver: CellAliasResolver,
        cell_resolver: CellResolver,
        configs: LegacyBuckConfigs,
    }

    type CellsData = (CellAliasResolver, CellResolver, LegacyBuckConfigs);

    fn cells(extra_root_config: Option<&str>) -> anyhow::Result<CellsData> {
        let repo_root = if cfg!(windows) { "C:/" } else { "/" };
        let project_fs = ProjectFilesystem::new(AbsPathBuf::unchecked_new(repo_root.to_owned()));
        let BuckConfigBasedCells {
            cell_resolver,
            configs_by_name,
        } = BuckConfigBasedCells::parse_with_file_ops(
            &project_fs,
            &TestConfigParserFileOps::new(&[
                (
                    "/.buckconfig",
                    indoc!(
                        r#"
                    [repositories]
                      root = .
                      cell1 = project/cell1
                      cell2 = project/cell2
                      xalias2 = project/cell2

                    <file:extra_cfg>
                    "#
                    ),
                ),
                ("/extra_cfg", extra_root_config.unwrap_or("")),
                // Just rely on the root's aliases being available everywhere for all the others.
            ])?,
            &[],
            &project_fs.root,
        )?;
        Ok((
            cell_resolver
                .get(&CellName::unchecked_new("root".to_owned()))?
                .cell_alias_resolver()
                .dupe(),
            cell_resolver,
            configs_by_name,
        ))
    }

    impl Tester {
        fn new() -> anyhow::Result<Self> {
            Self::with_cells(cells(None)?)
        }

        fn with_cells(cells_data: CellsData) -> anyhow::Result<Self> {
            let (cell_alias_resolver, cell_resolver, configs) = cells_data;
            Ok(Self {
                cell_alias_resolver,
                cell_resolver,
                configs,
            })
        }

        fn interpreter(&self) -> anyhow::Result<InterpreterForCell> {
            let root_cell = BuildFileCell::new(CellName::unchecked_new("root".to_owned()));
            let import_paths = ImportPaths::parse(
                self.configs.get(root_cell.name()).unwrap(),
                &root_cell,
                &self.cell_alias_resolver,
            )?;
            Ok(InterpreterForCell::new(
                Arc::new(InterpreterConfigForCell::new(
                    self.cell_alias_resolver.dupe(),
                    Arc::new(GlobalInterpreterState::new(
                        &self.configs,
                        self.cell_resolver.dupe(),
                        TesterConfiguror::new(vec![
                            "export_file".to_owned(),
                            "java_library".to_owned(),
                        ]),
                        false,
                    )?),
                )?),
                Arc::new(import_paths),
            ))
        }

        fn eval_module(
            &self,
            path: StarlarkModulePath<'_>,
            content: &str,
            loaded_modules: LoadedModules,
        ) -> anyhow::Result<LoadedModule> {
            let interpreter = self.interpreter()?;
            let ParseResult(ast, _) = interpreter.parse(path.into(), content.to_owned())?;
            let buckconfig = LegacyBuckConfig::empty();
            let env = interpreter.eval_module(
                path,
                &buckconfig,
                ast,
                loaded_modules.clone(),
                StarlarkProfilerInstrumentation::default(),
            )?;
            Ok(LoadedModule::new(
                OwnedStarlarkModulePath::new(path),
                loaded_modules,
                env,
            ))
        }

        fn eval_build_file(
            &self,
            path: &BuildFilePath,
            content: &str,
            loaded_modules: LoadedModules,
            package_listing: PackageListing,
            package_boundary_exception: bool,
        ) -> anyhow::Result<TesterEvalResult> {
            let interpreter = self.interpreter()?;
            let ParseResult(ast, _) =
                interpreter.parse(StarlarkPath::BuildFile(path), content.to_owned())?;
            let buckconfig = LegacyBuckConfig::empty();
            let eval_result = interpreter.eval_build_file::<TesterExtraContext>(
                path,
                &buckconfig,
                package_listing,
                package_boundary_exception,
                ast,
                loaded_modules,
                &mut StarlarkProfilerOrInstrumentation::disabled(),
            )?;
            Ok(eval_result)
        }

        fn parse(&self, import: StarlarkPath<'_>, content: &str) -> anyhow::Result<ParseResult> {
            self.interpreter()?.parse(import, content.to_owned())
        }
    }

    #[test]
    fn test_eval_import() -> anyhow::Result<()> {
        let loaded = Tester::new()?.eval_module(
            StarlarkModulePath::LoadFile(&import("root", "some/package", "defs.bzl")),
            indoc!(
                r#"
            one = 1
            hello = "world"
            "#
            ),
            LoadedModules::default(),
        )?;

        assert_eq!(
            loaded.env().get("one").map(|x| x.unpack_int()),
            Some(Some(1))
        );

        assert_eq!(
            "world",
            loaded.env().get("hello").unwrap().unpack_str().unwrap()
        );

        Ok(())
    }

    #[test]
    fn test_load() -> anyhow::Result<()> {
        let import_path = import("cell1", "imports", "one.bzl");
        let import_result = Tester::new()?.eval_module(
            StarlarkModulePath::LoadFile(&import_path),
            indoc!(
                r#"
                    def concat(*args):
                      s = ""
                      for a in args:
                        s += a
                      return s
                    "#
            ),
            LoadedModules::default(),
        )?;
        let loaded_modules = IndexMap::from_iter([(import_path.id().to_owned(), import_result)]);
        let loaded_modules = LoadedModules {
            map: loaded_modules,
        };

        let parse_result = Tester::new()?.eval_module(
            StarlarkModulePath::LoadFile(&import("cell1", "some/package", "defs.bzl")),
            indoc!(
                r#"
                load("@cell1//imports:one.bzl", "concat")
                message = concat("hello", " ", "world!")
                "#
            ),
            loaded_modules,
        )?;

        assert_eq!(
            "hello world!",
            parse_result
                .env()
                .get("message")
                .unwrap()
                .unpack_str()
                .unwrap()
        );
        Ok(())
    }

    #[test]
    fn test_eval_build_file() -> anyhow::Result<()> {
        let import_path = cross_cell_import("cell1", "imports", "one.bzl", "root");
        let import_result = Tester::new()?.eval_module(
            StarlarkModulePath::LoadFile(&import_path),
            indoc!(
                r#"
                    def some_macro(name, **kwargs):
                        export_file(
                            name=name+"-exported",
                            **kwargs
                        )
                    "#
            ),
            LoadedModules::default(),
        )?;
        let build_path = build("root", "some/package", "BUILD");
        let loaded_modules = IndexMap::from_iter([(import_path.id().to_owned(), import_result)]);
        let loaded_modules = LoadedModules {
            map: loaded_modules,
        };
        let eval_result = Tester::new()?.eval_build_file(
            &build_path,
            indoc!(
                r#"
                load("@cell1//imports:one.bzl", "some_macro")
                some_macro(
                    name = "invoke_some",
                    src = "some.file",
                )
                java_library(
                    name = "java",
                    srcs = glob(["**/*.java"]),
                )
                "#
            ),
            loaded_modules,
            PackageListing::testing_files(&["file1.java", "file2.java"]),
            false,
        )?;

        assert_eq!(build_path.package(), &eval_result.package);
        assert_eq!(
            json!({
                    "invoke_some-exported": {
                        "__type__": "export_file",
                        "name": "invoke_some-exported",
                        "src": "some.file"
                    },
                    "java": {
                        "__type__": "java_library",
                        "name": "java",
                        "srcs": "[\"file1.java\", \"file2.java\"]"
                    }
            }),
            eval_result.to_json()
        );
        Ok(())
    }

    #[test]
    fn test_find_imports() -> anyhow::Result<()> {
        let tester = Tester::new()?;
        let path = build("cell1", "config", "BUCK");
        let parse_result = tester.parse(
            StarlarkPath::BuildFile(&path),
            indoc!(
                r#"
            a = 1
        "#
            ),
        )?;

        assert!(parse_result.imports().is_empty());

        let parse_result = tester.parse(
            StarlarkPath::BuildFile(&path),
            indoc!(
                r#"
            # some documentation
            """ and a string """

            load("//imports:one.bzl", "some_macro")
            load("@cell1//:one.bzl", "some_macro")
            load("@xalias2//:two.bzl", "some_macro")

            # some other comments
            load(":other.bzl", "some_macro")
        "#
            ),
        )?;

        assert_eq!(
            &[
                "root//imports/one.bzl@cell1",
                "cell1//one.bzl",
                "cell2//two.bzl@cell1",
                "cell1//config/other.bzl"
            ],
            parse_result.imports().map(|e| e.to_string()).as_slice()
        );

        Ok(())
    }

    #[test]
    fn test_root_import() -> anyhow::Result<()> {
        let tester = Tester::with_cells(cells(Some(indoc!(
            r#"
            [buildfile]
                includes = //include.bzl
        "#
        )))?)?;

        let import_path = import("root", "", "include.bzl");
        let import_result = tester.eval_module(
            StarlarkModulePath::LoadFile(&import_path),
            indoc!(
                r#"
            some_var = 1
            def some_func():
               return "hello"
        "#
            ),
            LoadedModules::default(),
        )?;
        let mut loaded_modules = IndexMap::new();
        loaded_modules.insert(import_path.id().to_owned(), import_result);
        let loaded_modules = LoadedModules {
            map: loaded_modules,
        };

        let build_path = build("root", "some/package", "BUCK");
        let eval_result = tester.eval_build_file(
            &build_path,
            indoc!(
                r#"
                export_file(
                    name = some_func(),
                    level = some_var,
                )
                "#
            ),
            loaded_modules,
            PackageListing::testing_files(&["file1.java", "file2.java"]),
            false,
        )?;

        assert_eq!(build_path.package(), &eval_result.package);
        assert_eq!(
            json!({
                    "hello": {
                        "__type__": "export_file",
                        "name": "hello",
                        "level": "1"
                    },
            }),
            eval_result.to_json()
        );
        Ok(())
    }
}
