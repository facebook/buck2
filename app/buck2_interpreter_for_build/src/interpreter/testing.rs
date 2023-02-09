/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use buck2_common::legacy_configs::testing::TestConfigParserFileOps;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_common::legacy_configs::LegacyBuckConfigs;
use buck2_common::package_listing::listing::testing::PackageListingExt;
use buck2_common::package_listing::listing::PackageListing;
use buck2_common::result::SharedResult;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::name::CellName;
use buck2_core::cells::*;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter::file_loader::LoadedModules;
use buck2_interpreter::global_interpreter_state::GlobalInterpreterState;
use buck2_interpreter::import_paths::ImplicitImportPaths;
use buck2_interpreter::interpreter::InterpreterForCell;
use buck2_interpreter::interpreter::ParseResult;
use buck2_interpreter::path::OwnedStarlarkModulePath;
use buck2_interpreter::path::StarlarkModulePath;
use buck2_interpreter::path::StarlarkPath;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::targets_map::TargetsMap;
use buck2_query::query::syntax::simple::functions::testing::QueryFunctionsPanic;
use dupe::Dupe;
use indoc::indoc;
use maplit::hashmap;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::values::Value;

use crate::interpreter::configuror::AdditionalGlobalsFn;
use crate::interpreter::configuror::BuildInterpreterConfiguror;
use crate::interpreter::module_internals::ModuleInternals;

/// Simple container that allows us to instrument things like imports
pub struct Tester {
    cell_alias_resolver: CellAliasResolver,
    cell_resolver: CellResolver,
    configs: LegacyBuckConfigs,
    loaded_modules: LoadedModules,
    additional_globals: Option<AdditionalGlobalsFn>,
    prelude_path: Option<ImportPath>,
}

/// These functions will be available in the starlark environment for all code running through a Tester.
#[starlark_module]
pub fn common_helpers(builder: &mut GlobalsBuilder) {
    /// Returns the string that pprint() will produce
    fn pprint_str<'v>(value: Value<'v>) -> anyhow::Result<String> {
        Ok(format!("{:#}", value))
    }
}

/// Helpers required to help drive the interpreter
pub type CellsData = (CellAliasResolver, CellResolver, LegacyBuckConfigs);

/// The same as `run_starlark_test`, but just make sure the parse succeds;
/// ignore the targets
pub fn run_simple_starlark_test(content: &str) -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    match tester.run_starlark_test(content) {
        Ok(_) => Ok(()),
        Err(e) => Err(anyhow::Error::from(e)),
    }
}

pub fn cells(extra_root_config: Option<&str>) -> anyhow::Result<CellsData> {
    let mut agg = CellsAggregator::new();
    agg.add_cell_entry(
        CellRootPathBuf::new(ProjectRelativePathBuf::try_from("".to_owned())?),
        CellAlias::new("root".to_owned()),
        CellRootPathBuf::new(ProjectRelativePathBuf::try_from("".to_owned())?),
    )?;
    let resolver = agg.make_cell_resolver()?;
    let root_path = if cfg!(windows) {
        AbsNormPath::new("c:/root").unwrap()
    } else {
        AbsNormPath::new("/root").unwrap()
    };

    let configs = hashmap![
        CellName::unchecked_new("root") =>
        LegacyBuckConfig::parse_with_file_ops(
            root_path,
            &TestConfigParserFileOps::new(&[
                (
                    "/root",
                    indoc!(
                        r#"
                        [section]
                            key = value
                            other = 1
                            multiline = hello \
                                        world!
                        [config]
                            key = okay

                        <file:extra_cfg>
                    "#
                    ),
                ),
                ("/extra_cfg", extra_root_config.unwrap_or("")),
            ])?,
            &[],
        )?
    ];

    Ok((
        resolver
            .get(CellName::unchecked_new("root"))?
            .cell_alias_resolver()
            .dupe(),
        resolver,
        LegacyBuckConfigs::new(configs),
    ))
}

pub fn expect_error<T>(result: SharedResult<T>, content: &str, expected: &str) {
    match result {
        Ok(_) => {
            eprintln!(
                "Expected starlark failure, got success.\nCode contents:\n{}",
                content
            );
            panic!();
        }
        Err(e) => {
            let returned = e.to_string();
            if !returned.contains(expected) {
                eprintln!(
                    "Could not find expected error string.\nExpected:\n{}\n\nError:\n{}\n\nCode contents:\n{}",
                    expected, returned, content
                );
                panic!();
            }
        }
    }
}

impl Tester {
    pub fn new() -> anyhow::Result<Self> {
        Self::with_cells(cells(None)?)
    }

    pub fn with_cells(cells_data: CellsData) -> anyhow::Result<Self> {
        let (cell_alias_resolver, cell_resolver, configs) = cells_data;
        Ok(Self {
            cell_alias_resolver,
            cell_resolver,
            configs,
            loaded_modules: LoadedModules::default(),
            additional_globals: None,
            prelude_path: None,
        })
    }

    pub fn set_additional_globals(
        &mut self,
        additional_globals: impl Fn(&mut GlobalsBuilder) + Sync + Send + 'static,
    ) {
        self.additional_globals = Some(AdditionalGlobalsFn(Arc::new(additional_globals)));
    }

    pub fn set_prelude(&mut self, prelude_import: ImportPath) {
        self.prelude_path = Some(prelude_import);
    }

    fn interpreter(&self) -> anyhow::Result<Arc<InterpreterForCell>> {
        let import_paths = ImplicitImportPaths::parse(
            self.configs
                .get(self.cell_alias_resolver.resolve_self())
                .unwrap(),
            BuildFileCell::new(self.cell_alias_resolver.resolve_self()),
            &self.cell_alias_resolver,
        )?;
        let additional_globals = self.additional_globals.dupe();
        Ok(Arc::new(InterpreterForCell::new(
            self.cell_alias_resolver.dupe(),
            Arc::new(GlobalInterpreterState::new(
                &self.configs,
                self.cell_resolver.dupe(),
                BuildInterpreterConfiguror::new(
                    self.prelude_path.clone(),
                    InterpreterHostPlatform::Linux,
                    InterpreterHostArchitecture::X86_64,
                    false,
                    |_| {},
                    |_| {},
                    |_| {},
                    Some(AdditionalGlobalsFn(Arc::new(move |globals_builder| {
                        common_helpers(globals_builder);
                        if let Some(additional_globals) = &additional_globals {
                            (additional_globals.0)(globals_builder)
                        }
                    }))),
                    Arc::new(QueryFunctionsPanic),
                ),
                false,
            )?),
            Arc::new(import_paths),
        )?))
    }

    /// Evaluate an import, and add it to the existing loaded_modules() map to be
    /// used with `eval_build_file`
    pub fn add_import(&mut self, path: &ImportPath, content: &str) -> anyhow::Result<()> {
        let loaded = self.eval_import(path, content, self.loaded_modules.clone())?;
        self.loaded_modules
            .map
            .insert(StarlarkModulePath::LoadFile(path).to_owned(), loaded);
        Ok(())
    }

    /// Evaluate an import without adding it to the accumulated `Tester`
    /// state, and with a specified set of modules loaded into the
    /// environment
    pub fn eval_import(
        &self,
        path: &ImportPath,
        content: &str,
        loaded_modules: LoadedModules,
    ) -> anyhow::Result<LoadedModule> {
        let interpreter = self.interpreter()?;
        let ParseResult(ast, _) =
            interpreter.parse(StarlarkPath::LoadFile(path), content.to_owned())?;
        let buckconfig = self
            .configs
            .get(self.cell_alias_resolver.resolve_self())
            .unwrap();
        let root_buckconfig = self.configs.get(self.cell_resolver.root_cell()).unwrap();
        let env = interpreter.eval_module(
            StarlarkModulePath::LoadFile(path),
            buckconfig,
            root_buckconfig,
            ast,
            loaded_modules.clone(),
            None,
        )?;
        Ok(LoadedModule::new(
            OwnedStarlarkModulePath::LoadFile(path.clone()),
            loaded_modules,
            env,
        ))
    }

    /// Evaluate a build file, adding anything from `add_import` to the
    /// environment
    pub fn eval_build_file(
        &self,
        path: &BuildFilePath,
        content: &str,
        package_listing: PackageListing,
    ) -> anyhow::Result<EvaluationResult> {
        self.eval_build_file_with_loaded_modules(
            path,
            content,
            self.loaded_modules.clone(),
            package_listing,
        )
    }

    /// Evaluate a build file, but only add a specific set of loaded modules to
    /// the environment
    pub fn eval_build_file_with_loaded_modules(
        &self,
        path: &BuildFilePath,
        content: &str,
        loaded_modules: LoadedModules,
        package_listing: PackageListing,
    ) -> anyhow::Result<EvaluationResult> {
        let interpreter = self.interpreter()?;
        let ParseResult(ast, _) =
            interpreter.parse(StarlarkPath::BuildFile(path), content.to_owned())?;
        let buckconfig = self
            .configs
            .get(self.cell_alias_resolver.resolve_self())
            .unwrap();
        let root_buckconfig = self.configs.get(self.cell_resolver.root_cell()).unwrap();
        let eval_result = interpreter.eval_build_file::<ModuleInternals>(
            path,
            buckconfig,
            root_buckconfig,
            package_listing,
            false,
            ast,
            loaded_modules,
            &mut StarlarkProfilerOrInstrumentation::disabled(),
        )?;
        Ok(eval_result)
    }

    pub fn build_file_path() -> BuildFilePath {
        buildfile("root", "some/package")
    }

    /// Run a starlark test with a basic environment. See
    /// `run_starlark_test()` above.
    pub fn run_starlark_test(&mut self, content: &str) -> SharedResult<TargetsMap> {
        let import_path = import("root", "some/package", "defs.bzl");
        self.add_import(
            &import_path,
            &(indoc!(
                r#"
        def assert_eq(a, b):
            if a != b:
                fail("expected: %s got %s" % (a, b))

        "#
            )
            .to_owned()
                + content),
        )?;

        let buildfile_path = Self::build_file_path();
        let res = self.eval_build_file(
            &buildfile_path,
            indoc!(
                r#"
            load(":defs.bzl", "test")
            test()
            "#
            ),
            PackageListing::testing_files(&["file1.java", "file2.java"]),
        )?;
        Ok(res.targets().clone())
    }

    pub fn run_starlark_test_expecting_error(&mut self, content: &str, expected: &str) {
        expect_error(self.run_starlark_test(content), content, expected);
    }

    /// Try to evaluate some content in a .bzl file. Returns `()` if
    /// evaluation was successful. This can be handy if the .bzl
    /// evaluation environment is different from the build file
    /// environment.
    pub fn run_starlark_bzl_test(&mut self, content: &str) -> SharedResult<()> {
        let import_path = import("root", "some/package", "defs.bzl");
        let template = indoc!(
            r#"
            def _assert_eq_ignore_hash_for_strings(a, b):
                (a0, a1, a2) = a.partition("<HASH>")
                # No hash in string
                if a1 == "":
                    assert_eq(a0, b)
                    return

                if not b.startswith(a0):
                    fail("expected %s to match %s", (b, a))
                if not b.endswith(a2):
                    fail("expected %s to match %s", (b, a))
                hash = b[len(a0):-len(a2)]
                if len(hash) != len("0123456789abcdef"):
                    fail("expected %s to match %s", (b, a))

            def assert_eq_ignore_hash(a, b):
                if type(a) == type(""):
                    a = [a]
                    b = [b]

                assert_eq(len(a), len(b))
                for i in range(len(a)):
                    _assert_eq_ignore_hash_for_strings(a[i], b[i])

            def assert_eq(a, b):
                if a != b:
                    fail("expected: %s got %s" % (a, b))

            def assert_ne(a, b):
                if a == b:
                    fail("expected: %s to not equal %s" % (a, b))"#
        );

        self.add_import(&import_path, &format!("{}\n\n{}", template, content))?;

        let test_path = import("root", "some/package", "test.bzl");
        let test_content = indoc!(
            r#"
            load("//some/package:defs.bzl", "test")
            test()
            "#
        );
        self.add_import(&test_path, test_content)
            .map_err(|e| e.into())
    }

    pub fn run_starlark_bzl_test_expecting_error(&mut self, content: &str, expected: &str) {
        expect_error(self.run_starlark_bzl_test(content), content, expected);
    }
}

pub fn import(cell: &str, package: &str, filename: &str) -> ImportPath {
    ImportPath::unchecked_new(cell, package, filename)
}

pub fn buildfile(cell: &str, package: &str) -> BuildFilePath {
    BuildFilePath::unchecked_new(cell, package, "BUCK")
}
