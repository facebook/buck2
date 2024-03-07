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
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::alias::NonEmptyCellAlias;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::name::CellName;
use buck2_core::cells::*;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_interpreter::factory::StarlarkPassthroughProvider;
use buck2_interpreter::file_loader::LoadedModule;
use buck2_interpreter::file_loader::LoadedModules;
use buck2_interpreter::import_paths::ImplicitImportPaths;
use buck2_interpreter::paths::module::OwnedStarlarkModulePath;
use buck2_interpreter::paths::module::StarlarkModulePath;
use buck2_interpreter::paths::path::StarlarkPath;
use buck2_interpreter::prelude_path::PreludePath;
use buck2_node::nodes::eval_result::EvaluationResult;
use buck2_node::nodes::targets_map::TargetsMap;
use buck2_node::super_package::SuperPackage;
use dupe::Dupe;
use indoc::indoc;
use maplit::hashmap;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::values::Value;

use crate::interpreter::buckconfig::LegacyConfigsViewForStarlark;
use crate::interpreter::cell_info::InterpreterCellInfo;
use crate::interpreter::configuror::AdditionalGlobalsFn;
use crate::interpreter::configuror::BuildInterpreterConfiguror;
use crate::interpreter::global_interpreter_state::GlobalInterpreterState;
use crate::interpreter::interpreter_for_cell::InterpreterForCell;
use crate::interpreter::interpreter_for_cell::ParseData;
use crate::super_package::package_value::SuperPackageValuesImpl;

/// Simple container that allows us to instrument things like imports
#[derive(Debug)]
pub struct Tester {
    cell_alias_resolver: CellAliasResolver,
    cell_resolver: CellResolver,
    configs: LegacyBuckConfigs,
    loaded_modules: LoadedModules,
    additional_globals: Vec<AdditionalGlobalsFn>,
    prelude_path: Option<PreludePath>,
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

/// The same as `run_starlark_test`, but just make sure the parse succeeds;
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
        NonEmptyCellAlias::new("root".to_owned()).unwrap(),
        CellRootPathBuf::new(ProjectRelativePathBuf::try_from("".to_owned())?),
    )?;
    let resolver = agg.make_cell_resolver()?;
    let root_path = if cfg!(windows) {
        AbsNormPath::new("c:/root").unwrap()
    } else {
        AbsNormPath::new("/root").unwrap()
    };

    let configs = hashmap![
        CellName::testing_new("root") =>
        LegacyBuckConfig::parse_with_file_ops(
            root_path,
            &mut TestConfigParserFileOps::new(&[
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
            .get(CellName::testing_new("root"))?
            .testing_cell_alias_resolver()
            .dupe(),
        resolver,
        LegacyBuckConfigs::new(configs),
    ))
}

pub fn expect_error<T>(result: buck2_error::Result<T>, content: &str, expected: &str) {
    match result {
        Ok(_) => {
            eprintln!(
                "Expected starlark failure, got success.\nCode contents:\n{}",
                content
            );
            panic!();
        }
        Err(e) => {
            let returned = format!("{:?}", e);
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
            additional_globals: Vec::new(),
            prelude_path: None,
        })
    }

    pub fn additional_globals(
        &mut self,
        additional_globals: impl Fn(&mut GlobalsBuilder) + Sync + Send + 'static,
    ) {
        self.additional_globals
            .push(AdditionalGlobalsFn(Arc::new(additional_globals)));
    }

    pub fn set_prelude(&mut self, prelude_import: ImportPath) {
        self.prelude_path = Some(PreludePath::testing_new(prelude_import));
    }

    fn interpreter(&self) -> anyhow::Result<Arc<InterpreterForCell>> {
        let build_file_cell = BuildFileCell::new(self.cell_alias_resolver.resolve_self());
        let import_paths = ImplicitImportPaths::parse(
            self.configs
                .get(self.cell_alias_resolver.resolve_self())
                .unwrap(),
            build_file_cell,
            &self.cell_alias_resolver,
        )?;
        let additional_globals = self.additional_globals.clone();
        let cell_info = InterpreterCellInfo::new(
            build_file_cell,
            self.cell_resolver.dupe(),
            self.cell_alias_resolver.dupe(),
        )?;
        Ok(Arc::new(InterpreterForCell::new(
            cell_info,
            Arc::new(GlobalInterpreterState::new(
                self.cell_resolver.dupe(),
                BuildInterpreterConfiguror::new(
                    self.prelude_path.clone(),
                    InterpreterHostPlatform::Linux,
                    InterpreterHostArchitecture::X86_64,
                    None,
                    false,
                    false,
                    |_| {},
                    |_| {},
                    |_| {},
                    |_| {},
                    Some(AdditionalGlobalsFn(Arc::new(move |globals_builder| {
                        common_helpers(globals_builder);
                        for additional_globals in &additional_globals {
                            (additional_globals.0)(globals_builder)
                        }
                    }))),
                )?,
                false,
                true,
            )?),
            Arc::new(import_paths),
        )?))
    }

    pub fn parse(&self, import: StarlarkPath, content: &str) -> ParseData {
        self.interpreter()
            .unwrap()
            .parse(import, content.to_owned())
            .unwrap()
            .unwrap()
    }

    /// Evaluate an import, and add it to the existing loaded_modules() map to be
    /// used with `eval_build_file`
    pub fn add_import(&mut self, path: &ImportPath, content: &str) -> anyhow::Result<LoadedModule> {
        let loaded = self.eval_import(path, content, self.loaded_modules.clone())?;
        self.loaded_modules
            .map
            .insert(StarlarkModulePath::LoadFile(path).to_owned(), loaded.dupe());
        Ok(loaded)
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
        let ParseData(ast, _) =
            interpreter.parse(StarlarkPath::LoadFile(path), content.to_owned())??;
        let buckconfig = self
            .configs
            .get(self.cell_alias_resolver.resolve_self())
            .unwrap();
        let root_buckconfig = self.configs.get(self.cell_resolver.root_cell()).unwrap();
        let mut provider = StarlarkPassthroughProvider;
        let mut buckconfigs =
            LegacyConfigsViewForStarlark::new(buckconfig.clone(), root_buckconfig.clone());

        let env = interpreter.eval_module(
            StarlarkModulePath::LoadFile(path),
            &mut buckconfigs,
            ast,
            loaded_modules.clone(),
            &mut provider,
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
        let ParseData(ast, _) =
            interpreter.parse(StarlarkPath::BuildFile(path), content.to_owned())??;
        let buckconfig = self
            .configs
            .get(self.cell_alias_resolver.resolve_self())
            .unwrap();
        let root_buckconfig = self.configs.get(self.cell_resolver.root_cell()).unwrap();
        let mut provider = StarlarkPassthroughProvider;
        let mut buckconfigs =
            LegacyConfigsViewForStarlark::new(buckconfig.clone(), root_buckconfig.clone());
        let eval_result_with_stats = interpreter.eval_build_file(
            path,
            &mut buckconfigs,
            package_listing,
            SuperPackage::empty::<SuperPackageValuesImpl>(),
            false,
            ast,
            loaded_modules,
            &mut provider,
            true,
        )?;
        Ok(eval_result_with_stats.result)
    }

    pub fn build_file_path() -> BuildFilePath {
        BuildFilePath::testing_new("root//some/package:BUCK")
    }

    /// Run a starlark test with a basic environment. See
    /// `run_starlark_test()` above.
    pub fn run_starlark_test(&mut self, content: &str) -> buck2_error::Result<TargetsMap> {
        let import_path = ImportPath::testing_new("root//some/package:defs.bzl");
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
    pub fn run_starlark_bzl_test(&mut self, content: &str) -> buck2_error::Result<()> {
        let import_path = ImportPath::testing_new("root//some/package:defs.bzl");
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
                    fail("expected: %s to not equal %s" % (a, b))

            def assert_true(c):
                if not c:
                    fail("assertion failed")

            def assert_false(c):
                if c:
                    fail("assertion failed")
            "#
        );

        self.add_import(&import_path, &format!("{}\n\n{}", template, content))?;

        let test_path = ImportPath::testing_new("root//some/package:test.bzl");
        let test_content = indoc!(
            r#"
            load("//some/package:defs.bzl", "test")
            test()
            "#
        );
        self.add_import(&test_path, test_content)
            .map(|_| ())
            .map_err(|e| e.into())
    }

    pub fn run_starlark_bzl_test_expecting_error(&mut self, content: &str, expected: &str) {
        expect_error(self.run_starlark_bzl_test(content), content, expected);
    }
}
