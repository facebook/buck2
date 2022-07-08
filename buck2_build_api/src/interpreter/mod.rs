/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

pub mod build_defs;
pub mod calculation;
pub mod context;
pub mod module_internals;
pub mod rule_defs;

#[cfg(test)]
pub mod testing {
    use std::convert::TryFrom;
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
    use buck2_core::cells::cell_path::CellPath;
    use buck2_core::cells::cell_root_path::CellRootPathBuf;
    use buck2_core::cells::*;
    use buck2_core::fs::paths::*;
    use buck2_core::fs::project::ProjectRelativePathBuf;
    use buck2_interpreter::common::OwnedStarlarkModulePath;
    use buck2_interpreter::common::StarlarkModulePath;
    use buck2_interpreter::common::StarlarkPath;
    use buck2_interpreter::extra::cell_info::InterpreterCellInfo;
    use buck2_interpreter::extra::ExtraContextDyn;
    use buck2_interpreter::extra::InterpreterConfiguror;
    use buck2_interpreter::extra::InterpreterHostArchitecture;
    use buck2_interpreter::extra::InterpreterHostPlatform;
    use buck2_interpreter::file_loader::LoadedModule;
    use buck2_interpreter::file_loader::LoadedModules;
    use buck2_interpreter::import_paths::ImportPaths;
    use buck2_interpreter::interpreter::GlobalInterpreterState;
    use buck2_interpreter::interpreter::InterpreterConfigForCell;
    use buck2_interpreter::interpreter::InterpreterForCell;
    use buck2_interpreter::interpreter::ParseResult;
    use buck2_interpreter::package_imports::ImplicitImport;
    use buck2_interpreter::starlark_profiler::StarlarkProfilerInstrumentation;
    use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
    use buck2_node::nodes::unconfigured::TargetsMap;
    use gazebo::cmp::PartialEqAny;
    use gazebo::prelude::*;
    use indoc::indoc;
    use starlark::environment::GlobalsBuilder;
    use starlark::values::Value;

    use crate::interpreter::context::configure_build_file_globals;
    use crate::interpreter::context::configure_extension_file_globals;
    use crate::interpreter::context::BuildInterpreterConfiguror;
    use crate::interpreter::module_internals::EvaluationResult;
    use crate::interpreter::module_internals::ModuleInternals;

    pub type GlobalsConfigurationFn = Arc<dyn Fn(&mut GlobalsBuilder) + Sync + Send>;

    /// Simple container that allows us to instrument things like imports
    pub struct Tester {
        cell_alias_resolver: CellAliasResolver,
        cell_resolver: CellResolver,
        configs: LegacyBuckConfigs,
        loaded_modules: LoadedModules,
        additional_globals: Option<GlobalsConfigurationFn>,
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
        match run_starlark_test(content) {
            Ok(_) => Ok(()),
            Err(e) => Err(anyhow::Error::from(e)),
        }
    }

    /// Run a test within a starlark build file
    ///
    /// Is written into a .bzl file, loaded, and its test() function is called.
    /// This test function has an 'assert_eq(a: Any, b: Any)' function available
    /// to do eq checks
    ///
    /// Returns the targets that were registered
    ///
    /// ```ignore
    /// run_starlark_test(indoc!(r#"
    ///     def add(a, b):
    ///         return a + b
    ///
    ///     def test():
    ///         assert_eq(add(2, 2), 4) # Success
    ///         assert_eq(add(2, 2), 5) # Fails
    ///     "#)
    /// ```
    pub fn run_starlark_test(content: &str) -> SharedResult<TargetsMap> {
        let mut tester = Tester::new()?;
        tester.run_starlark_test(content)
    }

    /// Run a test within a starlark extension file. Useful for functions that
    /// only exist in .bzl files
    ///
    /// Is written into a .bzl file, and its test() function is called.
    /// This test function has an 'assert_eq(a: Any, b: Any)' function available
    /// to do eq checks
    ///
    /// Returns `()` if the file could be evaluated.
    ///
    /// ```ignore
    /// run_starlark_test(indoc!(r#"
    ///     def add(a, b):
    ///         return a + b
    ///
    ///     def test():
    ///         assert_eq(add(2, 2), 4) # Success
    ///         assert_eq(add(2, 2), 5) # Fails
    ///     "#)
    /// ```
    pub fn run_starlark_bzl_test(content: &str) -> SharedResult<()> {
        let mut tester = Tester::with_cells(cells(None)?)?;
        tester.run_starlark_bzl_test(content)
    }

    pub fn cells(extra_root_config: Option<&str>) -> anyhow::Result<CellsData> {
        let mut agg = CellsAggregator::new();
        agg.add_cell_alias_entry(
            CellRootPathBuf::new(ProjectRelativePathBuf::try_from("".to_owned())?),
            CellAlias::new("root".to_owned()),
            CellRootPathBuf::new(ProjectRelativePathBuf::try_from("".to_owned())?),
        )?;
        let resolver = agg.make_cell_resolver()?;
        let root_path = if cfg!(windows) {
            AbsPath::unchecked_new("c:/root")
        } else {
            AbsPath::unchecked_new("/root")
        };

        let configs = hashmap![
            CellName::unchecked_new("root".to_owned()) =>
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
                .get(&CellName::unchecked_new("root".to_owned()))?
                .cell_alias_resolver()
                .dupe(),
            resolver,
            LegacyBuckConfigs::new(configs),
        ))
    }

    pub fn run_starlark_test_expecting_error(content: &str, expected: &str) {
        expect_error(run_starlark_test(content), content, expected);
    }

    pub fn run_starlark_bzl_test_expecting_error(content: &str, expected: &str) {
        expect_error(run_starlark_bzl_test(content), content, expected);
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

    /// Configuror for testing that can add extra symbols into globals. This can be handy
    /// for testing structures that aren't directly exposed, but need to operate in Starlark
    /// (e.g. ProviderCollection, or Artifact)
    struct InjectableInterpreterConfiguror {
        additional_globals: Option<GlobalsConfigurationFn>,
        inner: Arc<BuildInterpreterConfiguror>,
    }

    impl InjectableInterpreterConfiguror {
        fn new(
            additional_globals: Option<GlobalsConfigurationFn>,
            inner: Arc<BuildInterpreterConfiguror>,
        ) -> Arc<Self> {
            Arc::new(Self {
                additional_globals,
                inner,
            })
        }
    }

    impl InterpreterConfiguror for InjectableInterpreterConfiguror {
        fn configure_build_file_globals(&self, globals_builder: &mut GlobalsBuilder) {
            self.inner.configure_build_file_globals(globals_builder);
            common_helpers(globals_builder);
            match &self.additional_globals {
                None => {}
                Some(module) => module(globals_builder),
            }
        }

        fn configure_extension_file_globals(&self, globals_builder: &mut GlobalsBuilder) {
            self.inner.configure_extension_file_globals(globals_builder);
            common_helpers(globals_builder);
            match &self.additional_globals {
                None => {}
                Some(module) => module(globals_builder),
            }
        }

        fn configure_bxl_file_globals(&self, globals_builder: &mut GlobalsBuilder) {
            self.inner.configure_bxl_file_globals(globals_builder);
            common_helpers(globals_builder);
            match &self.additional_globals {
                None => {}
                Some(module) => module(globals_builder),
            }
        }

        fn configure_native_struct(&self, native_module: &mut GlobalsBuilder) {
            self.inner.configure_native_struct(native_module)
        }

        fn host_platform(&self) -> InterpreterHostPlatform {
            self.inner.host_platform()
        }

        fn host_architecture(&self) -> InterpreterHostArchitecture {
            self.inner.host_architecture()
        }

        fn new_extra_context(
            &self,
            cell_info: &InterpreterCellInfo,
            buildfile_path: BuildFilePath,
            package_listing: PackageListing,
            package_boundary_exception: bool,
            loaded_modules: &LoadedModules,
            implicit_import: Option<&Arc<ImplicitImport>>,
        ) -> SharedResult<Box<dyn ExtraContextDyn>> {
            self.inner.new_extra_context(
                cell_info,
                buildfile_path,
                package_listing,
                package_boundary_exception,
                loaded_modules,
                implicit_import,
            )
        }

        fn get_prelude_import(&self, import: StarlarkPath) -> Option<&ImportPath> {
            self.inner.get_prelude_import(import)
        }

        fn is_prelude_path(&self, path: &CellPath) -> bool {
            self.inner.is_prelude_path(path)
        }

        fn eq_token(&self) -> PartialEqAny {
            PartialEqAny::always_false()
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

        pub fn set_additional_globals(&mut self, additional_globals: GlobalsConfigurationFn) {
            self.additional_globals = Some(additional_globals);
        }

        pub fn set_prelude(&mut self, prelude_import: ImportPath) {
            self.prelude_path = Some(prelude_import);
        }

        fn interpreter(&self) -> anyhow::Result<InterpreterForCell> {
            let import_paths = ImportPaths::parse(
                self.configs
                    .get(self.cell_alias_resolver.resolve_self())
                    .unwrap(),
                &BuildFileCell::new(self.cell_alias_resolver.resolve_self().clone()),
                &self.cell_alias_resolver,
            )?;
            Ok(InterpreterForCell::new(
                Arc::new(InterpreterConfigForCell::new(
                    self.cell_alias_resolver.dupe(),
                    Arc::new(GlobalInterpreterState::new(
                        &self.configs,
                        self.cell_resolver.dupe(),
                        InjectableInterpreterConfiguror::new(
                            self.additional_globals.dupe(),
                            BuildInterpreterConfiguror::new(
                                self.prelude_path.clone(),
                                InterpreterHostPlatform::Linux,
                                InterpreterHostArchitecture::X86_64,
                                false,
                                configure_build_file_globals,
                                configure_extension_file_globals,
                                |_| {},
                            ),
                        ),
                        false,
                    )?),
                )?),
                Arc::new(import_paths),
            ))
        }

        /// Evaluate an import, and add it to the existing loaded_modules() map to be
        /// used with `eval_build_file`
        pub fn add_import(&mut self, path: &ImportPath, content: &str) -> anyhow::Result<()> {
            let loaded = self.eval_import(path, content, self.loaded_modules.clone())?;
            self.loaded_modules.map.insert(path.id().to_owned(), loaded);
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
            let env = interpreter.eval_module(
                StarlarkModulePath::LoadFile(path),
                buckconfig,
                ast,
                loaded_modules.clone(),
                StarlarkProfilerInstrumentation::default(),
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
            let eval_result = interpreter.eval_build_file::<ModuleInternals>(
                path,
                buckconfig,
                package_listing,
                false,
                ast,
                loaded_modules,
                &mut StarlarkProfilerOrInstrumentation::disabled(),
            )?;
            Ok(eval_result)
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

            let buildfile_path = buildfile("root", "some/package");
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
    }

    pub fn import(cell: &str, package: &str, filename: &str) -> ImportPath {
        ImportPath::unchecked_new(cell, package, filename)
    }

    pub fn buildfile(cell: &str, package: &str) -> BuildFilePath {
        BuildFilePath::unchecked_new(cell, package, "BUCK")
    }
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use buck2_common::result::SharedResult;
    use indoc::indoc;
    use starlark::environment::GlobalsBuilder;

    use crate::interpreter::testing::import;
    use crate::interpreter::testing::run_starlark_test;
    use crate::interpreter::testing::Tester;

    #[test]
    fn cannot_register_target_twice() {
        let err = run_starlark_test(indoc!(
            r#"
            def _impl(ctx):
                pass
            export_file = rule(impl=_impl, attrs = {})
            def test():
                export_file(name="foo")
                export_file(name="foo")
        "#
        ))
        .expect_err("should fail");
        assert!(
            err.to_string()
                .contains("Attempted to register target root//some/package:foo twice"),
            "got `{}`",
            err
        );
    }

    // Dummy module just to make sure that our integration test framework is working...
    #[starlark_module]
    fn extra_provider_module(builder: &mut GlobalsBuilder) {
        fn add_one(i: i32) -> anyhow::Result<i32> {
            Ok(i + 1)
        }
    }

    #[test]
    fn tester_can_load_extra_modules() -> SharedResult<()> {
        let mut tester = Tester::new()?;
        tester.set_additional_globals(Arc::new(extra_provider_module));

        tester.run_starlark_test(indoc!(
            r#"
            x = add_one(1)
            def test():
                y = 2
                assert_eq(2, x)
                assert_eq(3, add_one(y))
            "#
        ))?;

        tester.run_starlark_bzl_test(indoc!(
            r#"
            x = add_one(1)
            def test():
                y = 2
                assert_eq(2, x)
                assert_eq(3, add_one(y))
            "#
        ))
    }

    #[test]
    fn tester_can_load_symbols_transitively() -> SharedResult<()> {
        fn new_tester() -> SharedResult<Tester> {
            let mut tester = Tester::new()?;
            tester.add_import(
                &import("root", "test", "def1.bzl"),
                indoc!(
                    r#"
                l = [1,2,3]
                "#
                ),
            )?;
            tester.add_import(
                &import("root", "test", "def2.bzl"),
                indoc!(
                    r#"
                load("//test:def1.bzl", "l")
                l2 = l + [4,5,6]
                "#
                ),
            )?;
            Ok(tester)
        }

        let mut tester = new_tester()?;
        tester.run_starlark_test(indoc!(
            r#"
            load("//test:def2.bzl", "l2")
            def test():
                assert_eq([1,2,3,4,5,6], l2)
            "#
        ))?;

        let mut tester = new_tester()?;
        tester.run_starlark_bzl_test(indoc!(
            r#"
            load("//test:def2.bzl", "l2")
            def test():
                assert_eq([1,2,3,4,5,6], l2)
            "#
        ))
    }
}
