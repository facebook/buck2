/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use sha2::{Digest, Sha256};
use starlark::{environment::GlobalsBuilder, eval::Evaluator, values::Value};

use crate::{extra::BuildContext, globspec::GlobSpec, selector::Selector};

#[starlark_module]
pub fn native_module(builder: &mut GlobalsBuilder) {
    fn select<'v>(d: Value<'v>) -> anyhow::Result<Selector<'v>> {
        Ok(Selector::new(d))
    }

    /// Applies a mapping function to a selector. See [Selector::select_map].
    fn select_map<'v>(
        d: Value<'v>,
        func: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        Selector::select_map(d, eval, func)
    }

    /// Applies a test function to a selector. See [Selector::select_test].
    fn select_test<'v>(
        d: Value<'v>,
        func: Value<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<bool> {
        Selector::select_test(d, eval, func)
    }

    /// Tests that two selects are equal to each other. For testing use only.
    fn select_equal_internal<'v>(left: Value<'v>, right: Value<'v>) -> anyhow::Result<bool> {
        Selector::select_equal_internal(left, right)
    }

    fn glob<'v>(
        include: Vec<String>,
        exclude: Option<Vec<String>>,
        #[starlark(default = false)] include_dotfiles: bool,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let extra = BuildContext::from_context(eval)?;
        let excludes = exclude.unwrap_or_default();
        let spec = GlobSpec::new(&include, &excludes, include_dotfiles)?;
        let res = extra
            .resolve_glob(&spec)?
            .map(|path| eval.heap().alloc(path.as_str()));
        Ok(eval.heap().alloc_list_iter(res))
    }

    fn package(eval: &mut Evaluator) -> anyhow::Result<String> {
        // TODO(cjhopman): Is this used much? Can we change it to return a thin wrapper
        // over the Package itself that exposes things like the cell name or fully specified name?
        Ok(BuildContext::from_context(eval)?
            .require_package()?
            .to_string())
    }

    fn package_name(eval: &mut Evaluator) -> anyhow::Result<String> {
        // An (IMO) unfortunate choice in the skylark api is that this just gives the cell-relative
        //  path of the package (which isn't a unique "name" for the package)
        Ok(BuildContext::from_context(eval)?
            .require_package()?
            .cell_relative_path()
            .to_string())
    }

    fn get_base_path(eval: &mut Evaluator) -> anyhow::Result<String> {
        Ok(BuildContext::from_context(eval)?
            .require_package()?
            .cell_relative_path()
            .to_string())
    }

    fn repository_name(eval: &mut Evaluator) -> anyhow::Result<String> {
        // In Buck v1 the repository name has a leading `@` on it, so match that with v2.
        // In practice, most users do `repository_name()[1:]` to drop it.
        Ok(format!(
            "@{}",
            BuildContext::from_context(eval)?.cell_info().name()
        ))
    }

    fn get_cell_name(eval: &mut Evaluator) -> anyhow::Result<String> {
        Ok(BuildContext::from_context(eval)?
            .cell_info()
            .name()
            .to_string())
    }
}

/// Contains functions that we include in all contexts.
#[starlark_module]
pub fn stdlib(builder: &mut GlobalsBuilder) {
    /// Computes a sha256 digest for a string. Returns the hex representation of the digest.
    fn sha256(val: &str) -> anyhow::Result<String> {
        let hash = Sha256::digest(val.as_bytes());
        Ok(hex::encode(&hash))
    }
}

pub fn register_natives(registry: &mut GlobalsBuilder) {
    native_module(registry);
    stdlib(registry);
}

pub fn register_globals(_env: &mut GlobalsBuilder) {
    // There are currently no globals. Everything is available on the native module.
    // TODO: once we support configuration of whether to include the native module
    // in the global scope, we will likely need to identify some functions to be
    // moved to the global module.
}

#[cfg(test)]
mod tests {
    use std::{convert::TryFrom, sync::Arc};

    use buck2_common::legacy_configs::{
        testing::TestConfigParserFileOps, LegacyBuckConfig, LegacyBuckConfigs,
    };
    use buck2_core::{
        cells::{CellAlias, CellAliasResolver, CellName, CellResolver, CellsAggregator},
        fs::{
            paths::{AbsPath, FileNameBuf},
            project::ProjectRelativePathBuf,
        },
    };
    use gazebo::prelude::*;
    use indoc::indoc;

    use crate::{
        common::{
            BuildFileCell, BuildFilePath, ImportPath, OwnedStarlarkModulePath, StarlarkModulePath,
            StarlarkPath,
        },
        extra::testing::{TesterConfiguror, TesterExtraContext},
        file_loader::{LoadedModule, LoadedModules},
        import_paths::ImportPaths,
        interpreter::{
            GlobalInterpreterState, InterpreterConfigForCell, InterpreterForCell, ParseResult,
        },
        package_listing::listing::PackageListing,
        starlark_profiler::{StarlarkProfilerInstrumentation, StarlarkProfilerOrInstrumentation},
    };

    fn cells() -> anyhow::Result<(CellAliasResolver, CellResolver, LegacyBuckConfigs)> {
        let mut agg = CellsAggregator::new();
        agg.add_cell_alias_entry(
            ProjectRelativePathBuf::try_from("".to_owned())?,
            CellAlias::new("root".to_owned()),
            ProjectRelativePathBuf::try_from("".to_owned())?,
        )?;
        let resolver = agg.make_cell_resolver()?;
        let repo_root = if cfg!(windows) { "C:/root" } else { "/root" };

        let configs = maplit::hashmap![
            CellName::unchecked_new("root".to_owned()) =>
            LegacyBuckConfig::parse_with_file_ops(
                AbsPath::unchecked_new(repo_root),
                &TestConfigParserFileOps::new(&[(
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
                        "#
                    ),
                )])?,
                &[],
            )?,
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

    fn interpreter() -> anyhow::Result<InterpreterForCell> {
        let root_cell = BuildFileCell::new(CellName::unchecked_new("root".to_owned()));
        let (cell_alias_resolver, resolver, configs) = cells()?;
        let import_paths = ImportPaths::parse(
            configs.get(root_cell.name()).unwrap(),
            &root_cell,
            &cell_alias_resolver,
        )?;
        Ok(InterpreterForCell::new(
            Arc::new(InterpreterConfigForCell::new(
                cell_alias_resolver,
                Arc::new(GlobalInterpreterState::new(
                    &configs,
                    resolver,
                    TesterConfiguror::new(vec!["export_file".to_owned()]),
                    false,
                )?),
            )?),
            Arc::new(import_paths),
        ))
    }

    fn import() -> ImportPath {
        ImportPath::unchecked_new("root", "some/package", "defs.bzl")
    }

    fn buildfile() -> BuildFilePath {
        BuildFilePath::unchecked_new("root", "some/package", "BUCK")
    }

    fn run_starlark_test(content: &str) -> anyhow::Result<()> {
        let import_path = import();
        let interpreter = interpreter()?;
        let ParseResult(ast, _) = interpreter.parse(
            StarlarkPath::LoadFile(&import_path),
            indoc!(
                r#"
            def assert_eq(a, b):
                if a != b:
                    fail("expected: %s got %s" % (a, b))

            "#
            )
            .to_owned()
                + content,
        )?;
        let buckconfig = LegacyBuckConfig::empty();
        let env = interpreter.eval_module(
            StarlarkModulePath::LoadFile(&import_path),
            &buckconfig,
            ast,
            LoadedModules::default(),
            StarlarkProfilerInstrumentation::default(),
        )?;
        let import_result = LoadedModule::new(
            OwnedStarlarkModulePath::LoadFile(import_path.clone()),
            LoadedModules::default(),
            env,
        );

        let buildfile_path = buildfile();
        let mut loaded_modules = LoadedModules::default();
        loaded_modules
            .map
            .insert(import_path.id().to_owned(), import_result);
        let ParseResult(ast, _) = interpreter.parse(
            StarlarkPath::BuildFile(&buildfile_path),
            indoc!(
                r#"
                load(":defs.bzl", "test")
                export_file(
                    name = "rule_name",
                )
                test()
                "#
            )
            .to_owned(),
        )?;
        let buckconfig = LegacyBuckConfig::empty();
        interpreter.eval_build_file::<TesterExtraContext>(
            &buildfile_path,
            &buckconfig,
            PackageListing::empty(FileNameBuf::unchecked_new("BUCK".to_owned())),
            false,
            ast,
            loaded_modules,
            &mut StarlarkProfilerOrInstrumentation::disabled(),
        )?;
        Ok(())
    }

    #[test]
    fn eval() -> anyhow::Result<()> {
        run_starlark_test(indoc!(
            r#"
            def test():
                assert_eq("some/package", __internal__.package_name())
                assert_eq("@root", __internal__.repository_name())

                assert_eq(package_name(), __internal__.package_name())
                assert_eq(repository_name(), __internal__.repository_name())

                assert_eq(package_name(), get_base_path())

                print("some message")
                print("multiple", "strings")

                assert_eq(sha256("123"), "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3")
            "#
        ))?;
        Ok(())
    }

    #[test]
    fn select_funcs() -> anyhow::Result<()> {
        run_starlark_test(indoc!(
            // This is from //xplat/build_infra/buck_client/test/com/facebook/buck/parser/testdata/select_introspection/defs.bzl
            r#"
def _map_func(value):
    if type(value) == type(""):
        if "TEST" in value:
            return value.replace("TEST", "replaced")
    elif type(value) == type([]):
        return [v for v in value if v != "INVALID"]
    elif type(value) == type({}):
        return {k: v for k, v in value.items() if "win" in v}
    return value

def _test_func(value):
    return "TEST" in value

def _assert_eq(expected, actual):
    if type(expected) == type(select({"DEFAULT": []})):
        result = select_equal_internal(expected, actual)
    else:
        result = expected == actual

    if not result:
        fail("expected %s but got %s" % (expected, actual))

def _test_single_config_str():
    str_select = select({"config/windows:x86_64": "flag_TEST"})

    _assert_eq(
        select({"config/windows:x86_64": "flag_replaced"}),
        select_map(str_select, _map_func),
    )
    _assert_eq(True, select_test(str_select, _test_func))

def _test_single_config_list():
    list_select = select({"config/windows:x86_64": ["flag", "INVALID"]})

    _assert_eq(
        select({"config/windows:x86_64": ["flag"]}),
        select_map(list_select, _map_func),
    )
    _assert_eq(False, select_test(list_select, _test_func))

def _test_single_config_dict():
    dict_select = select({
        "config/windows:x86_64": {"test.h": "windows/test.h", "test_apple.h": "apple/test.h"},
    })

    _assert_eq(
        select({"config/windows:x86_64": {"test.h": "windows/test.h"}}),
        select_map(dict_select, _map_func),
    )
    _assert_eq(False, select_test(dict_select, _test_func))

def _test_multi_config():
    multi_select = select({
        "DEFAULT": ["-DBASE", "TEST"],
        "config//android:base": ["-DANDROID"],
        "config//iphoneos:base": ["INVALID", "-DIPHONE"],
        "config//windows:base": ["TEST"],
    })

    _assert_eq(
        select({
            "DEFAULT": ["-DBASE", "TEST"],
            "config//android:base": ["-DANDROID"],
            "config//iphoneos:base": ["-DIPHONE"],
            "config//windows:base": ["TEST"],
        }),
        select_map(multi_select, _map_func),
    )
    _assert_eq(True, select_test(multi_select, _test_func))

def _test_concatenated_native():
    expr_select = ["INVALID"] + ["TEST"] + select({"config/windows:x86_64": ["-DWINDOWS"]})

    _assert_eq(
        [] + ["TEST"] + select({"config/windows:x86_64": ["-DWINDOWS"]}),
        select_map(expr_select, _map_func),
    )
    _assert_eq(True, select_test(expr_select, _test_func))

def _test_concatenated_nested():
    expr_select = ["TEST"] + select({"config/windows:x86_64": ["-DWINDOWS", "INVALID"]})

    _assert_eq(
        ["TEST"] + select({"config/windows:x86_64": ["-DWINDOWS"]}),
        select_map(expr_select, _map_func),
    )
    _assert_eq(True, select_test(expr_select, _test_func))

def test():
    _test_single_config_str()
    _test_single_config_list()
    _test_single_config_dict()
    _test_multi_config()
    _test_concatenated_native()
    _test_concatenated_nested()
    "#
        ))?;
        Ok(())
    }
}
