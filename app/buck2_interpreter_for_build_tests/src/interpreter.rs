/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::register_rule_defs;
use buck2_common::legacy_configs::cells::BuckConfigBasedCells;
use buck2_common::legacy_configs::testing::TestConfigParserFileOps;
use buck2_common::package_listing::listing::testing::PackageListingExt;
use buck2_common::package_listing::listing::PackageListing;
use buck2_core::build_file_path::BuildFilePath;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::name::CellName;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_interpreter::file_loader::LoadedModules;
use buck2_interpreter::path::OwnedStarlarkModulePath;
use buck2_interpreter::path::StarlarkPath;
use buck2_interpreter_for_build::interpreter::natives::register_module_natives;
use buck2_interpreter_for_build::interpreter::testing::run_simple_starlark_test;
use buck2_interpreter_for_build::interpreter::testing::CellsData;
use buck2_interpreter_for_build::interpreter::testing::Tester;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::nodes::unconfigured::testing::targets_to_json;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use indoc::indoc;
use serde_json::json;

#[test]
fn test_eval_import() {
    let loaded = Tester::new()
        .unwrap()
        .add_import(
            &ImportPath::testing_new("root//some/package:defs.bzl"),
            indoc!(
                r#"
            one = 1
            hello = "world"
            "#
            ),
        )
        .unwrap();

    assert_eq!(1, loaded.env().get("one").unwrap().unpack_int().unwrap());

    assert_eq!(
        "world",
        loaded.env().get("hello").unwrap().unpack_str().unwrap()
    );
}

#[test]
fn test_load() {
    let import_path = ImportPath::testing_new("root//imports:one.bzl");
    let mut tester = Tester::new().unwrap();
    tester
        .add_import(
            &import_path,
            indoc!(
                r#"
                    def concat(*args):
                      s = ""
                      for a in args:
                        s += a
                      return s
                    "#
            ),
        )
        .unwrap();

    let parse_result = tester
        .add_import(
            &ImportPath::testing_new("root//some/package:defs.bzl"),
            indoc!(
                r#"
                load("@root//imports:one.bzl", "concat")
                message = concat("hello", " ", "world!")
                "#
            ),
        )
        .unwrap();

    assert_eq!(
        "hello world!",
        parse_result
            .env()
            .get("message")
            .unwrap()
            .unpack_str()
            .unwrap()
    );
}

#[test]
fn test_eval_build_file() {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_rule_defs);

    tester
        .add_import(
            &ImportPath::testing_new("root//:rules.bzl"),
            indoc!(
                r#"
                def _impl(ctx):
                    return DefaultInfo()

                export_file = rule(
                    impl = _impl,
                    attrs = {
                        "src": attrs.any(),
                    },
                )

                java_library = rule(
                    impl = _impl,
                    attrs = {
                        "srcs": attrs.list(attrs.any()),
                    },
                )
            "#
            ),
        )
        .unwrap();

    tester
        .add_import(
            &ImportPath::testing_new_cross_cell("root", "imports", "one.bzl", "root"),
            indoc!(
                r#"
                    load("@root//:rules.bzl", "export_file")

                    def some_macro(name, **kwargs):
                        export_file(
                            name=name+"-exported",
                            **kwargs
                        )
                    "#
            ),
        )
        .unwrap();

    let build_path = BuildFilePath::testing_new("root//some/package:BUILD");
    let eval_result = tester
        .eval_build_file(
            &build_path,
            indoc!(
                r#"
                load("@root//imports:one.bzl", "some_macro")
                load("@root//:rules.bzl", "java_library")

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
            PackageListing::testing_files(&["file1.java", "file2.java"]),
        )
        .unwrap();

    assert_eq!(build_path.package(), eval_result.package());
    let target_names = eval_result
        .targets()
        .keys()
        .map(|t| t.as_str().to_owned())
        .collect::<Vec<_>>();
    assert_eq!(vec!["invoke_some-exported", "java"], target_names);
}

fn cells() -> CellsData {
    let repo_root = if cfg!(windows) { "C:/" } else { "/" };
    let project_fs =
        ProjectRoot::new_unchecked(AbsNormPathBuf::try_from(repo_root.to_owned()).unwrap());
    let BuckConfigBasedCells {
        cell_resolver,
        configs_by_name,
        config_paths: _,
    } = BuckConfigBasedCells::parse_with_file_ops(
        &project_fs,
        &mut TestConfigParserFileOps::new(&[(
            "/.buckconfig",
            indoc!(
                r#"
                    [repositories]
                        root = .
                        cell1 = project/cell1
                        cell2 = project/cell2
                        xalias2 = project/cell2
                    "#
            ),
        )])
        .unwrap(),
        &[],
        ProjectRelativePath::empty(),
    )
    .unwrap();
    (
        cell_resolver
            .get(CellName::testing_new("root"))
            .unwrap()
            .cell_alias_resolver()
            .dupe(),
        cell_resolver,
        configs_by_name,
    )
}

#[test]
fn test_find_imports() {
    let tester = Tester::with_cells(cells()).unwrap();
    let path = BuildFilePath::testing_new("cell1//config:BUCK");
    let parse_result = tester.parse(
        StarlarkPath::BuildFile(&path),
        indoc!(
            r#"
            a = 1
        "#
        ),
    );

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
    );

    assert_eq!(
        &[
            "root//imports/one.bzl@cell1",
            "cell1//one.bzl",
            "cell2//two.bzl@cell1",
            "cell1//config/other.bzl"
        ],
        parse_result.imports().map(|e| e.1.to_string()).as_slice()
    );
}

#[test]
fn test_root_import() {
    let mut tester = Tester::with_cells(
        buck2_interpreter_for_build::interpreter::testing::cells(Some(indoc!(
            r#"
            [buildfile]
                includes = //include.bzl
        "#
        )))
        .unwrap(),
    )
    .unwrap();

    tester.additional_globals(register_rule_defs);

    let import_path = ImportPath::testing_new("root//:include.bzl");
    tester
        .add_import(
            &import_path,
            indoc!(
                r#"
                    some_var = 1
                    def some_func():
                       return "hello"

                    def _impl(ctx):
                        return DefaultInfo()

                    export_file = rule(
                        impl = _impl,
                        attrs = {
                            "level": attrs.int(),
                        },
                    )
        "#
            ),
        )
        .unwrap();

    let build_path = BuildFilePath::testing_new("root//some/package:BUCK");
    let eval_result = tester
        .eval_build_file(
            &build_path,
            indoc!(
                r#"
                export_file(
                    name = some_func(),
                    level = some_var,
                )
                "#
            ),
            PackageListing::testing_files(&["file1.java", "file2.java"]),
        )
        .unwrap();

    assert_eq!(build_path.package(), eval_result.package());
    let target_names = eval_result
        .targets()
        .keys()
        .map(|t| t.as_str().to_owned())
        .collect::<Vec<_>>();
    assert_eq!(vec!["hello"], target_names);
}

#[test]
fn prelude_is_included() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    let prelude_path = ImportPath::testing_new("root//prelude:prelude.bzl");
    tester.set_prelude(prelude_path.clone());

    let prelude = tester.eval_import(&prelude_path, "some_var = 1", LoadedModules::default())?;
    let mut loaded_modules = LoadedModules::default();
    loaded_modules
        .map
        .insert(OwnedStarlarkModulePath::LoadFile(prelude_path), prelude);

    // The prelude should be included in build files, and in .bzl files that are not in the
    // prelude's package
    let build_file = BuildFilePath::testing_new("root//prelude:TARGETS.v2");
    assert!(
        tester
            .eval_build_file_with_loaded_modules(
                &build_file,
                "other_var = some_var",
                loaded_modules.clone(),
                PackageListing::testing_empty()
            )
            .is_ok(),
        "build files in the prelude package should have access to the prelude"
    );

    let import = ImportPath::testing_new("root//not_prelude:sibling.bzl");
    assert!(
        tester
            .eval_import(&import, "other_var = some_var", loaded_modules.clone())
            .is_ok(),
        ".bzl files not in the prelude package should have access to the prelude"
    );

    let import = ImportPath::testing_new("root//prelude:defs.bzl");
    assert!(
        tester
            .eval_import(&import, "other_var = some_var", loaded_modules)
            .is_err(),
        "bzl files in the prelude package should NOT have access to the prelude"
    );

    Ok(())
}

#[test]
fn test_package_import() -> anyhow::Result<()> {
    let mut tester = Tester::with_cells(buck2_interpreter_for_build::interpreter::testing::cells(
        Some(indoc!(
            r#"
            [buildfile]
                package_includes = src=>//include.bzl::func_alias=some_func
        "#
        )),
    )?)?;
    tester.additional_globals(register_rule_defs);
    tester.additional_globals(register_module_natives);

    let import_path = ImportPath::testing_new("root//:include.bzl");
    tester.add_import(
        &import_path,
        indoc!(
            r#"
            def _impl(ctx):
                pass
            export_file = rule(impl=_impl, attrs = {})

            def some_func(name):
                export_file(name = name)
        "#
        ),
    )?;

    let build_path = BuildFilePath::testing_new("root//src/package:BUCK");
    let eval_result = tester.eval_build_file(
        &build_path,
        indoc!(
            r#"
                implicit_package_symbol("func_alias")(
                    implicit_package_symbol("missing", "DEFAULT")
                )
                "#
        ),
        PackageListing::testing_files(&["file1.java", "file2.java"]),
    )?;
    assert_eq!(build_path.package(), eval_result.package());
    assert_eq!(
        json!({
                "DEFAULT": {
                    "__type__": "root//include.bzl:export_file",
                    "compatible_with": [],
                    "default_target_platform": null,
                    "exec_compatible_with": [],
                    "name": "DEFAULT",
                    "target_compatible_with": [],
                    "tests": [],
                    "visibility": [],
                },
        }),
        targets_to_json(
            eval_result.targets(),
            build_path.package(),
            AttrInspectOptions::All
        )?
    );
    Ok(())
}

#[test]
fn eval() -> anyhow::Result<()> {
    let mut tester = Tester::new()?;
    tester.additional_globals(register_module_natives);
    tester.additional_globals(register_rule_defs);
    let content = indoc!(
        r#"
            def _impl(ctx):
                pass
            export_file = rule(impl=_impl, attrs = {})

            def test():
                assert_eq("some/package", __internal__.package_name())
                assert_eq("@root", __internal__.repository_name())

                assert_eq(package_name(), __internal__.package_name())
                assert_eq(repository_name(), __internal__.repository_name())

                assert_eq(package_name(), get_base_path())

                export_file(name = "rule_name")
                assert_eq(True, rule_exists("rule_name"))
                assert_eq(False, rule_exists("not_rule_name"))

                print("some message")
                print("multiple", "strings")
            "#
    );
    tester.run_starlark_test(content)?;
    Ok(())
}

#[test]
fn test_internal() -> anyhow::Result<()> {
    // Test that most things end up on __internal__
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_rule_defs);
    run_simple_starlark_test(indoc!(
        r#"
            def test():
                assert_eq(__internal__.json.encode({}), "{}")
            "#
    ))
}

#[test]
fn test_oncall() -> anyhow::Result<()> {
    let mut tester = Tester::new().unwrap();
    tester.additional_globals(register_module_natives);
    tester.additional_globals(register_rule_defs);
    tester.run_starlark_test(indoc!(
        r#"
            def _impl(ctx):
                pass
            export_file = rule(impl=_impl, attrs = {})

            def test():
                oncall("valid")
                export_file(name = "rule_name")
            "#
    ))?;
    tester.run_starlark_test_expecting_error(
        indoc!(
            r#"
            def test():
                oncall("valid")
                oncall("twice")
            "#
        ),
        "more than once",
    );
    tester.run_starlark_test_expecting_error(
        indoc!(
            r#"
            def _impl(ctx):
                pass
            export_file = rule(impl=_impl, attrs = {})

            def test():
                export_file(name = "rule_name")
                oncall("failure after")
            "#
        ),
        "after one or more targets",
    );
    Ok(())
}
