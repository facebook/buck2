/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use buck2_build_api::interpreter::rule_defs::register_rule_defs;
use buck2_common::dice::cells::SetCellResolver;
use buck2_common::dice::data::testing::SetTestingIoProvider;
use buck2_common::legacy_configs::dice::SetLegacyConfigs;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_common::legacy_configs::LegacyBuckConfigs;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::name::CellName;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::forward_rel_path::ForwardRelativePath;
use buck2_core::fs::project::ProjectRootTemp;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_core::package::PackageLabel;
use buck2_events::dispatch::with_dispatcher_async;
use buck2_events::dispatch::EventDispatcher;
use buck2_interpreter::dice::starlark_debug::SetStarlarkDebugger;
use buck2_interpreter::dice::starlark_profiler::SetStarlarkProfilerInstrumentation;
use buck2_interpreter::dice::starlark_profiler::StarlarkProfilerConfiguration;
use buck2_interpreter::dice::starlark_types::SetDisableStarlarkTypes;
use buck2_interpreter::extra::InterpreterHostArchitecture;
use buck2_interpreter::extra::InterpreterHostPlatform;
use buck2_interpreter::path::StarlarkModulePath;
use buck2_interpreter::starlark_profiler::StarlarkProfilerOrInstrumentation;
use buck2_interpreter_for_build::interpreter::configuror::BuildInterpreterConfiguror;
use buck2_interpreter_for_build::interpreter::context::SetInterpreterContext;
use buck2_interpreter_for_build::interpreter::dice_calculation_delegate::HasCalculationDelegate;
use buck2_interpreter_for_build::super_package::defs::register_package_natives;
use buck2_interpreter_for_build::super_package::package_value::register_read_package_value;
use dice::DetectCycles;
use dice::Dice;
use dice::DiceTransaction;
use dice::UserComputationData;
use dupe::Dupe;
use indoc::indoc;

fn empty_configs(resolver: &CellResolver) -> LegacyBuckConfigs {
    let config = resolver
        .cells()
        .map(|(name, _)| (name, LegacyBuckConfig::empty()))
        .collect();

    LegacyBuckConfigs::new(config)
}

pub(crate) fn root_cell() -> CellName {
    CellName::testing_new("root")
}

pub(crate) async fn calculation(fs: &ProjectRootTemp) -> DiceTransaction {
    let mut dice = Dice::builder();
    dice.set(EventDispatcher::null());
    dice.set_testing_io_provider(fs);
    let dice = dice.build(DetectCycles::Enabled);

    let mut per_transaction_data = UserComputationData::new();
    per_transaction_data.data.set(EventDispatcher::null());
    per_transaction_data.set_starlark_debugger_handle(None);
    let mut ctx = dice.updater_with_data(per_transaction_data);

    let resolver = CellResolver::testing_with_names_and_paths_with_alias(&[(
        CellName::testing_new("root"),
        CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("".to_owned())),
        HashMap::new(),
    )]);
    let cell_configs = empty_configs(&resolver);

    ctx.set_cell_resolver(resolver.dupe()).unwrap();
    ctx.set_interpreter_context(
        BuildInterpreterConfiguror::new(
            None,
            InterpreterHostPlatform::Linux,
            InterpreterHostArchitecture::X86_64,
            None,
            false,
            register_read_package_value,
            register_package_natives,
            register_rule_defs,
            |_| {},
            None,
        )
        .unwrap(),
    )
    .unwrap();
    ctx.set_legacy_configs(cell_configs).unwrap();
    ctx.set_starlark_profiler_instrumentation_override(StarlarkProfilerConfiguration::default())
        .unwrap();
    ctx.set_disable_starlark_types(false).unwrap();
    ctx.commit().await
}

#[tokio::test]
async fn test_eval_import() {
    let fs = ProjectRootTemp::new().unwrap();
    fs.write_file(
        "pkg/two.bzl",
        indoc!(
            r#"
        message = "hello world!"
        "#
        ),
    );

    let ctx = calculation(&fs).await;

    let calculation = ctx
        .get_interpreter_calculator(root_cell(), BuildFileCell::new(root_cell()))
        .await
        .unwrap();

    let env = calculation
        .eval_module(StarlarkModulePath::LoadFile(&ImportPath::testing_new(
            "root//pkg:two.bzl",
        )))
        .await
        .unwrap();
    assert_eq!(
        "hello world!",
        env.env().get("message").unwrap().unpack_str().unwrap()
    );
}

// TODO: this test require imports extractions
#[tokio::test]
async fn test_eval_import_with_load() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file(
        "imports/one.bzl",
        indoc!(
            r#"
                def concat(*args):
                    s = ""
                    for a in args:
                        s += a
                    return s
            "#
        ),
    );
    fs.write_file(
        "pkg/two.bzl",
        indoc!(
            r#"
                load("//imports:one.bzl", "concat")
                message = concat("hello", " ", "world!")
            "#
        ),
    );

    let ctx = calculation(&fs).await;
    let calculation = ctx
        .get_interpreter_calculator(root_cell(), BuildFileCell::new(root_cell()))
        .await
        .unwrap();

    let env = calculation
        .eval_module(StarlarkModulePath::LoadFile(&ImportPath::testing_new(
            "root//pkg:two.bzl",
        )))
        .await
        .unwrap();
    assert_eq!(
        "hello world!",
        env.env().get("message").unwrap().unpack_str().unwrap()
    );
}

// TODO: this test require imports extractions
#[tokio::test]
async fn test_eval_build_file() {
    let fs = ProjectRootTemp::new().unwrap();

    fs.write_file(
        "rules.bzl",
        indoc!(
            r#"
                def _impl(ctx):
                    return DefaultInfo()

                export_file = rule(
                    impl = _impl,
                    attrs = {
                        "src": attrs.string(),
                    },
                )

                java_library = rule(
                    impl = _impl,
                    attrs = {
                        "srcs": attrs.list(attrs.string()),
                    },
                )
        "#
        ),
    );

    fs.write_file(
        "imports/one.bzl",
        indoc!(
            r#"
                load("//rules.bzl", "export_file")

                def some_macro(name, **kwargs):
                    export_file(
                        name=name+"-exported",
                        **kwargs
                    )
            "#
        ),
    );
    fs.write_file("pkg/file1.java", "");
    fs.write_file("pkg/file2.java", "");
    fs.write_file(
        "pkg/BUCK",
        indoc!(
            r#"
                load("//imports:one.bzl", "some_macro")
                load("//rules.bzl", "java_library")

                some_macro(
                    name = "invoke_some",
                    src = "some.file",
                )
                java_library(
                    name = "java",
                    srcs = glob(["*.java"]),
                )
            "#
        ),
    );

    let ctx = calculation(&fs).await;
    let calculation = ctx
        .get_interpreter_calculator(root_cell(), BuildFileCell::new(root_cell()))
        .await
        .unwrap();

    let package = PackageLabel::new(
        root_cell(),
        CellRelativePath::new(ForwardRelativePath::new("pkg").unwrap()),
    );
    let eval_result = with_dispatcher_async(
        EventDispatcher::null(),
        calculation.eval_build_file(
            package.dupe(),
            &mut StarlarkProfilerOrInstrumentation::disabled(),
        ),
    )
    .await
    .unwrap();
    assert_eq!(package.dupe(), eval_result.package());
    let target_names = eval_result
        .targets()
        .keys()
        .map(|t| t.as_str())
        .collect::<Vec<_>>();

    assert_eq!(vec!["invoke_some-exported", "java"], target_names);
}
