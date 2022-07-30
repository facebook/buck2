/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::testing::SetTestingIoProvider;
use buck2_common::legacy_configs::dice::HasLegacyConfigs;
use buck2_common::legacy_configs::LegacyBuckConfig;
use buck2_common::legacy_configs::LegacyBuckConfigs;
use buck2_core::bzl::ImportPath;
use buck2_core::cells::build_file_cell::BuildFileCell;
use buck2_core::cells::cell_root_path::CellRootPathBuf;
use buck2_core::cells::paths::CellRelativePath;
use buck2_core::cells::testing::CellResolverExt;
use buck2_core::cells::CellAlias;
use buck2_core::cells::CellName;
use buck2_core::cells::CellResolver;
use buck2_core::fs::paths::ForwardRelativePath;
use buck2_core::fs::project::ProjectFilesystemTemp;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::fs::project::ProjectRelativePathBuf;
use buck2_core::package::Package;
use dice::cycles::DetectCycles;
use dice::Dice;
use dice::DiceTransaction;
use dice::UserComputationData;
use events::dispatch::EventDispatcher;
use gazebo::dupe::Dupe;
use indoc::indoc;
use maplit::hashmap;
use serde_json::json;

use crate::common::StarlarkModulePath;
use crate::dice::starlark_profiler::SetStarlarkProfilerInstrumentation;
use crate::dice::starlark_types::SetDisableStarlarkTypes;
use crate::dice::HasCalculationDelegate;
use crate::dice::HasInterpreterContext;
use crate::extra::testing::TesterConfiguror;
use crate::extra::testing::TesterExtraContext;
use crate::starlark_profiler::StarlarkProfilerOrInstrumentation;

fn rules() -> Vec<String> {
    vec![
        "export_file".to_owned(),
        "genrule".to_owned(),
        "java_library".to_owned(),
    ]
}

fn empty_configs(resolver: &CellResolver) -> LegacyBuckConfigs {
    let config = resolver
        .cells()
        .map(|(name, _)| (name.clone(), LegacyBuckConfig::empty()))
        .collect();

    LegacyBuckConfigs::new(config)
}

fn import(cell: &str, package: &str, filename: &str) -> ImportPath {
    ImportPath::unchecked_new(cell, package, filename)
}

fn root_cell() -> CellName {
    CellName::unchecked_new("".to_owned())
}

fn calculation(fs: &ProjectFilesystemTemp) -> anyhow::Result<DiceTransaction> {
    let mut dice = Dice::builder();
    dice.set(EventDispatcher::null());
    dice.set_testing_io_provider(fs);
    let dice = dice.build(DetectCycles::Enabled);

    let mut per_transaction_data = UserComputationData::new();
    per_transaction_data.data.set(EventDispatcher::null());
    let ctx = dice.with_ctx_data(per_transaction_data);

    let resolver = CellResolver::with_names_and_paths_with_alias(&[(
        CellName::unchecked_new("".to_owned()),
        CellRootPathBuf::new(ProjectRelativePathBuf::unchecked_new("".to_owned())),
        hashmap![
            CellAlias::new("".to_owned()) => CellName::unchecked_new("".to_owned())
        ],
    )]);
    let cell_configs = empty_configs(&resolver);

    ctx.set_cell_resolver(resolver.dupe())?;
    ctx.set_interpreter_context(TesterConfiguror::new(rules()))?;
    ctx.set_legacy_configs(cell_configs)?;
    ctx.set_starlark_profiler_instrumentation_override(None)?;
    ctx.set_disable_starlark_types(false)?;
    Ok(ctx.commit())
}

#[tokio::test]
async fn test_eval_import() -> anyhow::Result<()> {
    let fs = ProjectFilesystemTemp::new()?;
    fs.path().write_file(
        ProjectRelativePath::new("package/two.bzl")?,
        indoc!(
            r#"
        message = "hello world!"
        "#
        ),
        false,
    )?;

    let ctx = calculation(&fs)?;

    let calculation = ctx
        .get_interpreter_calculator(&root_cell(), &BuildFileCell::new(root_cell()))
        .await?;

    let env = calculation
        .eval_module(StarlarkModulePath::LoadFile(&import(
            "", "package", "two.bzl",
        )))
        .await?;
    assert_eq!(
        "hello world!",
        env.env().get("message").unwrap().unpack_str().unwrap()
    );
    Ok(())
}

// TODO: this test require imports extractions
#[tokio::test]
async fn test_eval_import_with_load() -> anyhow::Result<()> {
    let fs = ProjectFilesystemTemp::new()?;

    fs.path().write_file(
        ProjectRelativePath::new("imports/one.bzl")?,
        indoc!(
            r#"
            def concat(*args):
              s = ""
              for a in args:
                s += a
              return s
            "#
        ),
        false,
    )?;
    fs.path().write_file(
        ProjectRelativePath::new("package/two.bzl")?,
        indoc!(
            r#"
        load("//imports:one.bzl", "concat")
        message = concat("hello", " ", "world!")
            "#
        ),
        false,
    )?;

    let ctx = calculation(&fs)?;
    let calculation = ctx
        .get_interpreter_calculator(&root_cell(), &BuildFileCell::new(root_cell()))
        .await?;

    let env = calculation
        .eval_module(StarlarkModulePath::LoadFile(&import(
            "", "package", "two.bzl",
        )))
        .await?;
    assert_eq!(
        "hello world!",
        env.env().get("message").unwrap().unpack_str().unwrap()
    );
    Ok(())
}

// TODO: this test require imports extractions
#[tokio::test]
async fn test_eval_build_file() -> anyhow::Result<()> {
    let fs = ProjectFilesystemTemp::new()?;
    fs.path().write_file(
        ProjectRelativePath::new("imports/one.bzl")?,
        indoc!(
            r#"
                def some_macro(name, **kwargs):
                    export_file(
                        name=name+"-exported",
                        **kwargs
                    )
        "#
        ),
        false,
    )?;
    fs.path()
        .write_file(ProjectRelativePath::new("package/file1.java")?, "", false)?;
    fs.path()
        .write_file(ProjectRelativePath::new("package/file2.java")?, "", false)?;
    fs.path().write_file(
        ProjectRelativePath::new("package/BUCK")?,
        indoc!(
            r#"
            load("//imports:one.bzl", "some_macro")
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
        false,
    )?;

    let ctx = calculation(&fs)?;
    let calculation = ctx
        .get_interpreter_calculator(&root_cell(), &BuildFileCell::new(root_cell()))
        .await?;

    let package = Package::new(
        &root_cell(),
        CellRelativePath::new(ForwardRelativePath::new("package")?),
    );
    let eval_result = calculation
        .eval_build_file::<TesterExtraContext>(
            &package,
            &mut StarlarkProfilerOrInstrumentation::disabled(),
        )
        .await?;
    assert_eq!(package, eval_result.package);
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
