/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::interpreter::rule_defs::cmd_args::value_as::ValueAsCommandLineLike;
use buck2_build_api::interpreter::rule_defs::cmd_args::DefaultCommandLineContext;
use buck2_common::executor_config::PathSeparatorKind;
use buck2_core::buck_path::resolver::BuckPathResolver;
use buck2_core::fs::artifact_path_resolver::ArtifactFs;
use buck2_core::fs::buck_out_path::BuckOutPathResolver;
use buck2_core::fs::paths::abs_norm_path::AbsNormPathBuf;
use buck2_core::fs::project::ProjectRoot;
use buck2_core::fs::project_rel_path::ProjectRelativePathBuf;
use buck2_execute::artifact::fs::ExecutorFs;
use buck2_interpreter_for_build::interpreter::testing::cells;
use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::values::Value;

fn artifact_fs() -> ArtifactFs {
    let cell_info = cells(None).unwrap();
    ArtifactFs::new(
        BuckPathResolver::new(cell_info.1),
        BuckOutPathResolver::new(ProjectRelativePathBuf::unchecked_new(
            "buck-out/v2".to_owned(),
        )),
        ProjectRoot::new(AbsNormPathBuf::try_from(std::env::current_dir().unwrap()).unwrap())
            .unwrap(),
    )
}

fn get_command_line(value: Value) -> anyhow::Result<Vec<String>> {
    let fs = artifact_fs();
    let executor_fs = ExecutorFs::new(&fs, PathSeparatorKind::Unix);
    let mut cli = Vec::<String>::new();
    let mut ctx = DefaultCommandLineContext::new(&executor_fs);

    match value.as_command_line() {
        Some(v) => v.add_to_command_line(&mut cli, &mut ctx),
        None => value
            .as_command_line_err()?
            .add_to_command_line(&mut cli, &mut ctx),
    }?;
    Ok(cli)
}

#[starlark_module]
pub(crate) fn command_line_stringifier(builder: &mut GlobalsBuilder) {
    fn get_args<'v>(value: Value<'v>) -> anyhow::Result<Vec<String>> {
        get_command_line(value)
    }

    fn stringify_cli_arg<'v>(value: Value<'v>) -> anyhow::Result<String> {
        let fs = artifact_fs();
        let executor_fs = ExecutorFs::new(&fs, PathSeparatorKind::Unix);
        let mut cli = Vec::<String>::new();
        let mut ctx = DefaultCommandLineContext::new(&executor_fs);
        value
            .as_command_line_err()?
            .add_to_command_line(&mut cli, &mut ctx)?;
        assert_eq!(1, cli.len());
        Ok(cli.get(0).unwrap().clone())
    }
}
