# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def inject_test_run_info(ctx: AnalysisContext, test_info: ExternalRunnerTestInfo) -> list[Provider]:
    # Access this here so we get failures in CI if we forget to inject it
    # anywhere, regardless of whether an `env` is used.
    inject_test_env = ctx.attrs._inject_test_env[RunInfo]

    if (not test_info.env) or _exclude_test_env_from_run_info(ctx):
        return [test_info, RunInfo(args = test_info.command)]

    cell_root = ctx.label.cell_root
    project_root = ctx.label.project_root

    relative_to = _maybe_relativize_path(test_info, cell_root, project_root)

    env_file = ctx.actions.write_json(
        "test_env.json",
        {
            k: cmd_args(v, delimiter = " ", relative_to = relative_to)
            for (k, v) in test_info.env.items()
        },
        with_inputs = True,
        absolute = True,
    )

    return [test_info, RunInfo(args = [inject_test_env, env_file, "--", test_info.command])]

# TODO(nga): D59890633 for symbol for `ProjectRoot` type.
def _maybe_relativize_path(test_info: ExternalRunnerTestInfo, cell_root: CellRoot, project_root) -> CellRoot | typing.Any:
    if test_info.run_from_project_root:
        return project_root
    else:
        return cell_root

def _exclude_test_env_from_run_info(ctx: AnalysisContext):
    # Antlir assumes that $(exe ...) is a single, relative path, so if we make
    # it multiple commands here, it'll break.
    return "antlir_inner_test" in ctx.attrs.labels
