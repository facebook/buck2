# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":toolchain.bzl", "PythonToolchainInfo")

def run_lazy_imports_analyzer(
        ctx: AnalysisContext,
        resources,
        output: Artifact,
        dbg_source_db_output: Artifact) -> DefaultInfo:
    """
    Run the lazy imports analyzer (lifeguard) against a Python binary by
    ingesting the existing dbg-db.json.
    """
    python_toolchain = ctx.attrs._python_toolchain[PythonToolchainInfo]
    cmd = cmd_args(ctx.attrs.lazy_imports_analyzer[RunInfo], hidden = resources)
    cmd.add(dbg_source_db_output)  # First arg: <DB_PATH>
    cmd.add(output.as_output())  # Second arg: <OUTPUT_PATH>

    ctx.actions.run(
        cmd,
        category = "py_lazy_import_analysis",
        error_handler = python_toolchain.python_error_handler,
    )

    return DefaultInfo(default_output = output)
