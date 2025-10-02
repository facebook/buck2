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
        output: Artifact,
        dbg_source_db_output: Artifact) -> DefaultInfo:
    """
    Run the lazy imports analyzer (lifeguard) for a Python binary using the
    existing dbg-db.json.

    The dbg-db.json already contains all the source files needed in this format:
    {
      "sources": {"module.name": "/path/to/source.py", ...},
      "dependencies": {"dep.module": "/path/to/dep.py", ...}
    }
    """

    # Get the lazy imports analyzer (lifeguard) from the toolchain
    python_toolchain = ctx.attrs._python_toolchain[PythonToolchainInfo]
    lazy_imports_analyzer = python_toolchain.lazy_imports_analyzer
    if lazy_imports_analyzer == None:
        # If no lazy imports tool is configured, write an empty dict to the
        # output file
        ctx.actions.write_json(output, {})
        return DefaultInfo(default_output = output)

    # Create a converter script to transform dbg-db.json to the format expected
    # by lifeguard
    # TODO(T239924112): Remove the need for a converter script
    converter_script = ctx.actions.write(
        "convert_dbg_to_build_map.py",
        '''#!/usr/bin/env python3
import json
import sys

# Read the dbg-db.json file
with open(sys.argv[1], 'r') as f:
    dbg_data = json.load(f)

# Convert to the format expected by lifeguard (same as classic.bxl merger output)
build_map = {}
build_map.update(dbg_data.get("sources", {}))
build_map.update(dbg_data.get("dependencies", {}))

converted_data = {
    "build_map": build_map,
    "built_targets_count": len(build_map),
    "dropped_targets": {}
}

# Write the converted format to output file
with open(sys.argv[2], 'w') as f:
    json.dump(converted_data, f, indent=2)
''',
        is_executable = True,
    )

    # Convert dbg-db.json to the format expected by lifeguard
    converted_db = ctx.actions.declare_output("converted-db.json")
    convert_cmd = cmd_args([
        python_toolchain.interpreter,
        converter_script,
        dbg_source_db_output,
        converted_db.as_output(),
    ])

    ctx.actions.run(
        convert_cmd,
        category = "convert_dbg_db",
        error_handler = python_toolchain.python_error_handler,
    )

    # Run the lazy imports analyzer with the converted data
    cmd = cmd_args(lazy_imports_analyzer[RunInfo])
    cmd.add(converted_db)  # First arg: <MERGED_DB_PATH>
    cmd.add(output.as_output())  # Second arg: <OUTPUT_PATH>

    ctx.actions.run(
        cmd,
        category = "py_lazy_import_analysis",
        error_handler = python_toolchain.python_error_handler,
    )

    return DefaultInfo(default_output = output)
