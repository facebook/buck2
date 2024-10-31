# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _simple(ctx):
    re_use_case = read_config("buck2_re_client", "override_use_case")
    if re_use_case != None:
        fail("RE use case is set to: {}".format(re_use_case))
    output = ctx.actions.declare_output("output")
    run = ctx.actions.write(
        "run.py",
        [
            "import os",
            "import sys",
            "build_id = os.environ[\"BUCK_BUILD_ID\"]",
            "with open(sys.argv[1], 'w') as f:",
            "  f.write(f'{build_id}\\n')",
        ],
    )
    ctx.actions.run(
        cmd_args(["python3", run, output.as_output()]),
        category = "test_category",
    )

    return [DefaultInfo(default_output = output)]

simple = rule(impl = _simple, attrs = {})
