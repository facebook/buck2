# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

Artifacts = provider(fields = ["associated", "artifacts"])

def _artifacts(ctx: AnalysisContext) -> list[Provider]:
    out1 = ctx.actions.write("out1", "")
    out2 = ctx.actions.write("out2", "")
    return [
        DefaultInfo(),
        Artifacts(
            associated = out1.with_associated_artifacts([out2]),
            artifacts = [out1, out2],
        ),
    ]

artifacts = rule(impl = _artifacts, attrs = {})

def _check(ctx: AnalysisContext) -> list[Provider]:
    dep1 = ctx.attrs.dep1[Artifacts]
    dep2 = ctx.attrs.dep2[Artifacts]

    # Those are the paths we'll check.
    all_artifacts = []
    all_artifacts.extend(dep1.artifacts)
    all_artifacts.extend(dep2.artifacts)

    inputs = []

    # Use associated artifacts on declared artifact from dep1
    inputs.append(dep1.associated)

    # Use associated artifacts on frozen artifact from dep2
    (out1, out2) = dep2.artifacts
    inputs.append(out1.with_associated_artifacts([out2]))

    check = ctx.actions.declare_output("check")
    ctx.actions.run(
        [
            "python3",
            "-c",
            ";".join([
                "import sys, os",
                "assert all(os.path.exists(f) for f in sys.argv[1:])",
                "open(os.environ['OUT'], 'w')",
            ]),
            # Look for all the artifacts but don't add a dependency here.
            cmd_args(all_artifacts, ignore_artifacts = True),
            # Actually make those artifacts only available
            cmd_args(hidden = inputs),
        ],
        env = {"OUT": check.as_output()},
        category = "check",
    )

    return [DefaultInfo(check)]

check = rule(impl = _check, attrs = {"dep1": attrs.dep(), "dep2": attrs.dep()})

def defs():
    artifacts(name = "dep1")
    artifacts(name = "dep2")
    check(name = "check", dep1 = ":dep1", dep2 = ":dep2")
