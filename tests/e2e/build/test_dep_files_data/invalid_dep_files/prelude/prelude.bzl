# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _test_impl(ctx):
    tag = ctx.actions.artifact_tag()

    dep_file = ctx.actions.declare_output("depfile")
    app = ctx.actions.declare_output("app")

    seed = ctx.actions.write("seed", ctx.attrs.seed)

    ctx.actions.run(
        [
            "sh",
            "-c",
            'echo "../invalid" > "$1" && touch "$2" && echo "$3"',
            "--",
            tag.tag_artifacts(dep_file.as_output()),
            app.as_output(),
            seed,
        ],
        category = "test",
        dep_files = {"deps": tag},
    )

    return [
        DefaultInfo(
            default_output = app,
            sub_targets = {"dep_file": [DefaultInfo(default_output = dep_file)]},
        ),
    ]

test = rule(
    attrs = {
        "seed": attrs.string(default = ""),
    },
    impl = _test_impl,
)
