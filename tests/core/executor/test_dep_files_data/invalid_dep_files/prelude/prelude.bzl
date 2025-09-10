# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

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

def _n_outputs_tagged_as_dep_file_impl(ctx):
    app = ctx.actions.declare_output("app")
    args = cmd_args([
        "sh",
        "-c",
        "never_executed",
        "--",
        app.as_output(),
    ])

    tag = ctx.actions.artifact_tag()
    for i in range(ctx.attrs.num_dep_files):
        dep_file = ctx.actions.declare_output("depfile{}".format(i))
        args.add(tag.tag_artifacts(dep_file.as_output()))

    ctx.actions.run(
        args,
        category = "test",
        dep_files = {"deps": tag},
    )

    return [
        DefaultInfo(
            default_output = app,
        ),
    ]

n_outputs_tagged_as_dep_file = rule(
    attrs = {
        "num_dep_files": attrs.int(),
    },
    impl = _n_outputs_tagged_as_dep_file_impl,
)

def _same_tag_for_multiple_labels_impl(ctx):
    app = ctx.actions.declare_output("app")
    tag = ctx.actions.artifact_tag()
    dep_file = ctx.actions.declare_output("depfile")
    args = cmd_args([
        "sh",
        "-c",
        "never_executed",
        "--",
        app.as_output(),
        tag.tag_artifacts(dep_file.as_output()),
    ])

    ctx.actions.run(
        args,
        category = "test",
        dep_files = {"deps": tag, "deps2": tag},
    )

    return [
        DefaultInfo(
            default_output = app,
        ),
    ]

same_tag_for_multiple_labels = rule(
    attrs = {
    },
    impl = _same_tag_for_multiple_labels_impl,
)
