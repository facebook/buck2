# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def project(f: Artifact):
    return f

NameSet = transitive_set(args_projections = {
    "project": project,
})

def _write_with_content_based_path_impl(ctx):
    artifact_input = ctx.actions.declare_output("artifact_input", uses_experimental_content_based_path_hashing = True)
    artifact_input = ctx.actions.write(artifact_input, "artifact_input")

    tset_item1 = ctx.actions.declare_output("tset_item1", uses_experimental_content_based_path_hashing = True)
    tset1 = ctx.actions.tset(NameSet, value = ctx.actions.write(tset_item1, "tset_item1"))
    tset_item2 = ctx.actions.declare_output("tset_item2", uses_experimental_content_based_path_hashing = True)
    tset2 = ctx.actions.tset(NameSet, value = ctx.actions.write(tset_item2, "tset_item2"))
    tset = ctx.actions.tset(NameSet, children = [tset1, tset2])

    out = ctx.actions.declare_output("out.txt", uses_experimental_content_based_path_hashing = True)
    return [DefaultInfo(default_output = ctx.actions.write(out, cmd_args(artifact_input, ctx.attrs.data, tset.project_as_args("project"))))]

write_with_content_based_path = rule(
    impl = _write_with_content_based_path_impl,
    attrs = {
        "data": attrs.string(),
    },
)

def _run_remote_with_content_based_path_impl(ctx):
    script = ctx.actions.declare_output("script.py", uses_experimental_content_based_path_hashing = True)
    script = ctx.actions.write(
        script,
        [
            "import sys",
            "with open(sys.argv[1], 'w') as f:",
            "  f.write(sys.argv[2])",
        ],
    )

    out = ctx.actions.declare_output("out", uses_experimental_content_based_path_hashing = True)
    args = cmd_args(["python3", script, out.as_output(), ctx.attrs.data])
    ctx.actions.run(args, category = "test_run", prefer_remote = True)

    return [DefaultInfo(default_output = out)]

run_remote_with_content_based_path = rule(
    impl = _run_remote_with_content_based_path_impl,
    attrs = {
        "data": attrs.string(),
    },
)

def _copy_impl(ctx):
    out = ctx.actions.declare_output("out", uses_experimental_content_based_path_hashing = True)
    ctx.actions.copy_file(out, ctx.attrs.to_copy)

    return [DefaultInfo(default_output = out)]

copy = rule(
    impl = _copy_impl,
    attrs = {
        "to_copy": attrs.source(),
    },
)

def _symlink_impl(ctx):
    out = ctx.actions.declare_output("out", uses_experimental_content_based_path_hashing = True)
    ctx.actions.symlink_file(out, ctx.attrs.to_symlink)

    return [DefaultInfo(default_output = out)]

symlink = rule(
    impl = _symlink_impl,
    attrs = {
        "to_symlink": attrs.source(),
    },
)
