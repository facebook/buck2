# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _simple_write_impl(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.write(out, ctx.attrs.string_attr)

    return [DefaultInfo(default_output = out)]

simple_write = rule(
    impl = _simple_write_impl,
    attrs = {
        "string_attr": attrs.string(),
    },
)

def _simple_write_json_impl(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.write_json(out, {"key": ctx.attrs.string_attr})

    return [DefaultInfo(default_output = out)]

simple_write_json = rule(
    impl = _simple_write_json_impl,
    attrs = {
        "string_attr": attrs.string(),
    },
)

def _simple_copy_impl(ctx):
    out = ctx.actions.declare_output("out")
    ctx.actions.copy_file(out, ctx.attrs.to_copy)

    return [DefaultInfo(default_output = out)]

simple_copy = rule(
    impl = _simple_copy_impl,
    attrs = {
        "to_copy": attrs.source(),
    },
)

def _simple_symlinked_dir_impl(ctx):
    srcs = {str(k): v for (k, v) in enumerate(ctx.attrs.inputs)}
    out = ctx.actions.symlinked_dir("out", srcs)

    return [DefaultInfo(default_output = out)]

simple_symlinked_dir = rule(
    impl = _simple_symlinked_dir_impl,
    attrs = {
        "inputs": attrs.list(attrs.source()),
    },
)

def _simple_run_impl(ctx):
    script = ctx.actions.write(
        "script.py",
        [
            "import sys",
            "with open(sys.argv[1], 'w') as f:",
            "  f.write(sys.argv[2])",
        ],
    )

    out = ctx.actions.declare_output("out")
    args = cmd_args(["python3", script, out.as_output(), ctx.attrs.string_attr])
    ctx.actions.run(args, category = "test_run")

    return [DefaultInfo(default_output = out)]

simple_run = rule(
    impl = _simple_run_impl,
    attrs = {
        "string_attr": attrs.string(),
    },
)

def project(f: Artifact):
    return f

NameSet = transitive_set(args_projections = {
    "project": project,
})

NameInfo = provider(fields = ["tset"])

def _simple_tset_impl(ctx):
    artifact1 = ctx.actions.write("artifact1", "hello_artifact_1_{}".format(ctx.attrs.string_attr))
    artifact2 = ctx.actions.write("artifact2", "hello_artifact_2_{}".format(ctx.attrs.string_attr))
    tset = ctx.actions.tset(NameSet, children = [
        ctx.actions.tset(NameSet, value = artifact1),
        ctx.actions.tset(NameSet, value = artifact2),
    ])

    out = ctx.actions.write("tset_output", tset.project_as_args("project"))

    return [DefaultInfo(default_output = out)]

simple_tset = rule(
    impl = _simple_tset_impl,
    attrs = {
        "string_attr": attrs.string(),
    },
)

def _simple_dynamic_impl(ctx: AnalysisContext) -> list[Provider]:
    input = ctx.actions.write("input", str("input"))
    output = ctx.actions.declare_output("out")

    def f(ctx: AnalysisContext, artifacts, outputs):
        src = artifacts[input].read_string()
        ctx.actions.write(outputs[output], src)

    ctx.actions.dynamic_output(dynamic = [input], inputs = [], outputs = [output.as_output()], f = f)
    return [DefaultInfo(default_output = output)]

simple_dynamic = rule(
    impl = _simple_dynamic_impl,
    attrs = {
    },
)

def _write_arg(ctx: AnalysisContext) -> list[Provider]:
    out, _ = ctx.actions.write(
        "out.txt",
        ctx.attrs.arg_command,
        is_executable = True,
        allow_args = True,
    )
    return [DefaultInfo(default_output = out)]

write_arg = rule(
    impl = _write_arg,
    attrs = {
        "arg_command": attrs.arg(),
    },
)
