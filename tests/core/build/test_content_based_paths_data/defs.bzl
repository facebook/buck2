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

def _write_macro_with_content_based_path_impl(ctx):
    out = ctx.actions.declare_output("out.txt", uses_experimental_content_based_path_hashing = True)
    out, macro_files = ctx.actions.write(out, ctx.attrs.arg, allow_args = True)
    if not len(macro_files) == 1:
        fail("Expected exactly one macro file, got {}".format(macro_files))
    else:
        macro_file = macro_files[0]

    return [DefaultInfo(default_output = macro_file, other_outputs = [out])]

write_macro_with_content_based_path = rule(
    impl = _write_macro_with_content_based_path_impl,
    attrs = {
        "arg": attrs.arg(),
    },
)

def _write_json_with_content_based_path_impl(ctx):
    out = ctx.actions.declare_output("out.json", uses_experimental_content_based_path_hashing = True)
    out = ctx.actions.write_json(
        out,
        {
            "baz": 42,
            "foo": "bar",
            "source": ctx.attrs.source,
        },
    )
    return [DefaultInfo(default_output = out)]

write_json_with_content_based_path = rule(
    impl = _write_json_with_content_based_path_impl,
    attrs = {
        "source": attrs.source(),
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

def _cas_artifact_with_content_based_path_impl(ctx: AnalysisContext):
    out = ctx.actions.declare_output("out", uses_experimental_content_based_path_hashing = True)
    out = ctx.actions.cas_artifact(
        out,
        ctx.attrs.digest,
        ctx.attrs.use_case,
        expires_after_timestamp = ctx.attrs.expires_after_timestamp,
        is_tree = ctx.attrs.is_tree,
        is_directory = ctx.attrs.is_directory,
    )
    return [DefaultInfo(default_output = out)]

cas_artifact_with_content_based_path = rule(
    impl = _cas_artifact_with_content_based_path_impl,
    attrs = {
        "digest": attrs.string(),
        "expires_after_timestamp": attrs.int(),
        "is_directory": attrs.bool(default = False),
        "is_tree": attrs.bool(default = False),
        "use_case": attrs.string(),
    },
)

def _failing_validation_with_content_based_path_impl(ctx):
    validation = ctx.actions.declare_output("validation.json", uses_experimental_content_based_path_hashing = True)
    validation = ctx.actions.write_json(validation, {
        "data": {
            "message": "This is a failing validation",
            "status": "failure",
        },
        "version": 1,
    }, pretty = True)

    return [
        DefaultInfo(default_output = ctx.actions.write("out", "hello world")),
        ValidationInfo(validations = [
            ValidationSpec(
                name = "whistle",
                validation_result = validation,
            ),
        ]),
    ]

failing_validation_with_content_based_path = rule(
    impl = _failing_validation_with_content_based_path_impl,
    attrs = {},
)

def _dynamic_with_content_based_path_impl(ctx: AnalysisContext) -> list[Provider]:
    input = ctx.actions.declare_output("input", uses_experimental_content_based_path_hashing = True)
    input = ctx.actions.write(input, str("input"))
    output = ctx.actions.declare_output("out", uses_experimental_content_based_path_hashing = True)

    def f(ctx: AnalysisContext, artifacts, outputs):
        src = artifacts[input].read_string()
        ctx.actions.write(outputs[output], src)

    ctx.actions.dynamic_output(dynamic = [input], inputs = [], outputs = [output.as_output()], f = f)
    return [DefaultInfo(default_output = output)]

dynamic_with_content_based_path = rule(
    impl = _dynamic_with_content_based_path_impl,
    attrs = {
    },
)

def _basic_dynamic_output_new_impl(actions: AnalysisActions, src: ArtifactValue, out: OutputArtifact):
    actions.write(out, src.read_string())
    return []

_basic_dynamic_output_new = dynamic_actions(
    impl = _basic_dynamic_output_new_impl,
    attrs = {
        "out": dynattrs.output(),
        "src": dynattrs.artifact_value(),
    },
)

def _dynamic_new_with_content_based_path_impl(ctx: AnalysisContext) -> list[Provider]:
    input = ctx.actions.declare_output("input", uses_experimental_content_based_path_hashing = True)
    input = ctx.actions.write(input, str("input"))
    output = ctx.actions.declare_output("out", uses_experimental_content_based_path_hashing = True)

    ctx.actions.dynamic_output_new(_basic_dynamic_output_new(
        src = input,
        out = output.as_output(),
    ))
    return [DefaultInfo(default_output = output)]

dynamic_new_with_content_based_path = rule(
    impl = _dynamic_new_with_content_based_path_impl,
    attrs = {
    },
)
