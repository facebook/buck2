# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def project(f: Artifact):
    return f

NameSet = transitive_set(args_projections = {
    "project": project,
})

def _write_with_content_based_path_impl(ctx):
    artifact_input = ctx.actions.write("artifact_input", "artifact_input", has_content_based_path = True)

    tset_item1 = ctx.actions.declare_output("tset_item1", has_content_based_path = True)
    tset1 = ctx.actions.tset(NameSet, value = ctx.actions.write(tset_item1, "tset_item1"))
    tset_item2 = ctx.actions.declare_output("tset_item2", has_content_based_path = True)
    tset2 = ctx.actions.tset(NameSet, value = ctx.actions.write(tset_item2, "tset_item2"))
    tset = ctx.actions.tset(NameSet, children = [tset1, tset2])

    out = ctx.actions.declare_output("out.txt", has_content_based_path = True)
    return [DefaultInfo(default_output = ctx.actions.write(out, cmd_args(artifact_input, ctx.attrs.data, tset.project_as_args("project"))))]

write_with_content_based_path = rule(
    impl = _write_with_content_based_path_impl,
    attrs = {
        "data": attrs.string(),
    },
)

def _write_macro_with_content_based_path_impl(ctx):
    out = ctx.actions.declare_output("out.txt", has_content_based_path = True)
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
    out = ctx.actions.write_json(
        "out.json",
        {
            "baz": 42,
            "foo": "bar",
            "source": ctx.attrs.source,
        },
        has_content_based_path = True,
    )
    return [DefaultInfo(default_output = out)]

write_json_with_content_based_path = rule(
    impl = _write_json_with_content_based_path_impl,
    attrs = {
        "source": attrs.source(),
    },
)

def _run_with_content_based_path_impl(ctx):
    script = ctx.actions.declare_output("script.py", has_content_based_path = True)
    script = ctx.actions.write(
        script,
        [
            "import sys",
            "with open(sys.argv[1], 'w') as f:",
            "  f.write(sys.argv[2])",
        ],
    )

    out = ctx.actions.declare_output("out", has_content_based_path = True)
    args = cmd_args(["fbpython", script, out.as_output(), ctx.attrs.data])
    args.add(cmd_args(hidden = ctx.attrs.depends_on))
    kwargs = {
        "category": "test_run",
        "expect_eligible_for_dedupe": True,
        "outputs_for_error_handler": [out.as_output()],
    }
    if ctx.attrs.prefer_local:
        kwargs["prefer_local"] = True
    else:
        kwargs["prefer_remote"] = True

    ctx.actions.run(args, **kwargs)

    return [DefaultInfo(default_output = out), RunInfo(args = [out])]

run_with_content_based_path = rule(
    impl = _run_with_content_based_path_impl,
    attrs = {
        "data": attrs.string(),
        "depends_on": attrs.list(attrs.source(), default = []),
        "prefer_local": attrs.bool(default = False),
        "_ignored": attrs.string(default = "ignored"),
    },
)

def _copy_impl(ctx):
    out = ctx.actions.copy_file("out", ctx.attrs.to_copy, has_content_based_path = True)

    return [DefaultInfo(default_output = out)]

copy = rule(
    impl = _copy_impl,
    attrs = {
        "to_copy": attrs.source(),
    },
)

def _symlink_impl(ctx):
    out = ctx.actions.symlink_file("out", ctx.attrs.to_symlink, has_content_based_path = True)

    return [DefaultInfo(default_output = out)]

symlink = rule(
    impl = _symlink_impl,
    attrs = {
        "to_symlink": attrs.source(),
    },
)

def _symlink_and_copy_impl(ctx):
    written = ctx.actions.write("written", "written", has_content_based_path = True)
    symlink = ctx.actions.symlink_file("symlink", written, has_content_based_path = True)

    script = ctx.actions.declare_output("script.py", has_content_based_path = True)
    script = ctx.actions.write(
        script,
        [
            "import shutil",
            "import sys",
            "shutil.copyfile(sys.argv[1], sys.argv[2])",
        ],
    )

    out = ctx.actions.declare_output("out", has_content_based_path = True)
    args = cmd_args(["fbpython", script, symlink, out.as_output()])

    ctx.actions.run(args, category = "test_run")

    return [DefaultInfo(default_output = out)]

symlink_and_copy = rule(
    impl = _symlink_and_copy_impl,
    attrs = {},
)

def _copied_dir_impl(ctx):
    another_to_copy = ctx.actions.declare_output("another_to_copy", has_content_based_path = True)
    ctx.actions.write(another_to_copy, "another_to_copy")
    out = ctx.actions.copied_dir(
        "out",
        {
            "another_to_copy": another_to_copy,
            "to_copy": ctx.attrs.to_copy,
        },
        has_content_based_path = True,
    )

    return [DefaultInfo(default_output = out)]

copied_dir = rule(
    impl = _copied_dir_impl,
    attrs = {
        "to_copy": attrs.source(),
    },
)

def _symlinked_dir_impl(ctx):
    another_to_symlink = ctx.actions.declare_output("another_to_symlink", has_content_based_path = True)
    ctx.actions.write(another_to_symlink, "another_to_symlink")
    out = ctx.actions.symlinked_dir(
        "out",
        {
            "another_to_symlink": another_to_symlink,
            "to_symlink": ctx.attrs.to_symlink,
        },
        has_content_based_path = True,
    )

    return [DefaultInfo(default_output = out)]

symlinked_dir = rule(
    impl = _symlinked_dir_impl,
    attrs = {
        "to_symlink": attrs.source(),
    },
)

def _cas_artifact_with_content_based_path_impl(ctx: AnalysisContext):
    out = ctx.actions.cas_artifact(
        "out",
        ctx.attrs.digest,
        ctx.attrs.use_case,
        expires_after_timestamp = ctx.attrs.expires_after_timestamp,
        is_tree = ctx.attrs.is_tree,
        is_directory = ctx.attrs.is_directory,
        has_content_based_path = True,
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

def _download_with_content_based_path_impl(ctx: AnalysisContext):
    url = "https://interncache-all.fbcdn.net/manifold/buck_build_test/tree/buck2_test/http_archive/test.tgz"

    if ctx.attrs.defer_download:
        sha1 = "1a45666759704bf08fc670aa96118a0415c470fc"
        dummy_sha_256 = None
    else:
        # sha256 is not actually supported for deferrable downloads, but we do need to provide either a sha1 or a sha256.
        # So, this causes us to fall into the "non-deferrable" code path, which is what we want.
        sha1 = None
        dummy_sha_256 = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

    download = ctx.actions.download_file("download", url, sha1 = sha1, sha256 = dummy_sha_256, has_content_based_path = True)
    return [
        DefaultInfo(default_output = download),
    ]

download_with_content_based_path = rule(
    impl = _download_with_content_based_path_impl,
    attrs = {
        "defer_download": attrs.bool(default = True),
    },
)

def _failing_validation_with_content_based_path_impl(ctx):
    validation = ctx.actions.declare_output("validation.json", has_content_based_path = True)
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
    input = ctx.actions.declare_output("input", has_content_based_path = True)
    input = ctx.actions.write(input, str("input"))
    output = ctx.actions.declare_output("out", has_content_based_path = True)

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
    input = ctx.actions.declare_output("input", has_content_based_path = True)
    input = ctx.actions.write(input, str("input"))
    output = ctx.actions.declare_output("out", has_content_based_path = True)

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

def _use_projection_with_content_based_path_impl(ctx):
    script = ctx.actions.declare_output("script.py", has_content_based_path = True)
    script = ctx.actions.write(
        script,
        [
            "import os",
            "import sys",
            "os.makedirs(sys.argv[1], exist_ok=True)",
            "with open(sys.argv[2], 'w') as f:",
            "  f.write('hello projection1')",
            "with open(sys.argv[3], 'w') as f:",
            "  f.write('hello projection2')",
        ],
    )

    out = ctx.actions.declare_output("out", has_content_based_path = True)
    projection1 = out.project("projection1.txt")
    projection2 = out.project("projection2.txt")
    args = cmd_args(["fbpython", script, out.as_output(), projection1.as_output(), projection2.as_output()])
    ctx.actions.run(args, category = "test_run", prefer_remote = True)

    copy_script = ctx.actions.write(
        "copy_script.py",
        [
            "import shutil",
            "import sys",
            "shutil.copyfile(sys.argv[1], sys.argv[2])",
        ],
        uses_experimental_content_based_path_hashing = True,
    )

    first_copy_projection1 = ctx.actions.declare_output("first_copied_projection1.txt", has_content_based_path = True)
    args = cmd_args(["fbpython", copy_script, projection1, first_copy_projection1.as_output()], hidden = [out])
    ctx.actions.run(args, category = "test_first_copy_projection1", prefer_local = True)

    second_copy_projection1 = ctx.actions.declare_output("second_copied_projection1.txt", has_content_based_path = True)
    args = cmd_args(["fbpython", copy_script, projection1, second_copy_projection1.as_output()], hidden = [first_copy_projection1])
    ctx.actions.run(args, category = "test_second_copy_projection1", prefer_local = True)

    return [DefaultInfo(default_output = second_copy_projection1)]

use_projection_with_content_based_path = rule(
    impl = _use_projection_with_content_based_path_impl,
    attrs = {
    },
)

def _ignores_content_based_artifact_impl(ctx):
    ignored = ctx.actions.declare_output("ignored.txt", has_content_based_path = True)
    ignored = ctx.actions.write(ignored, "ignored")

    out = ctx.actions.declare_output("out", has_content_based_path = True)
    args = cmd_args([out.as_output()], cmd_args(ignored, ignore_artifacts = True))

    ctx.actions.run(args, category = "test_run")

    return [DefaultInfo(default_output = out)]

ignores_content_based_artifact = rule(
    impl = _ignores_content_based_artifact_impl,
    attrs = {},
)

def _slow_running_local_action_with_content_based_path_impl(ctx):
    script = ctx.actions.declare_output("script.py", has_content_based_path = True)
    script = ctx.actions.write(
        script,
        [
            "import sys",
            "import time",
            "with open(sys.argv[1], 'w') as f:",
            "  f.write(sys.argv[2])",
            "time.sleep(2)",
        ],
    )

    out = ctx.actions.declare_output("out", has_content_based_path = True)
    args = cmd_args(["fbpython", script, out.as_output(), ctx.attrs.data])

    ctx.actions.run(args, category = "test_run", local_only = True)

    return [DefaultInfo(default_output = out)]

slow_running_local_action_with_content_based_path = rule(
    impl = _slow_running_local_action_with_content_based_path_impl,
    attrs = {
        "data": attrs.string(),
    },
)

def _writes_input_to_output_impl(ctx):
    script = ctx.actions.declare_output("script.py", has_content_based_path = True)
    script = ctx.actions.write(
        script,
        [
            "import sys",
            "with open(sys.argv[1], 'w') as output:",
            "  with open(sys.argv[2], 'r') as input:",
            "    output.write(input.read())",
        ],
    )

    out = ctx.actions.declare_output("out", has_content_based_path = True)
    args = cmd_args(["fbpython", script, out.as_output(), ctx.attrs.input])

    ctx.actions.run(args, category = "test_run", local_only = True)

    return [DefaultInfo(default_output = out)]

writes_input_to_output = rule(
    impl = _writes_input_to_output_impl,
    attrs = {
        "input": attrs.source(),
    },
)

def _uses_relative_to_impl(ctx):
    out1 = ctx.actions.declare_output("out1", has_content_based_path = True)
    ctx.actions.write(out1, "hello world")

    out2 = ctx.actions.declare_output("out2", has_content_based_path = True)
    ctx.actions.write(out2, "hello world")

    out3 = ctx.actions.declare_output("out3", has_content_based_path = True)
    ctx.actions.write(out3, cmd_args(out1, relative_to = out2))

    return [DefaultInfo(default_output = out3)]

uses_relative_to = rule(
    impl = _uses_relative_to_impl,
    attrs = {
    },
)

def _sets_inconsistent_params_impl(ctx):
    out = ctx.actions.declare_output("out", has_content_based_path = True)
    ctx.actions.write(out, "hello world", has_content_based_path = False)

    return [DefaultInfo(default_output = out)]

sets_inconsistent_params = rule(
    impl = _sets_inconsistent_params_impl,
    attrs = {},
)

def _argsfile_with_incorrectly_declared_output_impl(ctx):
    script = ctx.actions.declare_output("script.py", has_content_based_path = True)
    out = ctx.actions.declare_output("out", has_content_based_path = True)
    script = ctx.actions.write(
        script,
        [
            "import sys",
            cmd_args(out, format = "with open('{}', 'w') as f:"),
            "  f.write('blah')",
        ],
    )

    args = cmd_args(["fbpython", script], hidden = [out.as_output()])

    ctx.actions.run(args, category = "test_run")

    return [DefaultInfo(default_output = out), RunInfo(args = [out])]

argsfile_with_incorrectly_declared_output = rule(
    impl = _argsfile_with_incorrectly_declared_output_impl,
    attrs = {
    },
)

def _incremental_action_impl(ctx) -> list[Provider]:
    script = ctx.actions.write(
        "script.py",
        [
            "import sys",
            "with open(sys.argv[1], 'w') as f:",
            "  f.write('hello')",
        ],
        uses_experimental_content_based_path_hashing = True,
    )

    out = ctx.actions.declare_output("out", uses_experimental_content_based_path_hashing = True)
    args = cmd_args(["fbpython", script, out.as_output()])

    ctx.actions.run(
        args,
        category = "test_incremental_run",
        no_outputs_cleanup = True,
        metadata_env_var = "METADATA",
        metadata_path = "metadata.json",
    )

    return [DefaultInfo(out)]

incremental_action = rule(impl = _incremental_action_impl, attrs = {})

HelloInfo = provider(fields = ["hello"])

def _anon_impl(ctx: AnalysisContext) -> list[Provider]:
    hello = ctx.actions.write("hello.out", "hello", has_content_based_path = ctx.attrs.has_content_based_path)
    return [DefaultInfo(), HelloInfo(hello = hello)]

_anon = anon_rule(
    impl = _anon_impl,
    attrs = {
        "has_content_based_path": attrs.bool(),
    },
    artifact_promise_mappings = {
        "hello": lambda x: x[HelloInfo].hello,
    },
)

def _resolve_promise_artifact_impl(ctx: AnalysisContext) -> list[Provider]:
    anon = ctx.actions.anon_target(_anon, {"has_content_based_path": ctx.attrs.artifact_has_content_based_path})
    hello_artifact = anon.artifact("hello")

    if ctx.attrs.assert_promised_artifact_has_content_based_path:
        hello_artifact = ctx.actions.assert_has_content_based_path(hello_artifact)

    written = ctx.actions.write("hello.out", hello_artifact)

    return [DefaultInfo(default_output = written)]

resolve_promise_artifact = rule(impl = _resolve_promise_artifact_impl, attrs = {
    "artifact_has_content_based_path": attrs.bool(),
    "assert_promised_artifact_has_content_based_path": attrs.bool(),
})

def _not_eligible_for_dedupe_impl(ctx) -> list[Provider]:
    script = ctx.actions.write(
        "script.py",
        [
            "import sys",
            "with open(sys.argv[1], 'w') as f:",
            "  f.write('hello')",
        ],
        uses_experimental_content_based_path_hashing = False,
    )

    out = ctx.actions.declare_output("out", uses_experimental_content_based_path_hashing = ctx.attrs.run_action_output_has_content_based_path)
    args = cmd_args(["fbpython", script, out.as_output()])

    ctx.actions.run(
        args,
        category = "test_run",
        expect_eligible_for_dedupe = ctx.attrs.expect_eligible_for_dedupe,
    )

    return [DefaultInfo(out)]

not_eligible_for_dedupe = rule(impl = _not_eligible_for_dedupe_impl, attrs = {
    "expect_eligible_for_dedupe": attrs.bool(default = False),
    "run_action_output_has_content_based_path": attrs.bool(default = True),
})

def _failing_run_with_content_based_path_impl(ctx):
    script = ctx.actions.write(
        "script.py",
        [
            "import sys",
            "sys.exit(1)",
        ],
        has_content_based_path = True,
    )

    out = ctx.actions.declare_output("out", has_content_based_path = True)
    args = cmd_args(["fbpython", script, out.as_output()])

    ctx.actions.run(
        args,
        category = "test_run",
    )

    return [DefaultInfo(default_output = out), RunInfo(args = [out])]

failing_run_with_content_based_path = rule(
    impl = _failing_run_with_content_based_path_impl,
    attrs = {
    },
)
