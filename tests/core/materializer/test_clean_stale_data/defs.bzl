# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _write_string_impl(ctx):
    out = ctx.actions.write(ctx.attrs.out, ctx.attrs.content)
    return [DefaultInfo(default_output = out)]

write_string = rule(
    impl = _write_string_impl,
    attrs = {
        "content": attrs.string(default = ""),
        "out": attrs.string(),
    },
)

def _copy_impl(ctx):
    out = ctx.actions.declare_output("action_output")
    ctx.actions.run(
        cmd_args(["cp", ctx.attrs.src, out.as_output()]),
        category = "cp",
    )
    return [DefaultInfo(default_output = out)]

copy = rule(
    impl = _copy_impl,
    attrs = {
        "dep": attrs.option(attrs.dep(), default = None),
        "src": attrs.source(),
    },
)

def _copy_to_dir_impl(ctx):
    out = ctx.actions.declare_output("action_output", dir = True)
    ctx.actions.run(
        cmd_args([
            "sh",
            "-c",
            'mkdir "$1" && cp "$2" "$1"/"$2"',
            "--",
            out.as_output(),
            ctx.attrs.src,
        ]),
        category = "cp_to_dir",
    )
    return [DefaultInfo(default_output = out)]

copy_to_dir = rule(
    impl = _copy_to_dir_impl,
    attrs = {
        "src": attrs.source(),
    },
)

def _download(ctx: AnalysisContext):
    url = "https://interncache-all.fbcdn.net/manifold/buck_build_test/tree/buck2_test/http_archive/test.tgz"
    sha1 = "1a45666759704bf08fc670aa96118a0415c470fc"
    download = ctx.actions.download_file("download", url, sha1 = sha1, is_deferrable = ctx.attrs.deferrable)
    return [
        DefaultInfo(default_output = download),
    ]

download = rule(
    impl = _download,
    attrs = {
        "deferrable": attrs.bool(),
    },
)

def _cas_artifact_impl(ctx: AnalysisContext):
    out = ctx.actions.cas_artifact(
        ctx.label.name,
        ctx.attrs.digest,
        ctx.attrs.use_case,
        expires_after_timestamp = ctx.attrs.expires_after_timestamp,
        is_tree = ctx.attrs.is_tree,
        is_directory = ctx.attrs.is_directory,
    )
    return [DefaultInfo(default_output = out)]

cas_artifact = rule(impl = _cas_artifact_impl, attrs = {
    "digest": attrs.string(),
    "expires_after_timestamp": attrs.int(default = 0),
    "is_directory": attrs.bool(default = False),
    "is_tree": attrs.bool(default = False),
    "use_case": attrs.string(default = "buck2-testing"),
})

def symlink_files_impl(ctx):
    srcs = {
        src.short_path: src
        for src in ctx.attrs.srcs
    }
    srcs.update({
        "subdir/{}.suffix".format(src.short_path): src
        for src in ctx.attrs.srcs
    })
    out = ctx.actions.symlinked_dir("out", srcs)
    return [DefaultInfo(default_output = out)]

symlink_files = rule(
    impl = symlink_files_impl,
    attrs = {
        "srcs": attrs.list(attrs.source()),
    },
)

def _write_json_impl(ctx: AnalysisContext) -> list[Provider]:
    out = ctx.actions.write_json("out.json", ctx.attrs.content)
    return [DefaultInfo(default_output = out)]

write_json = rule(
    impl = _write_json_impl,
    attrs = {
        "content": attrs.string(default = "text"),
    },
)
