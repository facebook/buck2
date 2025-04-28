# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def _http_archive_impl(ctx: AnalysisContext):
    download = ctx.actions.declare_output("download")
    ctx.actions.download_file(download, ctx.attrs.urls[0], sha1 = ctx.attrs.sha1, is_deferrable = True)

    output = ctx.actions.declare_output("output")
    ctx.actions.run(["cp", download, output.as_output()], category = "cp")

    return [
        DefaultInfo(default_output = output),
    ]

http_archive = rule(impl = _http_archive_impl, attrs = {
    "sha1": attrs.string(),
    "urls": attrs.list(attrs.string()),
})

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
    "expires_after_timestamp": attrs.int(),
    "is_directory": attrs.bool(default = False),
    "is_tree": attrs.bool(default = False),
    "use_case": attrs.string(),
})
