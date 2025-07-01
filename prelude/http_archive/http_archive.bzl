# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//utils:expect.bzl", "expect")
load("@prelude//utils:materialization_test.bzl", "materialization_test")
load("@prelude//utils:utils.bzl", "value_or")
load(":exec_deps.bzl", "HttpArchiveExecDeps")
load(":unarchive.bzl", "archive_type", "unarchive")

def http_archive_impl(ctx: AnalysisContext) -> list[Provider]:
    expect(len(ctx.attrs.urls) == 1, "multiple `urls` not supported: {}".format(ctx.attrs.urls))
    expect(len(ctx.attrs.vpnless_urls) < 2, "multiple `vpnless_urls` not supported: {}".format(ctx.attrs.vpnless_urls))

    # The HTTP download is local so it makes little sense to run actions
    # remotely, unless we can defer them. We'll be able to defer if we provide
    # a digest that the daemon's digest config natively supports.
    digest_config = ctx.actions.digest_config()
    prefer_local = True
    if ctx.attrs.sha1 != None and digest_config.allows_sha1():
        prefer_local = False
    elif ctx.attrs.sha256 != None and digest_config.allows_sha256():
        prefer_local = False

    ext_type = archive_type(ctx.attrs.urls[0], ctx.attrs.type)

    # Download archive.
    archive = ctx.actions.declare_output("archive." + ext_type)
    url = ctx.attrs.urls[0]
    vpnless_url = None if len(ctx.attrs.vpnless_urls) == 0 else ctx.attrs.vpnless_urls[0]
    ctx.actions.download_file(
        archive.as_output(),
        url,
        vpnless_url = vpnless_url,
        sha1 = ctx.attrs.sha1,
        sha256 = ctx.attrs.sha256,
        size_bytes = ctx.attrs.size_bytes,
    )

    output, sub_targets = unarchive(
        ctx,
        archive = archive,
        output_name = value_or(ctx.attrs.out, ctx.label.name),
        ext_type = ext_type,
        excludes = ctx.attrs.excludes,
        strip_prefix = ctx.attrs.strip_prefix,
        sub_targets = ctx.attrs.sub_targets,
        exec_deps = ctx.attrs.exec_deps[HttpArchiveExecDeps],
        prefer_local = prefer_local,
    )

    return [
        DefaultInfo(
            default_output = output,
            sub_targets = sub_targets,
        ),
        materialization_test([archive, output]),
    ]
