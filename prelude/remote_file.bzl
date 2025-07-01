# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:http_file.bzl", "http_file_shared")
load("@prelude//utils:expect.bzl", "expect")
load("@prelude//utils:utils.bzl", "value_or")

_DEFAULT_MAVEN_REPO = read_config("http", "maven_repo", "https://repo1.maven.org/maven2")

def _from_mvn_url(url: str) -> str:
    """
    Convert `mvn:` style URIs to a URL.
    """

    count = url.count(":")
    mod = ""

    if count == 4:
        mvn, group, id, typ, version = url.split(":")
        repo = _DEFAULT_MAVEN_REPO
    elif count == 5:
        mvn, group, id, typ, mod, version = url.split(":")
        mod = "-" + mod
        repo = _DEFAULT_MAVEN_REPO
    elif count == 6:
        mvn, repo_protocol, repo_host, group, id, typ, version = url.split(":")
        repo = repo_protocol + ":" + repo_host
    elif count == 7:
        mvn, repo_protocol, repo_host, group, id, typ, mod, version = url.split(":")
        repo = repo_protocol + ":" + repo_host
    else:
        fail("Unsupported mvn URL scheme: " + url + " (" + str(count) + ")")

    expect(mvn == "mvn")

    group = group.replace(".", "/")

    if typ == "src":
        ext = "-sources.jar"
    else:
        ext = "." + typ

    return "{repo}/{group}/{id}/{version}/{id}-{version}{mod}{ext}".format(
        repo = repo,
        group = group,
        id = id,
        version = version,
        ext = ext,
        mod = mod,
    )

# Implementation of the `remote_file` build rule.
def remote_file_impl(ctx: AnalysisContext) -> list[Provider]:
    url = ctx.attrs.url
    if url.startswith("mvn:"):
        url = _from_mvn_url(url)
    return http_file_shared(
        ctx.actions,
        name = value_or(ctx.attrs.out, ctx.label.name),
        url = url,
        vpnless_url = ctx.attrs.vpnless_url,
        is_executable = ctx.attrs.type == "executable",
        is_exploded_zip = ctx.attrs.type == "exploded_zip",
        unzip_tool = ctx.attrs._unzip_tool[RunInfo],
        sha1 = ctx.attrs.sha1,
        sha256 = ctx.attrs.sha256,
        size_bytes = ctx.attrs.size_bytes,
    )
