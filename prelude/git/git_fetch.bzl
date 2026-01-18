# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

_HEX_DIGITS = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f"]

def _is_hex(rev: str, *, length: int) -> bool:
    if len(rev) != length:
        return False
    for digit in rev.elems():
        if digit not in _HEX_DIGITS:
            return False
    return True

def git_fetch_impl(ctx: AnalysisContext) -> list[Provider]:
    object_format = ctx.attrs.object_format
    rev = ctx.attrs.rev
    if object_format == None:
        if not _is_hex(rev, length = 40):
            fail("git_fetch's `rev` must be a 40-hex-digit commit hash: {}".format(rev))
    elif object_format == "sha1":
        if not _is_hex(rev, length = 40):
            fail("git_fetch's `rev` must be a 40-hex-digit commit hash when the chosen object format is sha1: {}".format(rev))
    elif object_format == "sha256":
        if not _is_hex(rev, length = 64):
            fail("git_fetch's `rev` must be a 64-hex-digit commit hash when the chosen object format is sha256: {}".format(rev))
    else:
        fail("Invalid git_fetch `object_format`: Must be one of sha1 or sha256: {}".format(object_format))

    git_dir = ctx.actions.declare_output(".git", dir = True)

    short_path = ctx.attrs.name.removesuffix(".git")
    if not short_path:
        short_path = "work-tree"
    work_tree = ctx.actions.declare_output(short_path, dir = True)

    cmd = [
        ctx.attrs._git_fetch_tool[RunInfo],
        cmd_args("--git-dir=", git_dir.as_output(), delimiter = ""),
        cmd_args("--work-tree=", work_tree.as_output(), delimiter = ""),
        cmd_args("--repo=", ctx.attrs.repo, delimiter = ""),
        cmd_args("--rev=", rev, delimiter = ""),
    ]
    if object_format != None:
        cmd.append(cmd_args("--object-format=", object_format, delimiter = ""))

    ctx.actions.run(
        cmd,
        category = "git_fetch",
        # local_only = True,
        allow_cache_upload = ctx.attrs.allow_cache_upload,
    )

    return [DefaultInfo(
        default_output = work_tree,
        sub_targets = {
            path: [DefaultInfo(default_output = work_tree.project(path))]
            for path in ctx.attrs.sub_targets
        },
    )]
