# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:artifacts.bzl", "ArtifactGroupInfo")

def filegroup_impl(ctx):
    """
    Creates a directory that contains links to the list of srcs

    The output is a directory that uses `out` for its name, if provided, or the rule name if not.
    Each symlink is based on the `short_path` for the provided `src`.
    """
    output_name = ctx.attrs.out if ctx.attrs.out else ctx.label.name

    if type(ctx.attrs.srcs) == type({}):
        srcs = ctx.attrs.srcs
    else:
        srcs = {}
        for src in ctx.attrs.srcs:
            existing = srcs.get(src.short_path)
            if existing != None and existing != src:
                soft_error(
                    "starlark_filegroup_duplicate_srcs",
                    "filegroup {} has srcs with duplicate names: {} and {}".format(ctx.label, src, srcs[src.short_path]),
                    quiet = True,
                    stack = False,
                )
            srcs[src.short_path] = src

    # It seems that buck1 always copies, and that's important for Python rules
    if ctx.attrs.copy:
        output = ctx.actions.copied_dir(output_name, srcs, executable_bit_override = ctx.attrs.executable_bit_override)
    elif ctx.attrs.executable_bit_override != None:
        fail("filegroup does not allow specifying `executable_bit_override` with `copy = False`")
    else:
        output = ctx.actions.symlinked_dir(output_name, srcs)

    if type(ctx.attrs.srcs) == type([]):
        artifacts = ctx.attrs.srcs
    else:
        artifacts = [output.project(name, hide_prefix = True) for name in srcs]

    return [
        DefaultInfo(default_output = output),
        ArtifactGroupInfo(artifacts = artifacts),
    ]
