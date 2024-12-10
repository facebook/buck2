# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

LibInfo = provider(
    fields = {
        "lib": provider_field(Artifact),
    },
)

ResourceInfo = provider(
    fields = {
        "data": provider_field(Artifact),
    },
)

def _library(ctx):
    name = ctx.attrs.name

    # generate index data
    index_output = ctx.actions.write(name + ".index", name)

    deps_libs = []
    resources = []
    for dep in ctx.attrs.deps:
        if LibInfo in dep:
            deps_libs.append(dep[LibInfo].lib)
        if ResourceInfo in dep:
            deps_libs.append(dep[ResourceInfo].data)

    # build lib
    lib = ctx.actions.write(name + ".o", [name] + resources)
    if deps_libs:
        # link lib
        lib = ctx.actions.write(name + "_linked.o", [lib] + deps_libs)

    return [
        DefaultInfo(
            default_output = lib,
            sub_targets = {
                "index": [DefaultInfo(default_output = index_output)],
            },
        ),
        LibInfo(lib = lib),
    ]

library = rule(
    impl = _library,
    attrs = {
        "deps": attrs.list(attrs.dep(), default = []),
    },
)

def _resource(ctx):
    name = ctx.attrs.name
    txt = ctx.actions.write(name + ".txt", name)

    return [
        DefaultInfo(
            default_output = txt,
        ),
        ResourceInfo(
            data = txt,
        ),
    ]

resource = rule(
    impl = _resource,
    attrs = {},
)

def _binary(ctx):
    name = ctx.attrs.name

    index_output = ctx.actions.write(name + ".index", name)

    deps = []
    out = ctx.actions.write(name + ".o", name)

    for dep in ctx.attrs.deps:
        deps.extend(dep[DefaultInfo].default_outputs)
    bin = ctx.actions.write(name, [out] + deps)
    return [
        DefaultInfo(
            default_output = bin,
            sub_targets = {
                "index": [DefaultInfo(default_output = index_output)],
            },
        ),
    ]

binary = rule(
    impl = _binary,
    attrs = {
        "deps": attrs.list(attrs.dep(), default = []),
    },
)
