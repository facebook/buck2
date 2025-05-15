# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load(
    ":erlang_toolchain.bzl",
    "Toolchain",  # @unused Used as type
)

def app_name(ctx: AnalysisContext) -> str:
    if ctx.attrs.app_name == None:
        return ctx.attrs.name
    else:
        return ctx.attrs.app_name

# `build_environments` is a `dict[str, BuildEnvironment]`.
def multidict_projection_key(build_environments: dict[str, typing.Any], field_name: str, key: str) -> dict:
    field = {}
    for name, env in build_environments.items():
        dict_val = getattr(env, field_name)
        field[name] = dict_val[key]
    return field

def action_identifier(toolchain: Toolchain, name: str) -> str:
    """builds an action identifier parameterized by the toolchain"""
    return "%s(%s)" % (name, toolchain.name)

def preserve_structure(path: str) -> dict[str, list[str]]:
    """Return a mapping from a path that preserves the filestructure relative to the path."""
    all_files = glob([paths.join(path, "**")])
    mapping = {}
    for filename in all_files:
        relative_path = paths.relativize(filename, path)
        dirname = paths.dirname(relative_path)
        mapping[dirname] = mapping.get(dirname, []) + [filename]
    return mapping

def _file_mapping_impl(ctx: AnalysisContext) -> list[Provider]:
    outputs = []
    for target_path, files in ctx.attrs.mapping.items():
        for file in files:
            target_path = paths.normalize(target_path)
            out_path = paths.normalize(paths.join(target_path, file.basename))
            out = ctx.actions.symlink_file(
                out_path,
                file,
            )
            outputs.append(out)

    return [DefaultInfo(default_outputs = outputs)]

file_mapping = rule(
    impl = _file_mapping_impl,
    attrs = {
        "mapping": attrs.dict(key = attrs.string(), value = attrs.list(attrs.source()), default = {}),
    },
)
