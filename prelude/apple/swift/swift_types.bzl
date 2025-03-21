# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

SWIFT_EXTENSION = ".swift"

SWIFTMODULE_EXTENSION = ".swiftmodule"

SwiftCompilationModes = ["wmo", "incremental", "auto"]

SwiftMacroPlugin = plugins.kind()

SwiftVersion = ["5", "6"]

def _swift_framework_implicit_search_paths_args(args: cmd_args):
    return args

FrameworkImplicitSearchPathInfoTSet = transitive_set(
    args_projections = {
        "swift_framework_implicit_search_paths_args": _swift_framework_implicit_search_paths_args,
    },
)

FrameworkImplicitSearchPathInfo = provider(fields = {
    "tset": provider_field(typing.Any, default = None),  # A tset of FrameworkImplicitSearchPathInfoTSet
})

def get_implicit_framework_search_path_providers(ctx: AnalysisContext, value: [cmd_args, None], deps: list[Dependency]) -> FrameworkImplicitSearchPathInfoTSet:
    deps_infos = [
        d[FrameworkImplicitSearchPathInfo].tset
        for d in deps
        if FrameworkImplicitSearchPathInfo in d and d[FrameworkImplicitSearchPathInfo].tset != None
    ]
    if value:
        return ctx.actions.tset(FrameworkImplicitSearchPathInfoTSet, value = value, children = deps_infos)
    else:
        return ctx.actions.tset(FrameworkImplicitSearchPathInfoTSet, children = deps_infos)
