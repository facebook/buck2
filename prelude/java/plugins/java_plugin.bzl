# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//java:java_providers.bzl", "JavaPackagingDepTSet")
load(
    "@prelude//java/plugins:java_annotation_processor.bzl",
    "JavaProcessorsInfo",
    "JavaProcessorsType",
    "derive_transitive_deps",
)
load("@prelude//utils:type_defs.bzl", "is_tuple")

PluginParams = record(
    processors = field(list[(str, list[str])]),
    deps = field([JavaPackagingDepTSet, None]),
)

def create_plugin_params(ctx: AnalysisContext, plugins: list[[Dependency, (Dependency, list[str])]]) -> [PluginParams, None]:
    processors = []
    plugin_deps = []

    # Compiler plugin derived from `plugins` attribute
    for item in plugins:
        # Each plugin can be either a tuple of (target, arguments) or just the target
        if is_tuple(item):
            plugin = item[0]
            arguments = item[1]
        else:
            plugin = item
            arguments = None

        processors_info = plugin.get(JavaProcessorsInfo)
        if processors_info != None and processors_info.type == JavaProcessorsType("plugin"):
            if len(processors_info.processors) > 1:
                fail("Only 1 java compiler plugin is expected. But received: {}".format(processors_info.processors))
            processor = processors_info.processors[0]
            if processors_info.deps:
                plugin_deps.append(processors_info.deps)

            processors.append((processor, arguments or []))

    if not processors:
        return None

    return PluginParams(
        processors = processors,
        deps = ctx.actions.tset(JavaPackagingDepTSet, children = plugin_deps) if plugin_deps else None,
    )

def java_plugin_impl(ctx: AnalysisContext) -> list[Provider]:
    if ctx.attrs._build_only_native_code:
        return [DefaultInfo()]

    return [
        JavaProcessorsInfo(
            deps = derive_transitive_deps(ctx, ctx.attrs.deps),
            processors = [ctx.attrs.plugin_name],
            type = JavaProcessorsType("plugin"),
        ),
        DefaultInfo(default_output = None),
    ]
