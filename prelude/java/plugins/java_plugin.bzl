# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//java:java_providers.bzl", "JavaPackagingDepTSet")
load(
    "@prelude//java/plugins:java_annotation_processor.bzl",
    "JavaProcessorsInfo",
    "JavaProcessorsType",
    "derive_transitive_deps",
)

PluginParams = record(
    processors = field(list[(str, cmd_args)]),
    deps = field([JavaPackagingDepTSet, None]),
)

def create_plugin_params(ctx: AnalysisContext, plugins: list[Dependency]) -> [PluginParams, None]:
    processors = []
    plugin_deps = []

    # _wip_java_plugin_arguments keys are providers_label, map to
    # target_label to allow lookup with plugin.label.raw_target()
    plugin_arguments = {
        label.raw_target(): arguments
        for label, arguments in ctx.attrs._wip_java_plugin_arguments.items()
    }

    # Compiler plugin derived from `plugins` attribute
    for plugin in plugins:
        processors_info = plugin.get(JavaProcessorsInfo)
        if processors_info != None and processors_info.type == JavaProcessorsType("plugin"):
            if len(processors_info.processors) > 1:
                fail("Only 1 java compiler plugin is expected. But received: {}".format(processors_info.processors))
            processor = processors_info.processors[0]
            if processors_info.deps:
                plugin_deps.append(processors_info.deps)

            arguments = plugin_arguments.get(plugin.label.raw_target())
            processors.append((processor, cmd_args(arguments) if arguments != None else cmd_args()))

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
