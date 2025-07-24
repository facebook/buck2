# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:validation_deps.bzl", "get_validation_deps_outputs")
load(
    "@prelude//android:android_providers.bzl",
    "AndroidLibraryIntellijInfo",
    "AndroidResourceInfo",
    "merge_android_packageable_info",
    "merge_exported_android_resource_info",
)
load("@prelude//android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@prelude//android:r_dot_java.bzl", "get_dummy_r_dot_java")
load("@prelude//java:java_library.bzl", "build_java_library")
load(
    "@prelude//java:java_providers.bzl",
    "JavaClasspathEntry",  # @unused Used as type
    "JavaCompilingDepsTSet",
    "JavaLibraryInfo",
    "JavaProviders",  # @unused Used as type
    "create_native_providers",
    "single_library_compiling_deps",
    "to_list",
)
load("@prelude//java/utils:java_utils.bzl", "CustomJdkInfo")
load("@prelude//kotlin:kotlin_library.bzl", "build_kotlin_library")
load("@prelude//utils:expect.bzl", "expect")

def get_custom_jdk_info(ctx: AnalysisContext) -> CustomJdkInfo:
    bootclasspath_entries = [] + ctx.attrs._android_toolchain[AndroidToolchainInfo].android_bootclasspath + optional_jars(ctx)

    return CustomJdkInfo(
        bootclasspath = bootclasspath_entries,
        system_image = ctx.attrs._android_toolchain[AndroidToolchainInfo].jdk_system_image,
    )

def android_library_impl(ctx: AnalysisContext) -> list[Provider]:
    packaging_deps = ctx.attrs.deps + ctx.attrs.exported_deps + ctx.attrs.runtime_deps
    if ctx.attrs._build_only_native_code:
        shared_library_info, cxx_resource_info, linkable_graph = create_native_providers(ctx, ctx.label, packaging_deps)
        return [
            merge_android_packageable_info(ctx.label, ctx.actions, packaging_deps),
            shared_library_info,
            cxx_resource_info,
            linkable_graph,
            # Add an unused default output in case this target is used as an attr.source() anywhere.
            DefaultInfo(
                default_output = ctx.actions.write("{}/unused.jar".format(ctx.label.name), []),
                sub_targets = {
                    "generated_sources": [DefaultInfo(default_output = ctx.actions.write("{}/generated_sources".format(ctx.label.name), []))],
                },
            ),
            TemplatePlaceholderInfo(keyed_variables = {
                "classpath": "unused_but_needed_for_analysis",
            }),
        ]

    java_providers, android_library_intellij_info = build_android_library(
        ctx = ctx,
        validation_deps_outputs = get_validation_deps_outputs(ctx),
    )
    android_providers = [android_library_intellij_info] if android_library_intellij_info else []

    return to_list(java_providers) + [
        merge_android_packageable_info(
            ctx.label,
            ctx.actions,
            packaging_deps,
            manifest = ctx.attrs.manifest,
        ),
        merge_exported_android_resource_info(ctx.attrs.exported_deps),
    ] + android_providers

def optional_jars(ctx: AnalysisContext) -> list[Artifact]:
    if not ctx.attrs.android_optional_jars:
        return []

    result = []
    for dep in ctx.attrs.android_optional_jars:
        java_library_info = dep.get(JavaLibraryInfo)
        expect(java_library_info != None and java_library_info.library_output != None, "Only targets producing a Java bytecode output can be added as 'android_optional_jars'!")
        result.append(java_library_info.library_output.full_library)

    return result

def build_android_library(
        ctx: AnalysisContext,
        r_dot_java: JavaClasspathEntry | None = None,
        extra_sub_targets = {},
        validation_deps_outputs: [list[Artifact], None] = None,
        classpath_entries: JavaCompilingDepsTSet | None = None) -> (JavaProviders, [AndroidLibraryIntellijInfo, None]):
    custom_jdk_info = get_custom_jdk_info(ctx)
    additional_classpath_entries_children = [classpath_entries] if classpath_entries else []

    dummy_r_dot_java, android_library_intellij_info = _get_dummy_r_dot_java(ctx)
    extra_sub_targets = dict(extra_sub_targets)

    if r_dot_java:
        additional_classpath_entries_children.append(single_library_compiling_deps(ctx.actions, r_dot_java))
    elif dummy_r_dot_java:
        additional_classpath_entries_children.append(single_library_compiling_deps(ctx.actions, dummy_r_dot_java))
        extra_sub_targets["dummy_r_dot_java"] = [DefaultInfo(default_output = dummy_r_dot_java.full_library)]

    additional_classpath_entries = ctx.actions.tset(
        JavaCompilingDepsTSet,
        children = additional_classpath_entries_children,
    ) if additional_classpath_entries_children else None

    if ctx.attrs.language != None and ctx.attrs.language.lower() == "kotlin":
        return build_kotlin_library(
            ctx,
            additional_classpath_entries = additional_classpath_entries,
            custom_jdk_info = custom_jdk_info,
            extra_sub_targets = extra_sub_targets,
            validation_deps_outputs = validation_deps_outputs,
        ), android_library_intellij_info
    else:
        return build_java_library(
            ctx,
            ctx.attrs.srcs,
            additional_classpath_entries = additional_classpath_entries,
            custom_jdk_info = custom_jdk_info,
            extra_sub_targets = extra_sub_targets,
            validation_deps_outputs = validation_deps_outputs,
        ), android_library_intellij_info

def _get_dummy_r_dot_java(
        ctx: AnalysisContext) -> (JavaClasspathEntry | None, [AndroidLibraryIntellijInfo, None]):
    android_resources = dedupe([resource for resource in filter(None, [
        x.get(AndroidResourceInfo)
        for x in ctx.attrs.deps + ctx.attrs.provided_deps + (getattr(ctx.attrs, "provided_deps_query", []) or [])
    ]) if resource.res != None])
    if len(android_resources) == 0:
        return (None, None)

    dummy_r_dot_java_info = get_dummy_r_dot_java(
        ctx,
        ctx.attrs._android_toolchain[AndroidToolchainInfo].merge_android_resources[RunInfo],
        android_resources,
        ctx.attrs.resource_union_package,
    )

    dummy_r_dot_java = dummy_r_dot_java_info.library_output
    return (dummy_r_dot_java, AndroidLibraryIntellijInfo(
        dummy_r_dot_java = dummy_r_dot_java.abi,
        android_resource_deps = android_resources,
    ))
