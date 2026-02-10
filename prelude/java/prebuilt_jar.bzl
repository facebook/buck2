# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//android:android_providers.bzl", "merge_android_packageable_info")
load("@prelude//java/utils:java_utils.bzl", "get_classpath_subtargets")
load(
    ":java_providers.bzl",
    "ClasspathSnapshotGranularity",
    "JavaClasspathEntry",
    "create_abi",
    "create_java_library_providers",
    "generate_java_classpath_snapshot",
)
load(":java_toolchain.bzl", "PrebuiltJarToolchainInfo")

def prebuilt_jar_impl(ctx: AnalysisContext) -> list[Provider]:
    """
     prebuilt_jar() rule implementation

    Args:
        ctx: rule analysis context
    Returns:
        list of created providers
    """

    expected_extensions = [".jar", ".jmod"]
    binary_jar = ctx.attrs.binary_jar
    extension = binary_jar.extension
    if extension not in expected_extensions:
        fail("Extension of the binary_jar attribute has to be one of '{}' but '{}' has an extension '{}'".format(
            expected_extensions,
            binary_jar,
            extension,
        ))

    output = ctx.actions.declare_output("symlink/{}".format(binary_jar.short_path), has_content_based_path = ctx.attrs.uses_content_based_paths)
    ctx.actions.symlink_file(output, binary_jar)

    gwt_output = ctx.actions.declare_output("{}-gwt.jar".format(ctx.label.name))
    ctx.actions.copy_file(gwt_output, ctx.attrs.source_jar if ctx.attrs.source_jar else ctx.attrs.binary_jar)

    abi = None
    prebuilt_jar_toolchain = ctx.attrs._prebuilt_jar_toolchain[PrebuiltJarToolchainInfo]
    if not prebuilt_jar_toolchain.is_bootstrap_toolchain:
        if ctx.attrs.generate_abi:
            abi = create_abi(ctx.actions, prebuilt_jar_toolchain.class_abi_generator, output)
    jar_snapshot = generate_java_classpath_snapshot(
        ctx.actions,
        ctx.attrs._prebuilt_jar_toolchain[PrebuiltJarToolchainInfo].cp_snapshot_generator,
        ClasspathSnapshotGranularity("CLASS_LEVEL"),
        abi or output,
        "",
    )

    library_output_classpath_entry = JavaClasspathEntry(
        full_library = output,
        abi = abi or output,
        abi_as_dir = None,
        required_for_source_only_abi = ctx.attrs.required_for_source_only_abi,
        abi_jar_snapshot = jar_snapshot,
    )

    java_library_info, java_packaging_info, global_code_info, shared_library_info, cxx_resource_info, linkable_graph, template_placeholder_info, _ = create_java_library_providers(
        ctx,
        library_output = library_output_classpath_entry,
        global_code_config = prebuilt_jar_toolchain.global_code_config,
        declared_deps = ctx.attrs.deps,
        exported_deps = ctx.attrs.deps,
        provided_deps = ctx.attrs.desugar_deps,
        needs_desugar = True,
        is_prebuilt_jar = True,
        gwt_module = gwt_output,
        sources_jar = ctx.attrs.source_jar,
    )

    # TODO(T107163344) this shouldn't be in prebuilt_jar itself, use overlays to remove it.
    android_packageable_info = merge_android_packageable_info(ctx.label, ctx.actions, ctx.attrs.deps)

    sub_targets = get_classpath_subtargets(ctx.actions, java_packaging_info)
    sub_targets["abi"] = [
        java_library_info,
        template_placeholder_info,
        DefaultInfo(default_output = library_output_classpath_entry.abi),
    ]

    return [
        java_library_info,
        java_packaging_info,
        global_code_info,
        shared_library_info,
        cxx_resource_info,
        android_packageable_info,
        template_placeholder_info,
        linkable_graph,
        DefaultInfo(default_output = output, sub_targets = sub_targets),
    ] + (
        [
            RunInfo(args = cmd_args([ctx.attrs._prebuilt_jar_toolchain[PrebuiltJarToolchainInfo].java[RunInfo], "-jar", output])),
        ] if ctx.attrs.is_executable else []
    )
