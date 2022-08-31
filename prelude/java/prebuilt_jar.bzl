load("@prelude//android:android_providers.bzl", "merge_android_packageable_info")
load(
    ":java_providers.bzl",
    "JavaClasspathEntry",
    "create_abi",
    "create_java_library_providers",
)
load(":java_toolchain.bzl", "PrebuiltJarToolchainInfo")

def prebuilt_jar_impl(ctx: "context") -> ["provider"]:
    """
     prebuilt_jar() rule implementation

    Args:
        ctx: rule analysis context
    Returns:
        list of created providers
    """

    expected_extension = ".jar"
    binary_jar = ctx.attrs.binary_jar
    extension = binary_jar.extension
    if extension != expected_extension:
        fail("Extension of the binary_jar attribute has to be equal to '{}' but '{}' has an extension '{}'".format(
            expected_extension,
            binary_jar,
            extension,
        ))

    output = ctx.actions.declare_output(binary_jar.basename[:-4] + "_symlink.jar")
    ctx.actions.symlink_file(output, binary_jar)

    abi = None
    if ctx.attrs.generate_abi:
        prebuilt_jar_toolchain = ctx.attrs._prebuilt_jar_toolchain[PrebuiltJarToolchainInfo]
        if not prebuilt_jar_toolchain.is_bootstrap_toolchain:
            abi = create_abi(ctx.actions, prebuilt_jar_toolchain.class_abi_generator, output)

    library_output_classpath_entry = JavaClasspathEntry(
        full_library = output,
        abi = abi or output,
        required_for_source_only_abi = ctx.attrs.required_for_source_only_abi,
    )

    java_library_info, java_packaging_info, shared_library_info, cxx_resource_info, template_placeholder_info, _ = create_java_library_providers(
        ctx,
        library_output = library_output_classpath_entry,
        declared_deps = ctx.attrs.deps,
        exported_deps = ctx.attrs.deps,
        needs_desugar = True,
        is_prebuilt_jar = True,
    )

    # TODO(T107163344) this shouldn't be in prebuilt_jar itself, use overlays to remove it.
    android_packageable_info = merge_android_packageable_info(ctx.label, ctx.actions, ctx.attrs.deps)

    sub_targets = {}
    sub_targets["abi"] = [
        java_library_info,
        template_placeholder_info,
        DefaultInfo(default_outputs = [library_output_classpath_entry.abi]),
    ]

    return [
        java_library_info,
        java_packaging_info,
        shared_library_info,
        cxx_resource_info,
        android_packageable_info,
        template_placeholder_info,
        DefaultInfo(default_outputs = [output], sub_targets = sub_targets),
    ]
