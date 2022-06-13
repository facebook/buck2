load(
    ":java_providers.bzl",
    "JavaClasspathEntry",
    "create_java_library_providers",
    "maybe_create_abi",
)
load(":java_toolchain.bzl", "JavaToolchainInfo")

def prebuilt_jar_impl(ctx: "context") -> ["provider"]:
    """
     prebuilt_jar() rule implementation

    Args:
        ctx: rule analysis context
    Returns:
        list of created providers
    """

    expected_extension = ".jar"
    binary_jar = ctx.attr.binary_jar
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
    if ctx.attr.generate_abi:
        abi = maybe_create_abi(ctx.actions, ctx.attr._java_toolchain[JavaToolchainInfo], output)

    library_output_classpath_entry = JavaClasspathEntry(
        full_library = output,
        abi = abi or output,
        required_for_source_only_abi = ctx.attr.required_for_source_only_abi,
    )

    java_library_info, java_packaging_info, shared_library_info, cxx_resource_info, template_placeholder_info = create_java_library_providers(
        ctx,
        library_output = library_output_classpath_entry,
        declared_deps = ctx.attr.deps,
        exported_deps = ctx.attr.deps,
        needs_desugar = True,
        is_prebuilt_jar = True,
    )

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
        template_placeholder_info,
        DefaultInfo(default_outputs = [output], sub_targets = sub_targets),
    ]
