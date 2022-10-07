load(":apple_sdk_module.bzl", "create_sdk_modules_graph")
load(":swift_toolchain_types.bzl", "SdkUncompiledModuleInfo", "SwiftToolchainInfo")

def swift_toolchain_impl(ctx):
    compiler = cmd_args(ctx.attrs._swiftc_wrapper[RunInfo]).add(ctx.attrs.swiftc[RunInfo])
    compiler_flags = ctx.attrs.swiftc_flags
    sdk_path = ctx.attrs._internal_sdk_path or ctx.attrs.sdk_path
    resource_dir = ctx.attrs.resource_dir

    toolchain_context = struct(
        compiler = compiler,
        sdk_path = sdk_path,
        compiler_flags = compiler_flags,
        swift_resource_dir = resource_dir,
    )

    compiled_sdk_module_providers = {}

    sdk_uncompiled_module_infos = filter(None, [d.get(SdkUncompiledModuleInfo) for d in ctx.attrs.sdk_modules])
    for uncompiled_swift_module_info in sdk_uncompiled_module_infos:
        create_sdk_modules_graph(
            ctx,
            compiled_sdk_module_providers,
            uncompiled_swift_module_info,
            toolchain_context,
        )

    compiled_sdk_swift_module_providers = {
        info.module_name: info
        for _, info in compiled_sdk_module_providers.items()
        if info.is_swiftmodule
    }

    compiled_sdk_clang_module_providers = {
        info.module_name: info
        for _, info in compiled_sdk_module_providers.items()
        if not info.is_swiftmodule
    }

    return [
        DefaultInfo(),
        SwiftToolchainInfo(
            architecture = ctx.attrs.architecture,
            # TODO(T99038725): until we add -debug-compilation-dir we need to wrap
            # the Swift invocations so that we can apply a debug prefix map for
            # the current directory while maintaining cache hit.
            compiled_sdk_clang_modules = compiled_sdk_clang_module_providers,
            compiled_sdk_swift_modules = compiled_sdk_swift_module_providers,
            compiler = compiler,
            compiler_flags = compiler_flags,
            prefix_serialized_debugging_options = ctx.attrs.prefix_serialized_debug_info,
            resource_dir = resource_dir,
            sdk_path = sdk_path,
            swift_stdlib_tool = ctx.attrs.swift_stdlib_tool[RunInfo],
            swift_stdlib_tool_flags = ctx.attrs.swift_stdlib_tool_flags,
        ),
    ]
