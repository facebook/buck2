load("@fbcode//buck2/prelude/apple:swift_toolchain_types.bzl", "SwiftToolchainInfo")

def swift_toolchain_impl(ctx):
    return [
        DefaultInfo(),
        SwiftToolchainInfo(
            architecture = ctx.attr.architecture,
            # TODO(T99038725): until we add -debug-compilation-dir we need to wrap
            # the Swift invocations so that we can apply a debug prefix map for
            # the current directory while maintaining cache hit.
            compiler = cmd_args(ctx.attr._swiftc_wrapper[RunInfo]).add(ctx.attr.swiftc[RunInfo]),
            compiler_flags = ctx.attr.swiftc_flags,
            swift_stdlib_tool = ctx.attr.swift_stdlib_tool[RunInfo],
            swift_stdlib_tool_flags = ctx.attr.swift_stdlib_tool_flags,
            sdk_path = ctx.attr._internal_sdk_path or ctx.attr.sdk_path,
            resource_dir = ctx.attr.resource_dir,
        ),
    ]
