load("@fbcode//buck2/prelude/apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@fbcode//buck2/prelude/apple:swift_toolchain_types.bzl", "SwiftToolchainInfo")
load("@fbcode//buck2/prelude/cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo", "CxxToolchainInfo")

def apple_toolchain_impl(ctx: "context") -> ["provider"]:
    sdk_path = ctx.attr._internal_sdk_path or ctx.attr.sdk_path
    platform_path = ctx.attr._internal_platform_path or ctx.attr.platform_path
    return [
        DefaultInfo(),
        AppleToolchainInfo(
            actool = ctx.attr.actool[RunInfo],
            ibtool = ctx.attr.ibtool[RunInfo],
            dsymutil = ctx.attr.dsymutil[RunInfo],
            dwarfdump = ctx.attr.dwarfdump[RunInfo] if ctx.attr.dwarfdump else None,
            lipo = ctx.attr.lipo[RunInfo],
            cxx_platform_info = ctx.attr.cxx_toolchain[CxxPlatformInfo],
            cxx_toolchain_info = ctx.attr.cxx_toolchain[CxxToolchainInfo],
            codesign = ctx.attr.codesign[RunInfo],
            codesign_allocate = ctx.attr.codesign_allocate[RunInfo],
            compile_resources_locally = ctx.attr.compile_resources_locally,
            libtool = ctx.attr.libtool[RunInfo],
            momc = ctx.attr.momc[RunInfo],
            min_version = ctx.attr.min_version,
            xctest = ctx.attr.xctest[RunInfo],
            platform_path = platform_path,
            sdk_name = ctx.attr.sdk_name,
            sdk_path = sdk_path,
            sdk_version = ctx.attr.version,
            sdk_build_version = ctx.attr.build_version,
            swift_toolchain_info = ctx.attr.swift_toolchain[SwiftToolchainInfo] if ctx.attr.swift_toolchain else None,
            watch_kit_stub_binary = ctx.attr.watch_kit_stub_binary,
            xcode_version = ctx.attr.xcode_version,
            xcode_build_version = ctx.attr.xcode_build_version,
        ),
    ]
