load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo")
load("@prelude//apple/swift:swift_toolchain_types.bzl", "SwiftToolchainInfo")

_APPLE_SDKS = [
    "appletvos",
    "appletvsimulator",
    "iphoneos",
    "iphonesimulator",
    "maccatalyst",
    "macosx",
    "visionos",
    "visionsimulator",
    "watchos",
    "watchsimulator"
]

def _system_apple_toolschain_impl(ctx: AnalysisContext) -> list[Provider]:
  xcode_bin_path =  "{}/Contents/Developer/usr/bin".format(ctx.attrs.xcode_app)
  sdk_path =  "{}/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk".format(ctx.attrs.xcode_app)

  return [
        DefaultInfo(),
        AppleToolchainInfo(
            actool = RunInfo(args = ["actool"]),
            architecture = ctx.attrs._architecture,
            codesign = RunInfo(args = ["codesign"]),
            codesign_allocate = RunInfo(args = ["codesign_allocate"]),
            codesign_identities_command = None,
            compile_resources_locally = True,
            copy_scene_kit_assets = RunInfo(args = ["{}/copySceneKitAssets".format(xcode_bin_path)]),
            dsymutil = RunInfo(args = ["dsymutil"]),
            dwarfdump = RunInfo(args = ["dwarfdump"]),
            extra_linker_outputs = ctx.attrs.extra_linker_outputs,
            ibtool = RunInfo(args = ["ibtool"]),
            libtool = RunInfo(args = ["libtool"]),
            lipo = RunInfo(args = ["lipo"]),
            objdump = RunInfo(args = ["objdump"]),
            installer = ctx.attrs.installer,
            momc = RunInfo(args = ["{}/momc".format(xcode_bin_path)]),
            platform_path = "/",
            sdk_name = ctx.attrs.sdk_name,
            sdk_path = sdk_path,
            xctest = RunInfo(args = ["{}/xctest".format(xcode_bin_path)]),
            swift_toolchain_info = SwiftToolchainInfo(
              sdk_path = sdk_path,
              compiler_flags = []
            )
        ),
        CxxPlatformInfo(name = "{}-{}".format(ctx.attrs.sdk_name, ctx.attrs._architecture)),
    ]

_DEFAULT_ARCH = select({
    "config//cpu:arm64": "arm64",
    "config//cpu:x86_64": "x86_64",
})


system_apple_toolchain = rule(
    impl = _system_apple_toolschain_impl,
    attrs = {
        "xcode_app": attrs.string(default = "/Applications/Xcode.app"),
        "extra_linker_outputs": attrs.list(attrs.string(), default = []),
        "sdk_name": attrs.enum(_APPLE_SDKS, default = "macosx"),
        "installer": attrs.default_only(attrs.label(default = "buck//src/com/facebook/buck/installer/apple:apple_installer")),
        "_architecture": attrs.string(default = _DEFAULT_ARCH),

    },
    is_toolchain_rule = True,
)


