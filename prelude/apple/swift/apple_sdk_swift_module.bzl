# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//apple:apple_utility.bzl", "get_disable_pch_validation_flags")
load(":swift_toolchain_types.bzl", "SdkSwiftOverlayInfo", "SdkTransitiveDepsTset", "SdkUncompiledModuleInfo")

def apple_sdk_swift_module_impl(ctx: AnalysisContext) -> list[Provider]:
    module_name = ctx.attrs.module_name

    cmd = cmd_args([
        "-frontend",
        "-compile-module-from-interface",
        "-disable-implicit-swift-modules",
        "-serialize-parseable-module-interface-dependency-hashes",
        "-disable-modules-validate-system-headers",
        "-suppress-warnings",
        "-module-name",
        module_name,
        "-target",
        ctx.attrs.target,
        "-Xcc",
        "-fno-implicit-modules",
        "-Xcc",
        "-fno-implicit-module-maps",
    ])
    cmd.add(get_disable_pch_validation_flags())

    if module_name == "Swift" or module_name == "SwiftOnoneSupport":
        cmd.add([
            "-parse-stdlib",
        ])

    overlays = []
    if ctx.attrs.overlays:
        overlays = [SdkSwiftOverlayInfo(overlays = ctx.attrs.overlays)]

    # Swift modules depend on both clang and swift transitive deps.
    clang_dep_children = []
    swift_dep_children = []
    for dep in ctx.attrs.deps:
        module_info = dep[SdkUncompiledModuleInfo]
        if module_info.is_swiftmodule:
            clang_dep_children.append(module_info.transitive_clang_deps)
            swift_dep_children.append(
                ctx.actions.tset(SdkTransitiveDepsTset, value = dep, children = [module_info.transitive_swift_deps]),
            )
        else:
            clang_dep_children.append(
                ctx.actions.tset(SdkTransitiveDepsTset, value = dep, children = [module_info.transitive_clang_deps]),
            )

    module_info = SdkUncompiledModuleInfo(
        name = ctx.attrs.name,
        module_name = ctx.attrs.module_name,
        is_framework = ctx.attrs.is_framework,
        is_swiftmodule = True,
        partial_cmd = cmd,
        input_relative_path = ctx.attrs.swiftinterface_relative_path,
        deps = ctx.attrs.deps,
        transitive_clang_deps = ctx.actions.tset(SdkTransitiveDepsTset, children = clang_dep_children),
        transitive_swift_deps = ctx.actions.tset(SdkTransitiveDepsTset, children = swift_dep_children),
    )

    return [
        DefaultInfo(),
        module_info,
    ] + overlays

# This rule represent a Swift module from SDK and forms a graph of dependencies between such modules.
apple_sdk_swift_module = rule(
    impl = apple_sdk_swift_module_impl,
    attrs = {
        "deps": attrs.list(attrs.dep(), default = []),
        "is_framework": attrs.bool(default = False),
        # This is a real module name, contrary to `name`
        # which has a special suffix to distinguish Swift and Clang modules with the same name
        "module_name": attrs.string(),
        "overlays": attrs.dict(key = attrs.string(), value = attrs.list(attrs.string(), default = []), sorted = False, default = {}),
        # A prefixed path ($SDKROOT/$PLATFORM_DIR) to swiftinterface textual file.
        "swiftinterface_relative_path": attrs.option(attrs.string(), default = None),  # if `swiftinterface` is None represents a Root node.
        "target": attrs.string(),
    },
)
