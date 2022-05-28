def project_as_hidden(args: "cmd_args", module_info: "SdkCompiledModuleInfo"):
    args.hidden(module_info.output_artifact)

def project_as_clang_deps(args: "cmd_args", module_info: "SdkCompiledModuleInfo"):
    if not module_info.is_swiftmodule:
        args.add([
            "-Xcc",
            cmd_args(["-fmodule-file=", module_info.module_name, "=", module_info.output_artifact], delimiter = ""),
            "-Xcc",
            cmd_args(["-fmodule-map-file=", module_info.input_relative_path], delimiter = ""),
        ])

SDKDepTSet = transitive_set(args_projections = {
    "clang_deps": project_as_clang_deps,
    "hidden": project_as_hidden,
})

_REQUIRED_SDK_MODULES = ["Swift", "SwiftOnoneSupport", "Darwin", "_Concurrency"]

def _check_if_no_sdk_modules_are_provided(toolchain: "SwiftToolchainInfo") -> bool.type:
    no_swift_modules = toolchain.compiled_sdk_swift_modules == None or len(toolchain.compiled_sdk_swift_modules) == 0
    no_clang_modules = toolchain.compiled_sdk_clang_modules == None or len(toolchain.compiled_sdk_clang_modules) == 0
    if no_swift_modules and no_clang_modules:
        return True
    return False

def get_sdk_deps_tset(
        ctx: "context",
        module_name: str.type,
        toolchain: "SwiftToolchainInfo") -> ["SDKDepTSet", None]:
    if _check_if_no_sdk_modules_are_provided(toolchain):
        return None
    all_sdk_deps = []

    # Adding all direct and transitive SDK dependencies.
    for sdk_module_dep_name in ctx.attr.sdk_modules + _REQUIRED_SDK_MODULES:
        if sdk_module_dep_name not in toolchain.compiled_sdk_swift_modules and sdk_module_dep_name not in toolchain.compiled_sdk_clang_modules:
            fail("{} depends on a non-existing SDK module: {}".format(module_name, sdk_module_dep_name))

        sdk_compiled_module_info = toolchain.compiled_sdk_swift_modules.get(sdk_module_dep_name) or toolchain.compiled_sdk_clang_modules.get(sdk_module_dep_name)
        sdk_module_with_transitive_deps_tset = ctx.actions.tset(
            SDKDepTSet,
            value = sdk_compiled_module_info,
            children = [sdk_compiled_module_info.deps],
        )
        all_sdk_deps.append(sdk_module_with_transitive_deps_tset)

    return ctx.actions.tset(SDKDepTSet, children = all_sdk_deps)
