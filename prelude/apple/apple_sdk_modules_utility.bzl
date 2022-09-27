def project_as_hidden(module_info: "SdkCompiledModuleInfo"):
    # NOTE(cjhopman): This would probably be better done by projecting as normal args and the caller putting it in hidden.
    args = cmd_args()
    args.hidden(module_info.output_artifact)
    return args

def project_as_clang_deps(module_info: "SdkCompiledModuleInfo"):
    if module_info.is_swiftmodule:
        return []
    else:
        return [
            "-Xcc",
            cmd_args(["-fmodule-file=", module_info.module_name, "=", module_info.output_artifact], delimiter = ""),
            "-Xcc",
            cmd_args(["-fmodule-map-file=", module_info.input_relative_path], delimiter = ""),
        ]

SDKDepTSet = transitive_set(args_projections = {
    "clang_deps": project_as_clang_deps,
    "hidden": project_as_hidden,
})

def _check_if_no_sdk_modules_are_provided(toolchain: "SwiftToolchainInfo") -> bool.type:
    no_swift_modules = toolchain.compiled_sdk_swift_modules == None or len(toolchain.compiled_sdk_swift_modules) == 0
    no_clang_modules = toolchain.compiled_sdk_clang_modules == None or len(toolchain.compiled_sdk_clang_modules) == 0
    if no_swift_modules and no_clang_modules:
        return True
    return False

def get_sdk_deps_tset(
        ctx: "context",
        module_name: str.type,
        required_modules: [str.type],
        toolchain: "SwiftToolchainInfo") -> "SDKDepTSet":
    if _check_if_no_sdk_modules_are_provided(toolchain):
        return ctx.actions.tset(SDKDepTSet)
    all_sdk_deps = []

    # Adding all direct and transitive SDK dependencies.
    for sdk_module_dep_name in ctx.attrs.sdk_modules + required_modules:
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
