def write_swift_module_map(
        ctx: "context",
        module_name: str.type,
        sdk_deps: ["SdkCompiledModuleInfo"]) -> "artifact":
    return write_swift_module_map_with_custom_deps(ctx, module_name, sdk_deps, [])

def write_swift_module_map_with_custom_deps(
        ctx: "context",
        module_name: str.type,
        sdk_swift_deps: ["SdkCompiledModuleInfo"],
        custom_swift_deps: ["SwiftDependencyInfo"]) -> "artifact":
    sdk_swift_deps_json = [{
        "isFramework": sdk_dep.is_framework,
        "moduleName": sdk_dep.module_name,
        "modulePath": sdk_dep.output_artifact,
    } for sdk_dep in sdk_swift_deps if sdk_dep.is_swiftmodule]

    custom_swift_deps_json = [{
        "isFramework": False,
        "moduleName": swift_dep.name,
        "modulePath": swift_dep.swiftmodule_path,
    } for swift_dep in custom_swift_deps if swift_dep.swiftmodule_path]

    return ctx.actions.write_json(
        module_name + ".swift_module_map.json",
        sdk_swift_deps_json + custom_swift_deps_json,
    )
