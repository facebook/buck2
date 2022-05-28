load(":swift_pcm_compilation.bzl", "get_shared_pcm_compilation_args")
load(":swift_toolchain_types.bzl", "SdkUncompiledModuleInfo")

def apple_sdk_clang_module_impl(ctx: "context") -> ["provider"]:
    cmd = get_shared_pcm_compilation_args(ctx.attr.target, ctx.attr.module_name)
    module_dependency_infos = filter(None, [d[SdkUncompiledModuleInfo] for d in ctx.attr.deps])
    return [
        DefaultInfo(),
        SdkUncompiledModuleInfo(
            name = ctx.attr.name,
            module_name = ctx.attr.module_name,
            is_framework = ctx.attr.is_framework,
            is_swiftmodule = False,
            partial_cmd = cmd,
            input_relative_path = ctx.attr.modulemap_relative_path,
            deps = module_dependency_infos,
        ),
    ]

# This rule represent a Clang module from SDK and forms a graph of dependencies between such modules.
apple_sdk_clang_module = rule(
    implementation = apple_sdk_clang_module_impl,
    attrs = {
        "deps": attr.list(attr.dep(), default = []),
        "is_framework": attr.bool(),
        # This is a real module name, contrary to `name`
        # which has a special suffix to distinguish Swift and Clang modules with the same name
        "module_name": attr.string(),
        "modulemap_relative_path": attr.string(),
        "target": attr.string(),
    },
)
