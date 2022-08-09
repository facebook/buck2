load("@fbcode//buck2/prelude/android:android_providers.bzl", "AndroidPackageableInfo", "merge_android_packageable_info")
load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@fbcode//buck2/prelude/utils:utils.bzl", "flatten")

# In order to calculate which targets belong to each module, we reconstruct a "target graph"
# from "deps" information that is propagated up through AndroidPackageableInfo.
# In buck1 we use the underlying "TargetGraph" object that is based on the raw target
# definitions. This results in some slightly different behavior for `provided_deps` - in
# buck2, we (correctly) ignore `provided_deps`, since they do not influence the packaging of
# the APK, whereas in `buck1`, we treat `provided_deps` the same as `deps`.
# In practice, this rarely affects the module assignments, but can mean that `buck2` will
# put a target inside a module whereas `buck1` will put it into the main APK (since `buck1`
# can find a path from an "always in main APK seed" to the target via some `provided_dep`,
# whereas `buck2` does not).
def android_app_modularity_impl(ctx: "context") -> ["provider"]:
    android_packageable_info = merge_android_packageable_info(ctx.label, ctx.actions, ctx.attrs.deps)

    deps_infos = list(android_packageable_info.deps.traverse()) if android_packageable_info.deps else []
    deps_map = {deps_info.name: deps_info.deps for deps_info in deps_infos}

    target_graph_file = ctx.actions.write_json("target_graph.json", deps_map)
    application_module_configs_map = {module_name: [seed.label.raw_target() for seed in seeds if seed[AndroidPackageableInfo]] for module_name, seeds in ctx.attrs.application_module_configs.items()}
    application_module_configs_file = ctx.actions.write_json("application_module_configs.json", application_module_configs_map)
    application_module_dependencies_file = ctx.actions.write_json("application_module_dependencies.json", ctx.attrs.application_module_dependencies or {})
    output = ctx.actions.declare_output("apk_module_metadata.txt")

    android_toolchain = ctx.attrs._android_toolchain[AndroidToolchainInfo]
    cmd = cmd_args([
        android_toolchain.apk_module_graph[RunInfo],
        "--root-target",
        str(ctx.label.raw_target()),
        "--target-graph",
        target_graph_file,
        "--seed-config-map",
        application_module_configs_file,
        "--app-module-dependencies-map",
        application_module_dependencies_file,
        "--output",
        output.as_output(),
    ])

    if ctx.attrs.application_module_blacklist:
        all_blocklisted_deps = flatten(ctx.attrs.application_module_blacklist)

        application_module_blocklist_file = ctx.actions.write(
            "application_module_blocklist.txt",
            [str(blocklisted_dep.label.raw_target()) for blocklisted_dep in all_blocklisted_deps if blocklisted_dep[AndroidPackageableInfo]],
        )
        cmd.add([
            "--always-in-main-apk-seeds",
            application_module_blocklist_file,
        ])

    ctx.actions.run(cmd, category = "apk_module_graph")

    return [DefaultInfo(default_outputs = [output])]
