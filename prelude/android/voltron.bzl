load("@prelude//android:android_providers.bzl", "AndroidPackageableInfo", "merge_android_packageable_info")
load("@prelude//android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@prelude//java:java_providers.bzl", "get_all_java_packaging_deps")
load("@prelude//linking:shared_libraries.bzl", "SharedLibraryInfo", "merge_shared_libraries", "traverse_shared_library_info")
load("@prelude//utils:utils.bzl", "expect", "filter_and_map_idx", "flatten")

# "Voltron" gives us the ability to split our Android APKs into different "modules". These
# modules can then be downloaded on demand rather than shipped with the "main" APK.
#
# The module corresponding to the "main" APK is called the "root" module.
#
# Voltron support comes in two main parts:
# (1) Constructing the Voltron module graph (assigning targets to each module). This is done
# by constructing a "target graph" and then delegating to buck1 to produce the module graph.
# (2) Using the Voltron module graph while building our APK.
#
# For (1), in order to calculate which targets belong to each module, we reconstruct a "target
# graph" from "deps" information that is propagated up through AndroidPackageableInfo.
# In buck1 we use the underlying "TargetGraph" object that is based on the raw target
# definitions. This results in some slightly different behavior for `provided_deps` - in
# buck2, we (correctly) ignore `provided_deps`, since they do not influence the packaging of
# the APK, whereas in `buck1`, we treat `provided_deps` the same as `deps`.
# In practice, this rarely affects the module assignments, but can mean that `buck2` will
# put a target inside a module whereas `buck1` will put it into the main APK (since `buck1`
# can find a path from an "always in main APK seed" to the target via some `provided_dep`,
# whereas `buck2` does not).
#
# For (2), we package up secondary DEX files and native libs into `assets/module_name` (see
# dex_rules.bzl and android_binary_native_rules.bzl for more information on how we do that).
# It is worth noting that we still put all of the non-root modules into the final APK. If
# the module should be downloaded on demand, then it is removed from the final APK in a
# subsequent post-processing step.
#
# There is also an `android_app_modularity` rule that just prints out details of the Voltron
# module graph and is used for any subsequent verification.
def android_app_modularity_impl(ctx: "context") -> ["provider"]:
    all_deps = ctx.attrs.deps + flatten(ctx.attrs.application_module_configs.values())
    android_packageable_info = merge_android_packageable_info(ctx.label, ctx.actions, all_deps)

    shared_library_info = merge_shared_libraries(
        ctx.actions,
        deps = filter_and_map_idx(SharedLibraryInfo, all_deps),
    )
    traversed_shared_library_info = traverse_shared_library_info(shared_library_info)

    cmd, output = _get_base_cmd_and_output(
        ctx.actions,
        ctx.label,
        android_packageable_info,
        traversed_shared_library_info,
        ctx.attrs._android_toolchain[AndroidToolchainInfo],
        ctx.attrs.application_module_configs,
        ctx.attrs.application_module_dependencies,
        ctx.attrs.application_module_blacklist,
    )

    if ctx.attrs.should_include_classes:
        no_dx_target_labels = [no_dx_target.label.raw_target() for no_dx_target in ctx.attrs.no_dx]
        java_packaging_deps = [packaging_dep for packaging_dep in get_all_java_packaging_deps(ctx, all_deps) if packaging_dep.dex and packaging_dep.dex.dex.owner.raw_target() not in no_dx_target_labels]
        targets_to_jars_args = [cmd_args([str(packaging_dep.label.raw_target()), packaging_dep.jar], delimiter = " ") for packaging_dep in java_packaging_deps]
        targets_to_jars = ctx.actions.write("targets_to_jars.txt", targets_to_jars_args)
        cmd.add([
            "--targets-to-jars",
            targets_to_jars,
        ]).hidden(targets_to_jars_args)

    if ctx.attrs.should_include_libraries:
        targets_to_so_names_args = [cmd_args([str(shared_lib.label.raw_target()), so_name, str(shared_lib.can_be_asset)], delimiter = " ") for so_name, shared_lib in traversed_shared_library_info.items()]
        targets_to_so_names = ctx.actions.write("targets_to_so_names.txt", targets_to_so_names_args)
        cmd.add([
            "--targets-to-so-names",
            targets_to_so_names,
        ]).hidden(targets_to_so_names_args)

    ctx.actions.run(cmd, category = "apk_module_graph")

    return [DefaultInfo(default_outputs = [output])]

def get_target_to_module_mapping(ctx: "context", deps: ["dependency"]) -> ["artifact", None]:
    if not ctx.attrs.application_module_configs:
        return None

    all_deps = deps + flatten(ctx.attrs.application_module_configs.values())
    android_packageable_info = merge_android_packageable_info(ctx.label, ctx.actions, all_deps)

    shared_library_info = merge_shared_libraries(
        ctx.actions,
        deps = filter_and_map_idx(SharedLibraryInfo, all_deps),
    )
    traversed_shared_library_info = traverse_shared_library_info(shared_library_info)

    cmd, output = _get_base_cmd_and_output(
        ctx.actions,
        ctx.label,
        android_packageable_info,
        traversed_shared_library_info,
        ctx.attrs._android_toolchain[AndroidToolchainInfo],
        ctx.attrs.application_module_configs,
        ctx.attrs.application_module_dependencies,
        ctx.attrs.application_module_blacklist,
    )

    cmd.add("--output-module-info-and-target-to-module-only")

    ctx.actions.run(cmd, category = "apk_module_graph")

    return output

def _get_base_cmd_and_output(
        actions: "actions",
        label: "label",
        android_packageable_info: "AndroidPackageableInfo",
        traversed_shared_library_info: {str.type: "SharedLibrary"},
        android_toolchain: "AndroidToolchainInfo",
        application_module_configs: {str.type: ["dependency"]},
        application_module_dependencies: [{str.type: [str.type]}, None],
        application_module_blocklist: [[["dependency"]], None]) -> ("cmd_args", "artifact"):
    deps_infos = list(android_packageable_info.deps.traverse()) if android_packageable_info.deps else []
    deps_map = {deps_info.name: deps_info.deps for deps_info in deps_infos}

    target_graph_file = actions.write_json("target_graph.json", deps_map)
    application_module_configs_map = {module_name: [seed.label.raw_target() for seed in seeds if seed[AndroidPackageableInfo]] for module_name, seeds in application_module_configs.items()}
    application_module_configs_file = actions.write_json("application_module_configs.json", application_module_configs_map)
    application_module_dependencies_file = actions.write_json("application_module_dependencies.json", application_module_dependencies or {})
    output = actions.declare_output("apk_module_metadata.txt")

    cmd = cmd_args([
        android_toolchain.apk_module_graph[RunInfo],
        "--root-target",
        str(label.raw_target()),
        "--target-graph",
        target_graph_file,
        "--seed-config-map",
        application_module_configs_file,
        "--app-module-dependencies-map",
        application_module_dependencies_file,
        "--output",
        output.as_output(),
    ])

    # Anything that is used by a wrap script needs to go into the primary APK, as do all
    # of their deps.

    used_by_wrap_script_libs = [str(shared_lib.label.raw_target()) for shared_lib in traversed_shared_library_info.values() if shared_lib.for_primary_apk]
    prebuilt_native_library_dirs = list(android_packageable_info.prebuilt_native_library_dirs.traverse()) if android_packageable_info.prebuilt_native_library_dirs else []
    prebuilt_native_library_targets_for_primary_apk = [str(native_lib_dir.raw_target) for native_lib_dir in prebuilt_native_library_dirs if native_lib_dir.for_primary_apk]
    if application_module_blocklist or used_by_wrap_script_libs or prebuilt_native_library_targets_for_primary_apk:
        all_blocklisted_deps = used_by_wrap_script_libs + prebuilt_native_library_targets_for_primary_apk
        if application_module_blocklist:
            all_blocklisted_deps.extend([str(blocklisted_dep.label.raw_target()) for blocklisted_dep in flatten(application_module_blocklist)])

        application_module_blocklist_file = actions.write(
            "application_module_blocklist.txt",
            all_blocklisted_deps,
        )
        cmd.add([
            "--always-in-main-apk-seeds",
            application_module_blocklist_file,
        ])

    return cmd, output

ROOT_MODULE = "dex"

def is_root_module(module: str.type) -> bool.type:
    return module == ROOT_MODULE

def all_targets_in_root_module(_module: str.type) -> str.type:
    return ROOT_MODULE

APKModuleGraphInfo = record(
    target_to_module_mapping_function = "function",
    module_to_canary_class_name_function = "function",
    module_to_module_deps_function = "function",
)

def get_root_module_only_apk_module_graph_info() -> APKModuleGraphInfo.type:
    def root_module_canary_class_name(module: str.type):
        expect(is_root_module(module))
        return "secondary"

    def root_module_deps(module: str.type):
        expect(is_root_module(module))
        return []

    return APKModuleGraphInfo(
        target_to_module_mapping_function = all_targets_in_root_module,
        module_to_canary_class_name_function = root_module_canary_class_name,
        module_to_module_deps_function = root_module_deps,
    )

def get_apk_module_graph_info(
        ctx: "context",
        apk_module_graph_file: "artifact",
        artifacts) -> APKModuleGraphInfo.type:
    apk_module_graph_lines = artifacts[apk_module_graph_file].read_string().split("\n")
    module_count = int(apk_module_graph_lines[0])
    module_infos = apk_module_graph_lines[1:module_count + 1]
    target_to_module_lines = apk_module_graph_lines[module_count + 1:-1]
    expect(apk_module_graph_lines[-1] == "", "Expect last line to be an empty string!")

    module_to_canary_class_name_map = {}
    module_to_module_deps_map = {}
    for line in module_infos:
        line_data = line.split(" ")
        module_name = line_data[0]
        canary_class_name = line_data[1]
        module_deps = [module_dep for module_dep in line_data[2:] if module_dep]
        module_to_canary_class_name_map[module_name] = canary_class_name
        module_to_module_deps_map[module_name] = module_deps

    target_to_module_mapping = {str(ctx.label.raw_target()): ROOT_MODULE}
    for line in target_to_module_lines:
        target, module = line.split(" ")
        target_to_module_mapping[target] = module

    def target_to_module_mapping_function(raw_target: str.type) -> str.type:
        return target_to_module_mapping.get(raw_target)

    def module_to_canary_class_name_function(voltron_module: str.type) -> str.type:
        return module_to_canary_class_name_map.get(voltron_module)

    def module_to_module_deps_function(voltron_module: str.type) -> list.type:
        return module_to_module_deps_map.get(voltron_module)

    return APKModuleGraphInfo(
        target_to_module_mapping_function = target_to_module_mapping_function,
        module_to_canary_class_name_function = module_to_canary_class_name_function,
        module_to_module_deps_function = module_to_module_deps_function,
    )
