load("@prelude//android:android_binary_resources_rules.bzl", "get_android_binary_resources_info")
load("@prelude//android:android_library.bzl", "build_android_library")
load("@prelude//android:android_providers.bzl", "merge_android_packageable_info")
load("@prelude//android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@prelude//java:java_test.bzl", "build_junit_test")
load("@prelude//java:java_toolchain.bzl", "JavaToolchainInfo")
load("@prelude//utils:utils.bzl", "expect")

def robolectric_test_impl(ctx: "context") -> ["provider"]:
    _verify_attributes(ctx)

    extra_cmds = []

    # Force robolectric to only use local dependency resolution.
    extra_cmds.append("-Drobolectric.offline=true")
    if ctx.attrs.robolectric_runtime_dependency:
        runtime_dependencies_dir = ctx.attrs.robolectric_runtime_dependency
    else:
        runtime_dependencies_dir = ctx.actions.symlinked_dir("runtime_dependencies", {
            runtime_dep.basename: runtime_dep
            for runtime_dep in ctx.attrs.robolectric_runtime_dependencies
        })

    extra_cmds.append(cmd_args(runtime_dependencies_dir, format = "-Drobolectric.dependency.dir={}"))

    all_packaging_deps = ctx.attrs.deps + (ctx.attrs.deps_query or []) + ctx.attrs.exported_deps + ctx.attrs.runtime_deps
    android_packageable_info = merge_android_packageable_info(ctx.label, ctx.actions, all_packaging_deps)
    resources_info = get_android_binary_resources_info(
        ctx,
        all_packaging_deps,
        android_packageable_info,
        java_packaging_deps = [],  # Only used for third-party jar resources, which we don't care about here.
        use_proto_format = False,
        referenced_resources_lists = [],
    )

    test_config_properties_file = ctx.actions.write(
        "test_config.properties",
        [
            cmd_args(["android_resource_apk", resources_info.primary_resources_apk], delimiter = "="),
            cmd_args(["android_merged_manifest", resources_info.manifest], delimiter = "="),
        ],
    )

    # Robolectric looks for a file named /com/android/tools/test_config.properties on the classpath
    test_config_symlinked_dir = ctx.actions.symlinked_dir("test_config_symlinked_dir", {"com/android/tools/test_config.properties": test_config_properties_file})
    test_config_properties_jar = ctx.actions.declare_output("test_config_properties.jar")
    jar_cmd = cmd_args([
        ctx.attrs._java_toolchain[JavaToolchainInfo].jar,
        "-cfM",  # -c: create new archive, -f: specify the file name, -M: do not create a manifest
        test_config_properties_jar.as_output(),
        "-C",
        test_config_symlinked_dir,
        ".",
    ])
    ctx.actions.run(jar_cmd, category = "test_config_properties_jar_cmd")
    extra_cmds.append(cmd_args().hidden(resources_info.primary_resources_apk, resources_info.manifest))

    extra_classpath_entries = [test_config_properties_jar] + ctx.attrs._android_toolchain[AndroidToolchainInfo].android_bootclasspath
    r_dot_java = None if resources_info.r_dot_java == None or resources_info.r_dot_java.library_output == None else resources_info.r_dot_java.library_output.full_library
    if r_dot_java != None:
        extra_classpath_entries.append(r_dot_java)

    java_providers = build_android_library(ctx, r_dot_java = r_dot_java)

    external_runner_test_info, run_info = build_junit_test(
        ctx,
        java_providers.java_library_info,
        java_providers.java_packaging_info,
        extra_cmds = extra_cmds,
        extra_classpath_entries = extra_classpath_entries,
    )

    return [
        java_providers.java_library_info,
        java_providers.java_packaging_info,
        java_providers.template_placeholder_info,
        java_providers.default_info,
        external_runner_test_info,
        run_info,
    ]

def _verify_attributes(ctx: "context"):
    expect(
        bool(ctx.attrs.robolectric_runtime_dependencies) != (ctx.attrs.robolectric_runtime_dependency != None),
        "Exactly one of robolectric_runtime_dependencies and robolectric_runtime_dependency must be specified!",
    )
