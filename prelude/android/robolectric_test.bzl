load("@fbcode//buck2/prelude/android:android_binary_resources_rules.bzl", "get_android_binary_resources_info")
load("@fbcode//buck2/prelude/android:android_library.bzl", "build_android_library")
load("@fbcode//buck2/prelude/android:android_providers.bzl", "merge_android_packageable_info")
load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@fbcode//buck2/prelude/java:java_test.bzl", "build_junit_test")
load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "JavaToolchainInfo")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

def robolectric_test_impl(ctx: "context") -> ["provider"]:
    _verify_attributes(ctx)

    extra_cmd_args = cmd_args()

    # Force robolectric to only use local dependency resolution.
    extra_cmd_args.add("-Drobolectric.offline=true")
    extra_cmd_args.add(cmd_args(ctx.attr.robolectric_runtime_dependency, format = "-Drobolectric.dependency.dir={}"))

    all_packaging_deps = ctx.attr.deps + (ctx.attr.deps_query or []) + ctx.attr.exported_deps + ctx.attr.runtime_deps
    android_packageable_info = merge_android_packageable_info(ctx.actions, all_packaging_deps)
    resources_info = get_android_binary_resources_info(
        ctx,
        all_packaging_deps,
        android_packageable_info,
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
        ctx.attr._java_toolchain[JavaToolchainInfo].jar,
        "-cfM",  # -c: create new archive, -f: specify the file name, -M: do not create a manifest
        test_config_properties_jar.as_output(),
        "-C",
        test_config_symlinked_dir,
        ".",
    ])
    ctx.actions.run(jar_cmd, "test_config_properties_jar_cmd")
    extra_cmd_args.hidden(resources_info.primary_resources_apk, resources_info.manifest)

    extra_classpath_entries = [test_config_properties_jar] + ctx.attr._android_toolchain[AndroidToolchainInfo].android_bootclasspath
    r_dot_java = None if resources_info.r_dot_java == None or resources_info.r_dot_java.library_output == None else resources_info.r_dot_java.library_output.full_library
    if r_dot_java != None:
        extra_classpath_entries.append(r_dot_java)

    (
        tests_java_library_info,
        tests_java_packaging_info,
        _shared_library_info,
        _cxx_resources_info,
        template_placeholder_info,
        default_info,
    ) = build_android_library(ctx, r_dot_java = r_dot_java)

    external_runner_test_info, run_info = build_junit_test(
        ctx,
        tests_java_library_info,
        tests_java_packaging_info,
        extra_cmd_args = extra_cmd_args,
        extra_classpath_entries = extra_classpath_entries,
    )

    return [
        tests_java_library_info,
        template_placeholder_info,
        default_info,
        external_runner_test_info,
        run_info,
    ]

def _verify_attributes(ctx: "context"):
    expect(ctx.attr.robolectric_runtime_dependencies == [], "robolectric_runtime_dependencies is not currently supported in buck2!")
    expect(ctx.attr.robolectric_runtime_dependency != None, "Must specify a robolectric_runtime_dependency!")
