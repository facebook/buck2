load("@fbcode//buck2/prelude/android:android_providers.bzl", "AndroidApkInfo")
load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "JavaToolchainInfo")
load("@fbcode//buck2/prelude/java/utils:java_utils.bzl", "get_path_separator")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

def android_instrumentation_test_impl(ctx: "context"):
    android_toolchain = ctx.attr._android_toolchain[AndroidToolchainInfo]

    cmd = [ctx.attr._java_toolchain[JavaToolchainInfo].java_for_tests]

    classpath = android_toolchain.instrumentation_test_runner_classpath

    classpath_args = cmd_args()
    classpath_args.add("-classpath")
    classpath_args.add(cmd_args(classpath, delimiter = get_path_separator()))
    classpath_args_file = ctx.actions.write("classpath_args_file", classpath_args)
    cmd.append(cmd_args(classpath_args_file, format = "@{}").hidden(classpath_args))

    cmd.append(android_toolchain.instrumentation_test_runner_main_class)

    apk_info = ctx.attr.apk[AndroidApkInfo]
    expect(apk_info != None, "Provided APK must have AndroidApkInfo!")

    target_package_file = ctx.actions.declare_output("target_package_file")
    package_file = ctx.actions.declare_output("package_file")
    test_runner_file = ctx.actions.declare_output("test_runner_file")
    manifest_utils_cmd = cmd_args(ctx.attr._android_toolchain[AndroidToolchainInfo].manifest_utils[RunInfo])
    manifest_utils_cmd.add([
        "--manifest-path",
        apk_info.manifest,
        "--package-output",
        package_file.as_output(),
        "--target-package-output",
        target_package_file.as_output(),
        "--instrumentation-test-runner-output",
        test_runner_file.as_output(),
    ])
    ctx.actions.run(manifest_utils_cmd, category = "get_manifest_info")

    cmd.extend(
        [
            "--test-package-name",
            cmd_args(package_file, format = "@{}"),
            "--target-package-name",
            cmd_args(target_package_file, format = "@{}"),
            "--test-runner",
            cmd_args(test_runner_file, format = "@{}"),
        ],
    )

    cmd.extend(
        [
            "--adb-executable-path",
            "required_but_unused",
            "--instrumentation-apk-path",
            apk_info.apk,
        ],
    )

    run_info = RunInfo(args = cmd_args(cmd))
    test_info = ExternalRunnerTestInfo(
        type = "android_instrumentation",
        command = cmd,
        env = ctx.attr.env,
        # TODO(T122022107) support static listing
        labels = ctx.attr.labels + ["tpx::dynamic_listing_instrumentation_test"],
        contacts = ctx.attr.contacts,
        run_from_project_root = True,
        use_project_relative_paths = True,
        executor_overrides = {
            "android-emulator": CommandExecutorConfig(
                local_enabled = False,
                remote_enabled = True,
                remote_execution_properties = {
                    "platform": "android-emulator",
                    "subplatform": "android-24",
                },
                remote_execution_use_case = "tpx-default",
            ),
            "static-listing": CommandExecutorConfig(local_enabled = True, remote_enabled = False),
        },
    )
    return [DefaultInfo(), test_info, run_info]
