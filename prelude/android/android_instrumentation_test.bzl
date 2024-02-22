# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//android:android_providers.bzl", "AndroidApkInfo", "AndroidInstrumentationApkInfo")
load("@prelude//android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@prelude//java:class_to_srcs.bzl", "JavaClassToSourceMapInfo")
load("@prelude//java:java_toolchain.bzl", "JavaToolchainInfo")
load("@prelude//java/utils:java_more_utils.bzl", "get_path_separator_for_exec_os")
load("@prelude//utils:expect.bzl", "expect")
load("@prelude//test/inject_test_run_info.bzl", "inject_test_run_info")

ANDROID_EMULATOR_ABI_LABEL_PREFIX = "tpx-re-config::"
DEFAULT_ANDROID_SUBPLATFORM = "android-30"
DEFAULT_ANDROID_PLATFORM = "android-emulator"
DEFAULT_ANDROID_INSTRUMENTATION_TESTS_USE_CASE = "instrumentation-tests"

def android_instrumentation_test_impl(ctx: AnalysisContext):
    android_toolchain = ctx.attrs._android_toolchain[AndroidToolchainInfo]

    cmd = [ctx.attrs._java_toolchain[JavaToolchainInfo].java_for_tests]

    classpath = android_toolchain.instrumentation_test_runner_classpath

    classpath_args = cmd_args()
    classpath_args.add("-classpath")
    extra_classpath = []
    if ctx.attrs.instrumentation_test_listener != None:
        extra_classpath.append(ctx.attrs.instrumentation_test_listener)
    classpath_args.add(cmd_args(classpath + extra_classpath, delimiter = get_path_separator_for_exec_os(ctx)))
    classpath_args_file = ctx.actions.write("classpath_args_file", classpath_args)
    cmd.append(cmd_args(classpath_args_file, format = "@{}").hidden(classpath_args))

    cmd.append(android_toolchain.instrumentation_test_runner_main_class)

    apk_info = ctx.attrs.apk.get(AndroidApkInfo)
    expect(apk_info != None, "Provided APK must have AndroidApkInfo!")

    instrumentation_apk_info = ctx.attrs.apk.get(AndroidInstrumentationApkInfo)
    if instrumentation_apk_info != None:
        cmd.extend(["--apk-under-test-path", instrumentation_apk_info.apk_under_test])

    target_package_file = ctx.actions.declare_output("target_package_file")
    package_file = ctx.actions.declare_output("package_file")
    test_runner_file = ctx.actions.declare_output("test_runner_file")
    manifest_utils_cmd = cmd_args(ctx.attrs._android_toolchain[AndroidToolchainInfo].manifest_utils[RunInfo])
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

    if ctx.attrs.instrumentation_test_listener_class != None:
        cmd.extend(["--extra-instrumentation-test-listener", ctx.attrs.instrumentation_test_listener_class])

    cmd.extend(
        [
            "--adb-executable-path",
            "required_but_unused",
            "--instrumentation-apk-path",
            apk_info.apk,
        ],
    )

    remote_execution_properties = {
        "platform": _compute_emulator_platform(ctx.attrs.labels or []),
        "subplatform": _compute_emulator_subplatform(ctx.attrs.labels or []),
    }
    re_emulator_abi = _compute_emulator_abi(ctx.attrs.labels or [])
    if re_emulator_abi != None:
        remote_execution_properties["abi"] = re_emulator_abi

    test_info = ExternalRunnerTestInfo(
        type = "android_instrumentation",
        command = cmd,
        env = ctx.attrs.env,
        labels = ctx.attrs.labels,
        contacts = ctx.attrs.contacts,
        run_from_project_root = True,
        use_project_relative_paths = True,
        executor_overrides = {
            "android-emulator": CommandExecutorConfig(
                local_enabled = android_toolchain.instrumentation_test_can_run_locally,
                remote_enabled = True,
                remote_execution_properties = remote_execution_properties,
                remote_execution_use_case = _compute_re_use_case(ctx.attrs.labels or []),
            ),
            "static-listing": CommandExecutorConfig(
                local_enabled = True,
                remote_enabled = True,
                remote_execution_properties = {
                    "platform": "linux-remote-execution",
                },
                remote_execution_use_case = "buck2-default",
            ),
        },
        local_resources = {
            "android_emulator": None,
        },
    )

    classmap_source_info = [ctx.attrs.apk[JavaClassToSourceMapInfo]] if JavaClassToSourceMapInfo in ctx.attrs.apk else []

    return inject_test_run_info(ctx, test_info) + [
        DefaultInfo(),
    ] + classmap_source_info

def _compute_emulator_abi(labels: list[str]):
    emulator_abi_labels = [label for label in labels if label.startswith(ANDROID_EMULATOR_ABI_LABEL_PREFIX)]
    expect(len(emulator_abi_labels) <= 1, "multiple '{}' labels were found:[{}], there must be only one!".format(ANDROID_EMULATOR_ABI_LABEL_PREFIX, ", ".join(emulator_abi_labels)))
    if len(emulator_abi_labels) == 0:
        return None
    else:  # len(emulator_abi_labels) == 1:
        return emulator_abi_labels[0].replace(ANDROID_EMULATOR_ABI_LABEL_PREFIX, "")

# replicating the logic in https://fburl.com/code/1fqowxu4 to match buck1's behavior
def _compute_emulator_subplatform(labels: list[str]) -> str:
    emulator_subplatform_labels = [label for label in labels if label.startswith("re_emulator_")]
    expect(len(emulator_subplatform_labels) <= 1, "multiple 're_emulator_' labels were found:[{}], there must be only one!".format(", ".join(emulator_subplatform_labels)))
    if len(emulator_subplatform_labels) == 0:
        return DEFAULT_ANDROID_SUBPLATFORM
    else:  # len(emulator_subplatform_labels) == 1:
        return emulator_subplatform_labels[0].replace("re_emulator_", "")

def _compute_emulator_platform(labels: list[str]) -> str:
    emulator_platform_labels = [label for label in labels if label.startswith("re_platform_")]
    expect(len(emulator_platform_labels) <= 1, "multiple 're_platform_' labels were found:[{}], there must be only one!".format(", ".join(emulator_platform_labels)))
    if len(emulator_platform_labels) == 0:
        return DEFAULT_ANDROID_PLATFORM
    else:  # len(emulator_platform_labels) == 1:
        return emulator_platform_labels[0].replace("re_platform_", "")

def _compute_re_use_case(labels: list[str]) -> str:
    re_use_case_labels = [label for label in labels if label.startswith("re_opts_use_case=")]
    expect(len(re_use_case_labels) <= 1, "multiple 're_opts_use_case' labels were found:[{}], there must be only one!".format(", ".join(re_use_case_labels)))
    if len(re_use_case_labels) == 0:
        return DEFAULT_ANDROID_INSTRUMENTATION_TESTS_USE_CASE
    else:  # len(re_use_case_labels) == 1:
        return re_use_case_labels[0].replace("re_opts_use_case=", "")
