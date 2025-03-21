# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""Module containing java macros."""

load("@fbsource//tools/build_defs:fb_native_wrapper.bzl", "fb_native")
load("@fbsource//tools/build_defs/features:feature_decorator.bzl", "consume_feature")
load("@prelude//:native.bzl", "native")
load("@prelude//toolchains/android/tools/build_rules:utils.bzl", "add_os_labels", "is_oss_build")

OPEN_JDK_COMPILER_ARGS = [
    "--add-exports=jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED",
    "--add-exports=jdk.compiler/com.sun.tools.javac.comp=ALL-UNNAMED",
    "--add-exports=jdk.compiler/com.sun.tools.javac.file=ALL-UNNAMED",
    "--add-exports=jdk.compiler/com.sun.tools.javac.jvm=ALL-UNNAMED",
    "--add-exports=jdk.compiler/com.sun.tools.javac.model=ALL-UNNAMED",
    "--add-exports=jdk.compiler/com.sun.tools.javac.parser=ALL-UNNAMED",
    "--add-exports=jdk.compiler/com.sun.tools.javac.processing=ALL-UNNAMED",
    "--add-exports=jdk.compiler/com.sun.tools.javac.tree=ALL-UNNAMED",
    "--add-opens=jdk.compiler/com.sun.tools.javac.api=ALL-UNNAMED",
    "--add-opens=jdk.compiler/com.sun.tools.javac.code=ALL-UNNAMED",
    "--add-opens=jdk.compiler/com.sun.tools.javac.jvm=ALL-UNNAMED",
    "--add-opens=jdk.compiler/com.sun.tools.javac.main=ALL-UNNAMED",
    "--add-opens=jdk.compiler/com.sun.tools.javac.processing=ALL-UNNAMED",
    "--add-opens=jdk.compiler/com.sun.tools.javac.util=ALL-UNNAMED",
]

_RUN_AS_BUNDLE_LABEL = "run_as_bundle"
_FDB_DEBUG_LABEL = "fdb:target:android"

def _maybe_add_java_version(**kwargs):
    if "source" not in kwargs and "target" not in kwargs and "java_version" not in kwargs:
        kwargs["java_version"] = "11"
    return kwargs

def _add_labels(**kwargs):
    if "labels" not in kwargs:
        kwargs["labels"] = []
    kwargs["labels"] += ["wrapped_with_buck_java_rules", "pfh:Infra"]
    return kwargs

def _set_buck2_java_toolchain(**kwargs):
    kwargs["_java_toolchain"] = "toolchains//:java_bootstrap"
    return kwargs

def _set_buck2_java_graalvm_toolchain(**kwargs):
    kwargs["_java_toolchain"] = "toolchains//:java_graalvm_bootstrap"
    return kwargs

def _set_buck2_kotlin_toolchain(**kwargs):
    kwargs["_kotlin_toolchain"] = "toolchains//:kotlin_bootstrap"
    return kwargs

def _set_buck2_dex_toolchain(**kwargs):
    # Override dex toolchain to avoid dependency cycles in unconfigured graph
    dex_toolchain = kwargs.pop("_dex_toolchain", None)
    kwargs["_dex_toolchain"] = dex_toolchain or select({
        "DEFAULT": "toolchains//:empty_dex",
        "ovr_config//os/constraints:android": "toolchains//:dex",
    })
    return kwargs

def _set_versioned_java_srcs(**kwargs):
    if not kwargs.pop("versioned_java_srcs", False):
        return kwargs
    java_version = native.read_config("java", "buck2_java_version", "17")
    versioned_srcs = native.glob(["java{}/*.java".format(java_version)])
    kwargs["srcs"] = kwargs.get("srcs", []) + versioned_srcs
    return kwargs

def _add_kotlin_deps(**kwargs):
    kwargs["deps"] = kwargs.pop("deps", []) + [
        "prelude//toolchains/android/third-party:kotlin-annotations",
        "prelude//toolchains/android/third-party:kotlin-stdlib",
    ]
    return kwargs

def buck_kotlin_library(name, **kwargs):
    kwargs = _maybe_add_java_version(**kwargs)
    kwargs = _set_buck2_java_toolchain(**kwargs)
    kwargs = _set_buck2_kotlin_toolchain(**kwargs)
    kwargs = _add_kotlin_deps(**kwargs)
    return fb_native.kotlin_library(
        name = name,
        **kwargs
    )

def buck_java_library(name, **kwargs):
    kwargs = _add_labels(**kwargs)
    kwargs = _maybe_add_java_version(**kwargs)
    kwargs = _set_buck2_java_toolchain(**kwargs)
    kwargs = _set_buck2_dex_toolchain(**kwargs)
    kwargs = _set_versioned_java_srcs(**kwargs)
    return fb_native.java_library(
        name = name,
        **kwargs
    )

def buck_java_binary(name, **kwargs):
    kwargs = _add_labels(**kwargs)
    kwargs = _set_buck2_java_toolchain(**kwargs)
    java_args = kwargs["java_args_for_run_info"] if "java_args_for_run_info" in kwargs else []

    # Directs the VM to refrain from setting the file descriptor limit to the default maximum.
    # https://stackoverflow.com/a/16535804/5208808
    java_args += ["-XX:-MaxFDLimit", "-Xss2m"]
    kwargs["java_args_for_run_info"] = java_args
    return fb_native.java_binary(
        name = name,
        **kwargs
    )

def buck_java_graalvm_binary(name, **kwargs):
    kwargs = _add_labels(**kwargs)
    kwargs = _set_buck2_java_graalvm_toolchain(**kwargs)
    java_args = kwargs["java_args_for_run_info"] if "java_args_for_run_info" in kwargs else []

    # Directs the VM to refrain from setting the file descriptor limit to the default maximum.
    # https://stackoverflow.com/a/16535804/5208808
    java_args += ["-XX:-MaxFDLimit", "-Xss2m"]
    kwargs["java_args_for_run_info"] = java_args
    return fb_native.java_binary(
        name = name,
        **kwargs
    )

def toolchain_prebuilt_jar(name, **kwargs):
    kwargs = _add_labels(**kwargs)
    kwargs = _set_buck2_dex_toolchain(**kwargs)
    kwargs = consume_feature(kwargs)
    if kwargs.pop("should_generate_snapshot", True) == False:
        kwargs["_prebuilt_jar_toolchain"] = "toolchains//:prebuilt_jar_bootstrap_no_snapshot"
    else:
        kwargs["_prebuilt_jar_toolchain"] = "toolchains//:prebuilt_jar_bootstrap"
    return fb_native.prebuilt_jar(
        name = name,
        **kwargs
    )

def _oss_remote_file_with_wrapper(name, ext, url, sha1, **kwargs):
    remote_file_target_name = name + "_" + ext
    if ext == "jar":
        toolchain_prebuilt_jar(
            name = name,
            binary_jar = ":" + remote_file_target_name,
            **kwargs
        )
    elif ext == "aar":
        fb_native.android_prebuilt_aar(
            name = name,
            aar = ":" + remote_file_target_name,
            **kwargs
        )

    fb_native.remote_file(
        name = remote_file_target_name,
        out = name + "." + ext,
        sha1 = sha1,
        url = url,
    )

def _buck_remote_file_with_wrapper(
        name,
        ext,
        url,
        sha1,
        # @oss-disable[end= ]: internal_alias,
        **kwargs):
    if is_oss_build():
        return _oss_remote_file_with_wrapper(name, ext, url, sha1, **kwargs)
    # @oss-disable[end= ]: else:
        # @oss-disable: # deps are managed by Artificer internally - only relevant for OSS builds.
        # @oss-disable[end= ]: kwargs.pop("deps", None)
        # @oss-disable[end= ]: return native.alias(name = name, actual = internal_alias, **kwargs)
        fail() # @oss-enable

def third_party_jar(
        name,
        url,
        sha1,
        # @oss-disable[end= ]: internal_alias,
        **kwargs):
    return _buck_remote_file_with_wrapper(
        name,
        "jar",
        url,
        sha1,
        # @oss-disable[end= ]: internal_alias,
        **kwargs
    )

def third_party_aar(
        name,
        url,
        sha1,
        # @oss-disable[end= ]: internal_alias,
        **kwargs):
    return _buck_remote_file_with_wrapper(
        name,
        "aar",
        url,
        sha1,
        # @oss-disable[end= ]: internal_alias,
        **kwargs
    )

def buck_prebuilt_jar(name, **kwargs):
    return toolchain_prebuilt_jar(name = name, **kwargs)

def _shallow_dict_copy_without_key(table, key_to_omit):
    """Returns a shallow copy of dict with key_to_omit omitted."""
    return {key: table[key] for key in table if key != key_to_omit}

def kotlin_test(**kwargs):
    extra_labels = [_RUN_AS_BUNDLE_LABEL, _FDB_DEBUG_LABEL]

    kwargs = _add_labels(**kwargs)
    kwargs = add_os_labels(**kwargs)
    kwargs["labels"] += extra_labels

    kwargs = _add_kotlin_deps(**kwargs)

    fb_native.kotlin_test(**kwargs)

def java_test(
        name,
        vm_args = None,
        run_test_separately = False,
        **kwargs):
    """java_test wrapper that provides sensible defaults for buck tests.

    Args:
      name: name
      vm_args: vm_args
      run_test_separately: run_test_separately
      **kwargs: kwargs
    """

    extra_labels = [_RUN_AS_BUNDLE_LABEL, _FDB_DEBUG_LABEL]

    # Windows command line is short and running a bundle with many tests can cause problems
    # We fix this by running bundles of max 100 tests
    if native.host_info().os.is_windows:
        extra_labels.append("tpx:experimental-shard-size-for-bundle=100")

    if run_test_separately:
        extra_labels.append("serialize")

    if "deps" in kwargs:
        deps = kwargs["deps"]
        kwargs = _shallow_dict_copy_without_key(kwargs, "deps")
    else:
        deps = []

    if "env" in kwargs:
        env = kwargs["env"]
        kwargs = _shallow_dict_copy_without_key(kwargs, "env")
    else:
        env = {}

    kwargs = _maybe_add_java_version(**kwargs)
    kwargs = _add_labels(**kwargs)
    kwargs = add_os_labels(**kwargs)
    kwargs["labels"] += extra_labels

    fb_native.java_test(
        name = name,
        deps = deps + [
            # When actually running Buck, the launcher script loads the bootstrapper,
            # and the bootstrapper loads the rest of Buck. For unit tests, which don't
            # run Buck, we have to add a direct dependency on the bootstrapper in case
            # they exercise code that uses it.
            "prelude//toolchains/android/src/com/facebook/buck/cli/bootstrapper:bootstrapper_lib",
        ],
        vm_args = [
            # Don't use the system-installed JNA; extract it from the local jar.
            "-Djna.nosys=true",

            # Add -Dsun.zip.disableMemoryMapping=true to work around a JDK issue
            # related to modifying JAR/ZIP files that have been loaded into memory:
            #
            # http://bugs.sun.com/view_bug.do?bug_id=7129299
            #
            # This has been observed to cause a problem in integration tests such as
            # CachedTestIntegrationTest where `buck build //:test` is run repeatedly
            # such that a corresponding `test.jar` file is overwritten several times.
            # The CompiledClassFileFinder in JavaTestRule creates a java.util.zip.ZipFile
            # to enumerate the zip entries in order to find the set of .class files
            # in `test.jar`. This interleaving of reads and writes appears to match
            # the conditions to trigger the issue reported on bugs.sun.com.
            #
            # Currently, we do not set this flag in bin/buck_common, as Buck does not
            # normally modify the contents of buck-out after they are loaded into
            # memory. However, we may need to use this flag when running buckd where
            # references to zip files may be long-lived.
            #
            # Finally, note that when you specify this flag,
            # `System.getProperty("sun.zip.disableMemoryMapping")` will return `null`
            # even though you have specified the flag correctly. Apparently sun.misc.VM
            # (http://www.docjar.com/html/api/sun/misc/VM.java.html) saves the property
            # internally, but removes it from the set of system properties that are
            # publicly accessible.
            "-Dsun.zip.disableMemoryMapping=true",
        ] + (vm_args or []),
        env = env,
        run_test_separately = run_test_separately,
        **kwargs
    )

def standard_java_test(
        name,
        run_test_separately = False,
        vm_args = None,
        fork_mode = "none",
        labels = None,
        with_test_data = False,
        **kwargs):
    test_srcs = native.glob(["*Test.java"])

    if len(test_srcs) > 0:
        # @lint-ignore BUCKLINT
        java_test(
            name = name,
            srcs = test_srcs,
            resources = native.glob(["testdata/**"]) if with_test_data else [],
            vm_args = vm_args,
            run_test_separately = run_test_separately,
            fork_mode = fork_mode,
            labels = (labels or []) + ["buck2_run_from_cell_root"],
            **kwargs
        )
