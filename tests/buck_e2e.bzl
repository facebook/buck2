# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode//buck2/app:modifier.bzl", "buck2_modifiers")
load("@fbcode_macros//build_defs:native_rules.bzl", "buck_filegroup")
load("@fbcode_macros//build_defs:python_pytest.bzl", "python_pytest")
load("@fbsource//tools/target_determinator/macros:ci.bzl", "ci")
load("@fbsource//tools/target_determinator/macros:ci_hint.bzl", "ci_hint")

def buck_e2e_test(
        name,
        executable,
        use_buck_api = True,
        contacts = None,
        base_module = None,
        data = None,
        data_dir = None,
        srcs = None,
        labels = None,
        deps = None,
        env = None,
        resources = None,
        skip_for_os = (),
        pytest_config = None,
        pytest_marks = None,
        pytest_expr = None,
        pytest_confcutdir = None,
        serialize_test_cases = None,
        require_nano_prelude = None,
        cfg_modifiers = None,
        ci_srcs = [],
        ci_deps = [],
        compatible_with = None,
        heavyweight_label: str | None = "heavyweight"):
    """
    Custom macro for buck2/buckaemon end-to-end tests using pytest.
    """
    srcs = srcs or []
    labels = labels or []
    deps = deps or []
    cfg_modifiers = cfg_modifiers or []

    for s in skip_for_os:
        if s not in ["darwin", "windows"]:
            fail("Skipped os must be one of darwin or windows, not {}".format(s))
    labels = list(labels)
    if heavyweight_label:
        labels += [heavyweight_label]
    env = env or {}
    env["RUST_BACKTRACE"] = "1"
    env["TEST_EXECUTABLE"] = executable

    # Having it enabled has significant negative impact on Starlark evaluation performance.
    env["RUST_LIB_BACKTRACE"] = "0"

    # Flags passed to pytest.
    # -vv shows full assertion output on failures.
    # ---tb=native shows python native traceback instead of default pytest traceback with source code.
    # --no-header disables headers printed after "test session starts" on output
    # --no-summary disables pytest summary printed after each test run on output
    env["PYTEST_ADDOPTS"] = "-vv --tb=native --no-header --no-summary"

    # For autodeps
    read_package_value = getattr(native, "read_package_value", None)
    e2e_flavor = read_package_value and read_package_value("buck2_e2e_test.flavor")
    if e2e_flavor == "isolated":
        env["BUCK2_E2E_TEST_FLAVOR"] = "isolated"
        serialize_test_cases = serialize_test_cases or False
    else:
        env["BUCK2_E2E_TEST_FLAVOR"] = "any"
        serialize_test_cases = serialize_test_cases if serialize_test_cases != None else True

    if serialize_test_cases:
        # This lets us pass stress runs by making all test cases inside of a test file serial
        # Test cases in different files can still run in parallel.
        labels.append("serialize_test_cases")

    if data and data_dir:
        fail("`data` and `data_dir` cannot be used together")

    # If there's `_data` dir next to test, use it as test data directory.
    if data_dir:
        if not data_dir.startswith("test_") or not data_dir.endswith("_data"):
            fail("Data dirs must be of the form `test_.*_data`, not {}".format(data_dir))
        buck_filegroup(
            name = data_dir,
            srcs = [data_dir],
            copy = False,
        )
        env["TEST_REPO_DATA"] = "$(location :{d})/{d}".format(d = data_dir)

    if data:
        env["TEST_REPO_DATA"] = "$(location {})".format(data)

    # Add nano_prelude unconditionally for isolated tests
    if require_nano_prelude == None:
        require_nano_prelude = data_dir != None
    if require_nano_prelude:
        env["NANO_PRELUDE"] = "$(location fbcode//buck2/tests/e2e_util/nano_prelude:nano_prelude)"

    deps += [
        "fbsource//third-party/pypi/pytest:pytest",
        "fbsource//third-party/pypi/pytest-asyncio:pytest-asyncio",
        "fbcode//buck2/tests/e2e_util:utilities",
    ]
    if use_buck_api:
        deps.append("fbcode//buck2/tests/e2e_util/api:api")
    resources = resources or {}

    # Let users of the macro define their own configuration for pytest. This allow for reusing all
    # the fixture code for tools building e2e tests that also need a working buck environment.
    if not "conftest.py" in resources.values():
        resources["fbcode//buck2/tests/e2e_util:conftest.py"] = "conftest.py"

    if "darwin" in skip_for_os:
        labels += ci.remove_labels(ci.mac(ci.aarch64(ci.opt())))
    if "windows" in skip_for_os:
        labels += ci.remove_labels(ci.windows(ci.opt()))

    metadata = {}
    metadata["buck.cfg_modifiers"] = cfg_modifiers

    python_pytest(
        name = name,
        base_module = base_module,
        srcs = srcs,
        labels = labels,
        deps = deps,
        env = env,
        emails = contacts,
        resources = resources,
        skip_on_mode_mac = "darwin" in skip_for_os,
        skip_on_mode_win = "windows" in skip_for_os,
        pytest_config = pytest_config,
        pytest_marks = pytest_marks,
        pytest_expr = pytest_expr,
        pytest_confcutdir = pytest_confcutdir,
        metadata = metadata,
        compatible_with = compatible_with,
    )

    if e2e_flavor == "buck2_non_isolated":
        # These are buck2's own non-isolated e2e tests. Add a ci hint indicating
        # that they depend on many of the macros in the repo. Intentionally
        # don't do this for other users of `buck2_e2e_test` in the repo
        BUCK2_E2E_TEST_CI_SRCS = [
            "fbandroid/buck2/**",
            "fbcode/buck2/cfg/**",
            "fbcode/buck2/prelude/**",
            "fbcode/buck2/platform/**",
            "fbcode/buck2/toolchains/**",
            "fbcode/buck2/tests/targets/**",
            "fbobjc/buck2/**",
            "xplat/buck2/**",
            "xplat/toolchains/**",
            "fbcode/hermetic_infra/fdb/**",
            "tools/build_defs/**",
            "arvr/tools/build_defs/config/**",
            ".buckconfig",
            "tools/buckconfigs/**",
        ]
        ci_srcs = ci_srcs + BUCK2_E2E_TEST_CI_SRCS
    if ci_srcs or ci_deps:
        ci_hint(
            ci_srcs = ci_srcs,
            ci_deps = ci_deps,
            reason = "Non isolated buck2 e2e tests depend heavily on macros",
            target = name,
            compatible_with = compatible_with,
        )

def buck2_e2e_test(
        name,
        test_with_compiled_buck2 = True,
        test_with_deployed_buck2 = False,
        test_with_reverted_buck2 = False,
        use_compiled_buck2_client_and_tpx = False,
        deps = (),
        env = None,
        skip_for_os = (),
        use_buck_api = True,
        contacts = None,
        base_module = None,
        data = None,
        data_dir = None,
        srcs = (),
        labels = (),
        resources = None,
        pytest_config = None,
        pytest_marks = None,
        pytest_expr = None,
        pytest_confcutdir = None,
        serialize_test_cases = None,
        require_nano_prelude = None,
        ci_srcs = [],
        ci_deps = [],
        compatible_with = None,
        heavyweight_label: str | None = "heavyweight"):
    """
    Custom macro for buck2 end-to-end tests using pytest. All tests are run against buck2 compiled in-repo (compiled buck2).

    test_with_compiled_buck2:
        A boolean for whether to run tests with the compiled buck2.
        Default is True.
        Should typically be unset when testing things that are not expected to be disproportionately
        sensitive to buck2 core changes. Unsetting this also simplifies the CI setup, as testing
        with buck2 core requires always using opt mode.
    test_with_deployed_buck2:
        A boolean for whether to run tests with the deployed buck2.
        Default is False.
        Should typically be set for tests of UDRs and other things that are not "core buck2 functionality"
    test_with_reverted_buck2:
        Like `test_with_deployed_buck2`, but for the previous version
    use_compiled_buck2_client_and_tpx:
        A full prod archive is distinct from a normal build of buck2 in that it uses a client-only
        binary and additionally makes TPX available. Needed if you want to be able to `buck.test`
        Default is False.
    heavyweight_label:
        Running multiple bucks are expensive. This label specifies how many cpu slots each test gets
        according to heavyweight_label. Default is 4. If set to None, then this label is not set.
        See different possible values for heavyweight label here:
        https://www.internalfb.com/wiki/TAE/tpx/Tpx_user_guide/#tests-that-oom-or-time-o.
    """
    kwargs = {
        "base_module": base_module,
        "ci_deps": ci_deps,
        "ci_srcs": ci_srcs,
        "compatible_with": compatible_with,
        "contacts": contacts,
        "data": data,
        "data_dir": data_dir,
        "labels": labels,
        "pytest_confcutdir": pytest_confcutdir,
        "pytest_config": pytest_config,
        "pytest_expr": pytest_expr,
        "pytest_marks": pytest_marks,
        "require_nano_prelude": require_nano_prelude,
        "resources": resources,
        "serialize_test_cases": serialize_test_cases,
        "srcs": srcs,
        "use_buck_api": use_buck_api,
    }

    env = env or {}
    if not test_with_compiled_buck2 and not test_with_deployed_buck2:
        fail("Must set one of `test_with_compiled_buck2` or `test_with_deployed_buck2` for " + name)

    # soft errors should always be allowed on tests with deployed buck2, or with reverted buck
    deployed_env = dict(env)
    deployed_env["BUCK2_HARD_ERROR"] = "false"

    if test_with_compiled_buck2:
        compiled_env = dict(env)
        compiled_env["BUCK2_HARD_ERROR"] = "true"
        compiled_env["BUCK2_TPX"] = "$BUCK2_BINARY_DIR/buck2-tpx"

        if use_compiled_buck2_client_and_tpx:
            base_exe = "$(location fbcode//buck2:symlinked_buck2_and_tpx)/buck2"
            exe = select({
                "DEFAULT": base_exe,
                "ovr_config//os:windows": base_exe + ".exe",
            })
        else:
            exe = "$(location fbcode//buck2:buck2)"

        buck_e2e_test(
            # deployed buck2 test target retains the original target name so that when user runs `buck test <test target>`,
            # it only runs the deployed buck2 tests and not the compiled buck2 tests.
            # This will make it much quicker for rule writers to run their tests locally.
            name = name + ("_with_compiled_buck2" if test_with_deployed_buck2 else ""),
            env = compiled_env,
            executable = exe,
            skip_for_os = skip_for_os,
            deps = deps,
            cfg_modifiers = buck2_modifiers() + [
                # Always run these tests under rust opt build
                "ovr_config//build_mode:opt",
            ],
            heavyweight_label = heavyweight_label,
            **kwargs
        )

    if test_with_deployed_buck2:
        deps = deps or []

        # Add a buck2 version file as dep so we can run deployed buck2 tests on version bumps.
        deps += ["fbsource//tools/buck2-versions:stable"]
        buck_e2e_test(
            name = name,
            env = deployed_env,
            executable = "buck2",
            skip_for_os = skip_for_os,
            deps = deps,
            heavyweight_label = heavyweight_label,
            **kwargs
        )

    if test_with_reverted_buck2:
        previous_env = dict(deployed_env)
        previous_env["BUCK2_CHANNEL"] = "previous"
        buck_e2e_test(
            name = name + "_with_reverted_buck2",
            env = previous_env,
            executable = "buck2",
            skip_for_os = skip_for_os,
            deps = deps,
            heavyweight_label = heavyweight_label,
            **kwargs
        )

def buck2_core_tests(
        extra_attrs = {},
        target_extra_attrs = {}):
    """
    A little wrapper that generates `buck2_e2e_test`s for core tests.

    extra_attrs:
        Extra attributes that are applied to all generated targets.
    target_extra_attrs:
        A map of target name to extra attrs to apply to that target.
    """

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    items = set([i.split("/")[0] for i in glob(["**/*", "**/.*"])])
    items = list(items)

    generated_targets = []

    for item in items:
        if item in ["TARGETS", "TARGETS.v2", "BUCK", "BUCK.v2"]:
            continue
        if item.startswith("test_") and item.endswith("_data"):
            # Just make sure the associated test exists
            if item[:-5] + ".py" not in items:
                fail("Test data directory {} exists but has no matching test!".format(item))
            continue
        if item.startswith("test_") and item.endswith(".py"):
            target = item[:-3]
            generated_targets.append(target)

            attrs = dict(extra_attrs)
            attrs.update(target_extra_attrs.get(target) or {})

            data_dir = target + "_data"
            if data_dir not in items:
                data_dir = None
            if "data_dir" in attrs:
                fail("May not set data dir in `extra_attrs`")
            attrs["data_dir"] = data_dir

            if "srcs" not in attrs:
                # Allowing people to override `srcs` seems fine
                attrs["srcs"] = [item]

            IMPLICIT_DEPS = [
                "//buck2/tests/e2e_util:utils",
                "//buck2/tests/e2e_util:golden",
            ]
            attrs["deps"] = list(attrs.get("deps") or [])
            attrs["deps"].extend(IMPLICIT_DEPS)

            buck2_e2e_test(
                name = target,
                **attrs
            )
            continue
        fail("Expected all directory entries to look like `test_*_data` or `test_*.py`, not {}".format(item))

    # Check that all target attrs actually correspond to a target
    for t in target_extra_attrs.keys():
        if t not in generated_targets:
            fail("No such target {}".format(t))
