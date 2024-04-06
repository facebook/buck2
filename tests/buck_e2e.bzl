# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode//target_determinator/macros:ci.bzl", "ci")
load("@fbcode_macros//build_defs:native_rules.bzl", "buck_filegroup")
load("@fbcode_macros//build_defs:python_pytest.bzl", "python_pytest")

def _resource_control_test_config(env, skip_for_os):
    rc_env = dict(env)
    rc_env["BUCK2_TEST_RESOURCE_CONTROL_CONFIG"] = '{"status":"Required","memory_max":"infinity"}'
    rc_skip_for_os = list(skip_for_os)

    # Resource control is only available for linux
    if "windows" not in skip_for_os:
        rc_skip_for_os.append("windows")
    if "darwin" not in skip_for_os:
        rc_skip_for_os.append("darwin")
    return rc_env, tuple(rc_skip_for_os)

def buck_e2e_test(
        name,
        executable_type,
        executable,
        use_buck_api = True,
        contacts = None,
        base_module = None,
        data = None,
        data_dir = None,
        conftest = "fbcode//buck2/tests/e2e_util:conftest.py",
        srcs = (),
        tags = (),
        deps = (),
        env = None,
        resources = None,
        skip_for_os = (),
        pytest_config = None,
        pytest_marks = None,
        pytest_expr = None,
        pytest_confcutdir = None,
        serialize_test_cases = True):
    """
    Custom macro for buck2/buckaemon end-to-end tests using pytest.
    """
    for s in skip_for_os:
        if s not in ["darwin", "windows"]:
            fail("Skipped os must be one of darwin or windows, not {}".format(s))
    tags = list(tags) + [
        # Running multiple bucks are expensive. This limits tpx to parallelism of 4.
        "heavyweight",
    ]
    if serialize_test_cases:
        # This lets us pass stress runs by making all test cases inside of a test file serial
        # Test cases in different files can still run in parallel.
        tags.append("serialize_test_cases")
    env = env or {}
    env["RUST_BACKTRACE"] = "1"
    env["TEST_EXECUTABLE_TYPE"] = executable_type
    env["TEST_EXECUTABLE"] = executable

    # Having it enabled has significant negative impact on Starlark evaluation performance.
    env["RUST_LIB_BACKTRACE"] = "0"

    # Flags passed to pytest.
    # -vv shows full assertion output on failures.
    # ---tb=native shows python native traceback instead of default pytest traceback with source code.
    # --no-header disables headers printed after "test session starts" on output
    # --no-summary disables pytest summary printed after each test run on output
    env["PYTEST_ADDOPTS"] = "-vv --tb=native --no-header --no-summary"

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

    if type(deps) == "tuple":
        deps = list(deps)

    deps = [
        "fbsource//third-party/pypi/pytest:pytest",
        "fbsource//third-party/pypi/pytest-asyncio:pytest-asyncio",
        "fbcode//buck2/tests/e2e_util:utilities",
    ] + deps
    if use_buck_api:
        deps.append("fbcode//buck2/tests/e2e_util/api:api")
    resources = resources or {}
    resources[conftest] = "conftest.py"

    labels = []
    if "darwin" not in skip_for_os:
        labels += [ci.mac(ci.opt())]
    if "windows" not in skip_for_os:
        labels += [ci.windows(ci.opt())]

    python_pytest(
        name = name,
        base_module = base_module,
        srcs = srcs,
        tags = tags,
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
        labels = labels,
    )

def buck2_e2e_test(
        name,
        contacts = None,
        use_buck_api = True,
        test_with_compiled_buck2 = True,
        test_with_deployed_buck2 = False,
        test_with_reverted_buck2 = False,
        test_with_compiled_buck2_tpx = False,
        test_with_resource_control = False,
        base_module = None,
        data = None,
        data_dir = None,
        conftest = "fbcode//buck2/tests/e2e_util:conftest.py",
        srcs = (),
        tags = (),
        deps = (),
        env = None,
        resources = None,
        skip_for_os = (),
        pytest_config = None,
        pytest_marks = None,
        pytest_expr = None,
        pytest_confcutdir = None,
        serialize_test_cases = True):
    """
    Custom macro for buck2 end-to-end tests using pytest. All tests are run against buck2 compiled in-repo (compiled buck2).

    test_with_deployed_buck2:
        A boolean for whether to run tests with the deployed buck2.
        Default is False.
        Should typically be set for tests of UDRs and other things that are not "core buck2 functionality"
    test_with_compiled_buck2:
        A boolean for whether to run tests with the compiled buck2.
        Default is True.
        Should typically be unset when testing things that are not expected to be disproportionately
        sensitive to buck2 core changes. Unsetting this also simplifies the CI setup, as testing
        with buck2 core requires always using opt mode.
    test_with_compiled_buck2_tpx:
        A boolean for whether to run tests with buck2_tpx compiled in-repo or deployed buck2_tpx.
        Default is False.
        Deployed buck2 tests will ignore this field and always use deployed buck2_tpx.
    """
    env = env or {}
    if not test_with_compiled_buck2 and not test_with_deployed_buck2:
        fail("Must set one of `test_with_compiled_buck2` or `test_with_deployed_buck2` for " + name)
    if test_with_compiled_buck2:
        compiled_env = dict(env)
        compiled_env["BUCK2_HARD_ERROR"] = "true"
        if test_with_compiled_buck2_tpx:
            compiled_env["BUCK2_TPX"] = "$(location fbcode//buck2/buck2_tpx_cli:buck2_tpx_cli)"
        buck_e2e_test(
            name = name + ("_with_compiled_buck2" if test_with_deployed_buck2 else ""),
            executable_type = "buck2",
            executable = "$(location fbcode//buck2:buck2)",
            use_buck_api = use_buck_api,
            base_module = base_module,
            data = data,
            data_dir = data_dir,
            conftest = conftest,
            srcs = srcs,
            tags = tags,
            deps = deps,
            contacts = contacts,
            env = compiled_env,
            resources = resources,
            skip_for_os = skip_for_os,
            pytest_config = pytest_config,
            pytest_marks = pytest_marks,
            pytest_expr = pytest_expr,
            pytest_confcutdir = pytest_confcutdir,
            serialize_test_cases = serialize_test_cases,
        )
        if test_with_resource_control:
            compiled_rc_env, compiled_rc_skip_for_os = _resource_control_test_config(compiled_env, skip_for_os)
            buck_e2e_test(
                name = name + ("_with_compiled_buck2_with_resource_control" if test_with_deployed_buck2 else "_with_resource_control"),
                executable_type = "buck2",
                executable = "$(location fbcode//buck2:buck2)",
                use_buck_api = use_buck_api,
                base_module = base_module,
                data = data,
                data_dir = data_dir,
                conftest = conftest,
                srcs = srcs,
                tags = tags,
                deps = deps,
                contacts = contacts,
                env = compiled_rc_env,
                resources = resources,
                skip_for_os = compiled_rc_skip_for_os,
                pytest_config = pytest_config,
                pytest_marks = pytest_marks,
                pytest_expr = pytest_expr,
                pytest_confcutdir = pytest_confcutdir,
                serialize_test_cases = serialize_test_cases,
            )

    # soft errors should always be allowed on tests with deployed buck2, or with reverted buck
    env["BUCK2_HARD_ERROR"] = "false"
    if test_with_deployed_buck2:
        deps = deps or []

        # Add a buck2 version file as dep so we can run deployed buck2 tests on version bumps.
        deps.append("fbsource//tools/buck2-versions:stable")
        buck_e2e_test(
            # deployed buck2 test target retains the original target name so that when user runs `buck test <test target>`,
            # it only runs the deployed buck2 tests and not the compiled buck2 tests.
            # This will make it much quicker for rule writers to run their tests locally.
            name = name,
            executable_type = "buck2",
            executable = "buck2",
            use_buck_api = use_buck_api,
            base_module = base_module,
            data = data,
            data_dir = data_dir,
            conftest = conftest,
            srcs = srcs,
            tags = tags,
            deps = deps,
            contacts = contacts,
            env = env,
            resources = resources,
            skip_for_os = skip_for_os,
            pytest_config = pytest_config,
            pytest_marks = pytest_marks,
            pytest_expr = pytest_expr,
            pytest_confcutdir = pytest_confcutdir,
            serialize_test_cases = serialize_test_cases,
        )

    # TODO(T176595052): reenable `with_reverted_buck2` tests
    if test_with_reverted_buck2:
        pass

    #     env = dict(env)
    #     env["BUCK2_CHANNEL"] = "previous"
    #     buck_e2e_test(
    #         name = name + "_with_reverted_buck2",
    #         executable_type = "buck2",
    #         executable = "buck2",
    #         base_module = base_module,
    #         data = data,
    #         data_dir = data_dir,
    #         conftest = conftest,
    #         srcs = srcs,
    #         tags = tags,
    #         deps = deps,
    #         env = env,
    #         resources = resources,
    #         skip_for_os = skip_for_os,
    #         pytest_config = pytest_config,
    #         pytest_marks = pytest_marks,
    #         pytest_expr = pytest_expr,
    #         pytest_confcutdir = pytest_confcutdir,
    #         serialize_test_cases = serialize_test_cases,
    #     )
