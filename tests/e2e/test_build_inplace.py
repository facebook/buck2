import os
import shutil
import subprocess
import sys
from pathlib import Path

import pytest
from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.api.buck_result import BuckException, ExitCode
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test, env, is_deployed_buck2

# rust rule implementations hardcode invocation of `/bin/jq` which is not available on Mac RE workers (or mac laptops)
def rust_linux_only() -> bool:
    return sys.platform == "linux"


# builds targets in an fbcode target configuration, unsupported on mac RE workers
def fbcode_linux_only() -> bool:
    return sys.platform == "linux"


@buck_test(inplace=True)
async def test_buildfiles(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/interpreter/buildfiles:buildfile")


def extract_gen_folder(output: str) -> str:
    return output[: output.find("{0}gen{0}".format(os.path.sep)) + 4]


if rust_linux_only():

    @buck_test(inplace=True)
    async def test_build_symlink_rust_rule(buck: Buck) -> None:
        args = [
            "fbcode//buck2/tests/targets/rules/rust/hello_world:welcome",
            "--show-full-output",
        ]
        if sys.platform == "darwin":
            args.append("@mode/mac")
        result = await buck.build(*args)

        output_dict = result.get_target_to_build_output()
        for _target, output in output_dict.items():
            gen_folder = extract_gen_folder(output)
            # v1: buck2/tests/targets/rules/rust/hello_world/welcome#binary/welcome
            symlink = (
                Path(gen_folder)
                / "fbcode"
                / "buck2"
                / "tests"
                / "targets"
                / "rules"
                / "rust"
                / "hello_world"
                / "shared"
                / "welcome"
            )
            assert symlink.is_symlink()


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_build_symlink_python_rule(buck: Buck) -> None:
        args = [
            "fbcode//buck2/tests/targets/rules/python/hello_world:welcome",
            "--show-full-output",
        ]
        if sys.platform == "darwin":
            args.append("@mode/mac")
        result = await buck.build(*args)
        output_dict = result.get_target_to_build_output()
        for _target, output in output_dict.items():
            gen_folder = extract_gen_folder(output)
            # v1: buck2/tests/targets/rules/python/hello_world/welcome.par
            symlink = (
                Path(gen_folder)
                / "fbcode"
                / "buck2"
                / "tests"
                / "targets"
                / "rules"
                / "python"
                / "hello_world"
                / "welcome.par"
            )
            assert symlink.is_symlink()


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_build_symlink_cpp_rule(buck: Buck) -> None:
        args = [
            "fbcode//buck2/tests/targets/rules/cxx/hello_world:welcome",
            "--show-full-output",
        ]
        if sys.platform == "darwin":
            args.append("@mode/mac")
        result = await buck.build(*args)
        output_dict = result.get_target_to_build_output()
        for _target, output in output_dict.items():
            gen_folder = extract_gen_folder(output)
            # v1: buck2/tests/targets/rules/cxx/hello_world/welcome
            symlink = (
                Path(gen_folder)
                / "fbcode"
                / "buck2"
                / "tests"
                / "targets"
                / "rules"
                / "cxx"
                / "hello_world"
                / "welcome"
            )
            assert symlink.is_symlink()


@buck_test(inplace=True)
async def test_build_symlink_genrule_rule(buck: Buck) -> None:
    args = [
        "fbcode//buck2/tests/targets/rules/genrule/hello_world:welcome",
        "--show-full-output",
    ]
    if sys.platform == "darwin":
        args.append("@mode/mac")
    result = await buck.build(*args)
    output_dict = result.get_target_to_build_output()
    for _target, output in output_dict.items():
        gen_folder = extract_gen_folder(output)
        # v1: buck2/tests/targets/rules/genrule/hello_world/welcome/out.txt
        symlink = (
            Path(gen_folder)
            / "fbcode"
            / "buck2"
            / "tests"
            / "targets"
            / "rules"
            / "genrule"
            / "hello_world"
            / "out"
            / "out.txt"
        )
        assert symlink.is_symlink()


@buck_test(inplace=True)
async def test_build_symlink_sh_binary(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/rules/shell:diff"
    args = [target, "--show-full-output"]
    if sys.platform == "darwin":
        args.append("@mode/mac")
    if sys.platform == "win32":
        args.append("@mode/win")
    result = await buck.build(*args)
    output_dict = result.get_target_to_build_output()

    output = output_dict[target]
    gen_folder = extract_gen_folder(output)
    symlink = (
        Path(gen_folder) / "fbcode" / "buck2" / "tests" / "targets" / "rules" / "shell"
    )
    if sys.platform == "win32":
        symlink /= "diff.bat"
    else:
        symlink /= "diff"

    # Verify we can both versions:
    subprocess.check_call([output])
    subprocess.check_call([symlink])


@buck_test(inplace=True)
async def test_build_symlink_does_not_traverse_existing_symlinks(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/rules/shell:diff"

    args = [target, "--show-full-output"]
    if sys.platform == "darwin":
        args.append("@mode/mac")
    if sys.platform == "win32":
        args.append("@mode/win")
    result = await buck.build(*args)
    output_dict = result.get_target_to_build_output()

    output = output_dict[target]
    gen_folder = extract_gen_folder(output)
    symlink_folder = (
        Path(gen_folder) / "fbcode" / "buck2" / "tests" / "targets" / "rules" / "shell"
    )

    # Now, overwrite part of the symlink path with something we cannot traverse.
    path = symlink_folder.parent
    shutil.rmtree(path)
    # On Windows this is just non existing path.
    os.symlink("/dev/null", path)

    # Can we still build? If we delete the symlink when walking up the path, we
    # can. If we traverse it, we can't.
    await buck.build(*args)


@buck_test(inplace=True)
async def test_cquery(buck: Buck) -> None:
    result = await buck.cquery(
        """deps(fbcode//buck2/tests/targets/commands:exported)"""
    )
    assert result.stdout.startswith(
        "fbcode//buck2/tests/targets/commands:exported (ovr_config//platform/linux:x86_64-fbcode"
    )


@buck_test(inplace=True)
async def test_cquery_with_config_value(buck: Buck) -> None:
    deps_enabled_result = await buck.cquery(
        "--config",
        "user.deps_enabled=true",
        "deps(fbcode//buck2/tests/targets/commands:lib)",
    )
    assert "fbcode//buck2/tests/targets/commands:dynamic" in deps_enabled_result.stdout

    deps_disabled_result = await buck.cquery(
        "--config",
        "user.deps_enabled=false",
        "deps(fbcode//buck2/tests/targets/commands:lib)",
    )
    assert (
        "fbcode//buck2/tests/targets/commands:dynamic"
        not in deps_disabled_result.stdout
    )


@buck_test(inplace=True)
async def test_targets(buck: Buck) -> None:
    result = await buck.targets("fbcode//buck2/tests/targets/commands:")

    targets = [
        "fbcode//buck2/tests/targets/commands:dynamic",
        "fbcode//buck2/tests/targets/commands:exported",
        "fbcode//buck2/tests/targets/commands:lib",
    ]

    for target in targets:
        assert target in result.stdout


@buck_test(inplace=True)
async def test_targets_errors(buck: Buck) -> None:
    with pytest.raises(BuckException) as result:
        await buck.targets(
            "fbcode//buck2/tests/targets/commands:",
            "fbcode//buck2/tests/targets/non_existent_path:",
        )

    assert result.value.get_exit_code() == ExitCode.BUILD_ERROR


@buck_test(inplace=True)
async def test_explicit_targets_errors(buck: Buck) -> None:
    with pytest.raises(BuckException) as result:
        await buck.targets(
            "fbcode//buck2/tests/targets/commands:notarealtarget",
        )

    assert result.value.get_exit_code() == ExitCode.BUILD_ERROR
    assert (
        "No target with name `notarealtarget` in package `fbcode//buck2/tests/targets/commands`."
        in result.value.stderr
    )


@buck_test(inplace=True)
async def test_targets_with_config_value(buck: Buck) -> None:
    targets_enabled_result = await buck.targets(
        "--config",
        "user.targets_enabled=true",
        "fbcode//buck2/tests/targets/commands:",
    )
    assert (
        "fbcode//buck2/tests/targets/commands:config_defined_target"
        in targets_enabled_result.stdout
    )

    targets_disabled_result = await buck.targets(
        "--config",
        "user.targets_enabled=false",
        "fbcode//buck2/tests/targets/commands:",
    )
    assert (
        "fbcode//buck2/tests/targets/commands:config_defined_target"
        not in targets_disabled_result.stdout
    )

    targets_cell_rel_result = await buck.targets(
        "--config",
        "fbsource//user.targets_enabled=true",
        "fbcode//buck2/tests/targets/commands:",
    )
    assert targets_cell_rel_result.stdout == targets_disabled_result.stdout


@buck_test(inplace=True)
async def test_targets_root_relative_from_fbcode(buck: Buck) -> None:
    result = await buck.targets("""//buck2/tests/targets/commands:""")

    targets = [
        "fbcode//buck2/tests/targets/commands:dynamic",
        "fbcode//buck2/tests/targets/commands:exported",
        "fbcode//buck2/tests/targets/commands:lib",
    ]

    for target in targets:
        assert target in result.stdout


@buck_test(inplace=True)
async def test_targets_show_output(buck: Buck) -> None:
    for target in [
        "fbcode//buck2/tests/targets/rules/genrule:executable_helper",
        "fbcode//buck2/tests/targets/rules/export_file:exported.txt",
    ]:
        build_result = await buck.build(target, "--show-output")
        targets_result = await buck.targets(target, "--show-output")

        build_report = build_result.get_build_report()
        build_report_outputs = [
            (target, str(output)) for output in build_report.outputs_for_target(target)
        ]
        show_output_outputs = [
            (target, os.path.join(build_report.root, output))
            for target, output in targets_result.get_target_to_build_output().items()
        ]

        assert show_output_outputs == build_report_outputs


@buck_test(inplace=True)
async def test_targets_show_output_subtargets(buck: Buck) -> None:

    TARGET = "fbcode//buck2/tests/targets/rules/cxx:my_cpp1"
    SUBTARGET = "compilation-database"
    TARGET_WITH_SUBTARGET = (
        "fbcode//buck2/tests/targets/rules/cxx:my_cpp1[compilation-database]"
    )

    build_result = await buck.build(TARGET_WITH_SUBTARGET, "--show-output")
    targets_result = await buck.targets(TARGET_WITH_SUBTARGET, "--show-output")

    build_report = build_result.get_build_report()
    build_report_outputs = [
        (TARGET_WITH_SUBTARGET, str(output))
        for output in build_report.outputs_for_target(TARGET, SUBTARGET)
    ]
    show_output_outputs = [
        (target, os.path.join(build_report.root, output))
        for target, output in targets_result.get_target_to_build_output().items()
    ]

    assert show_output_outputs == build_report_outputs


@buck_test(inplace=True)
async def test_targets_show_full_output(buck: Buck) -> None:
    for target in [
        "fbcode//buck2/tests/targets/rules/genrule:executable_helper",
        "fbcode//buck2/tests/targets/rules/export_file:exported.txt",
    ]:
        build_result = await buck.build(target, "--show-full-output")
        targets_result = await buck.targets(target, "--show-full-output")

        build_report = build_result.get_build_report()
        build_report_outputs = [
            (target, str(output)) for output in build_report.outputs_for_target(target)
        ]
        show_output_outputs = [
            (target, os.path.join(build_report.root, output))
            for target, output in targets_result.get_target_to_build_output().items()
        ]

        assert show_output_outputs == build_report_outputs


if rust_linux_only():

    @buck_test(inplace=True)
    async def test_show_output(buck: Buck) -> None:
        TARGET = "fbcode//buck2/tests/targets/rules/genrule:executable_helper"
        result = await buck.build(TARGET, "--show-output")

        build_report = result.get_build_report()
        build_report_outputs = [
            (TARGET, str(output)) for output in build_report.outputs_for_target(TARGET)
        ]
        show_output_outputs = [
            (target, os.path.join(build_report.root, output))
            for target, output in result.get_target_to_build_output().items()
        ]

        assert show_output_outputs == build_report_outputs

        TARGET = "fbcode//buck2/tests/targets/rules/rust:hello_explicit"
        result = await buck.build(TARGET, "--show-output")

        build_report = result.get_build_report()
        build_report_outputs = [
            (TARGET, str(output)) for output in build_report.outputs_for_target(TARGET)
        ]
        show_output_outputs = [
            (target, os.path.join(build_report.root, output))
            for target, output in result.get_target_to_build_output().items()
        ]

        assert show_output_outputs == build_report_outputs

        TARGET = "fbcode//buck2/tests/targets/rules/cxx:my_cpp1"
        SUBTARGET = "compilation-database"
        TARGET_WITH_SUBTARGET = (
            "fbcode//buck2/tests/targets/rules/cxx:my_cpp1[compilation-database]"
        )
        result = await buck.build(TARGET_WITH_SUBTARGET, "--show-output")

        build_report = result.get_build_report()
        build_report_outputs = [
            (TARGET_WITH_SUBTARGET, str(output))
            for output in build_report.outputs_for_target(TARGET, SUBTARGET)
        ]
        show_output_outputs = [
            (target, os.path.join(build_report.root, output))
            for target, output in result.get_target_to_build_output().items()
        ]

        assert show_output_outputs == build_report_outputs


@buck_test(inplace=True)
async def test_show_full_output(buck: Buck) -> None:
    TARGET = "fbcode//buck2/tests/targets/rules/genrule:executable_helper"
    result = await buck.build(TARGET, "--show-full-output")

    build_report = result.get_build_report()
    build_report_outputs = [
        (TARGET, str(output)) for output in build_report.outputs_for_target(TARGET)
    ]
    show_output_outputs = list(result.get_target_to_build_output().items())

    assert show_output_outputs == build_report_outputs

    for _, output in show_output_outputs:
        assert os.path.isabs(output), f"Output path must be absolute, got `{output}`."
        assert os.path.exists(output), f"Output path `{output}` does not exist!"


@buck_test(inplace=True)
@env("BUCK_LOG", "info")
async def test_consistent_build(buck: Buck) -> None:
    args = ["fbcode//buck2/tests/targets/rules/genrule:"]
    if sys.platform == "win32":
        args.append("@mode/win")
    result0 = await buck.build(*args)
    await buck.kill()
    result1 = await buck.build(*args)
    # Don't know if action key should stay consistent between clean builds,
    # but number of cache misses should.
    assert sum(result0.get_action_to_cache_miss_count().values()) == sum(
        result1.get_action_to_cache_miss_count().values()
    )


@buck_test(inplace=True)
@env("BUCK_LOG", "info")
async def test_cached_build(buck: Buck) -> None:
    args = ["fbcode//buck2/tests/targets/rules/genrule:"]
    if sys.platform == "win32":
        args.append("@mode/win")
    await buck.build(*args)
    result = await buck.build(*args)
    # Should be empty since nothing needs to be rebuilt
    assert sum(result.get_action_to_cache_miss_count().values()) == 0


@buck_test(inplace=True)
async def test_build_test_dependencies(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/rules/sh_test:test_with_env"
    build = await buck.build(
        target,
        "-c",
        "build_report.unstable_include_other_outputs=true",
        "--build-test-dependencies",
        "--build-report=-",
    )
    report = build.get_build_report().build_report

    path = ["results", target, "other_outputs", "DEFAULT"]
    for p in path:
        report = report[p]

    has_file = False
    for artifact in report:
        if "__file__" in artifact:
            has_file = True

    assert has_file


@buck_test(inplace=True)
async def test_fat_platforms(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/fat_platforms:example_use"
    result = await buck.build(
        target,
        "-c",
        "build.execution_platforms=fbcode//buck2/tests/targets/fat_platforms:platforms",
        "--show-full-output",
    )
    output = result.get_target_to_build_output()[target]
    with open(output) as output:
        s = output.read()
        assert "darwin" in s, "expected 'darwin' in output: `{}`".format(output)
        assert "linux" in s, "expected 'darwin' in output: `{}`".format(output)


@buck_test(inplace=True)
@pytest.mark.skipif(is_deployed_buck2(), reason="Not implemented yet on master")
async def test_classpath_query(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/template_placeholder/...")


@buck_test(inplace=True)
@pytest.mark.skipif(is_deployed_buck2(), reason="Not implemented yet on master")
async def test_missing_outputs_error(buck: Buck) -> None:
    # Check that we a) say what went wrong, b) show the command and c) show
    # stdout & stderr.
    await expect_failure(
        buck.build("fbcode//buck2/tests/targets/rules/genrule/bad:my_genrule_bad"),
        stderr_regex="Action failed to produce output.*frecli.*Stdout: HELLO_STDOUT.*Stderr: HELLO_STDERR",
    )

    # Same, but locally.
    await expect_failure(
        buck.build(
            "fbcode//buck2/tests/targets/rules/genrule/bad:my_genrule_bad_local"
        ),
        stderr_regex="Action failed to produce output.*OUT=.*Stdout: HELLO_STDOUT.*Stderr: HELLO_STDERR",
    )
