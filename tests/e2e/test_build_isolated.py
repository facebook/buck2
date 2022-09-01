import fileinput
import json
import os
import platform
import random
import re
import shutil
import socket
import string
import subprocess
import sys
import tempfile
import typing
from pathlib import Path

import pytest

from py._path.local import LocalPath
from xplat.build_infra.buck_e2e import asserts
from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.asserts import expect_failure
from xplat.build_infra.buck_e2e.buck_workspace import buck_test, env


"""
If you need to add a directory that's isolated in buck2/test/targets
(ex. some test of form @buck_test(inplace=False, data_dir=some_new_directory)),
then you will need to update isolated_targets in buck2/test/targets/TARGETS.
Otherwise the test will fail because it cannot recognize the new directory.
"""

# Eden materializer only available on Linux
def eden_linux_only() -> bool:
    return sys.platform == "linux"


def watchman_dependency_linux_only() -> bool:
    return sys.platform == "linux"


@buck_test(inplace=False, data_dir="pass")
async def test_pass(buck: Buck) -> None:
    results = await buck.build("//:abc")
    assert "does not have any outputs: building it does nothing" not in results.stderr


@buck_test(inplace=False, data_dir="pass")
async def test_missing_target(buck: Buck) -> None:
    await expect_failure(buck.build("//:not_a_target_name"))


def _replace_hash(s: str) -> str:
    return re.sub(r"\b[0-9a-f]{16}\b", "<HASH>", s)


def _replace_hashes(strings: typing.List[str]) -> typing.List[str]:
    return [_replace_hash(s) for s in strings]


@buck_test(inplace=False, data_dir="build_report")
async def test_build_report_format(buck: Buck) -> None:
    await buck.build(
        "//:rule1", "//:rule2", "--build-report", "report", "//:rule2[out1]"
    )
    with open(buck.cwd / "report") as file:
        report = json.load(file)

        assert report["success"]
        assert report["failures"] == {}

        results = report["results"]

        rule1 = results["root//:rule1"]
        assert rule1["success"] == "SUCCESS"
        assert _replace_hashes(rule1["outputs"]["DEFAULT"]) == [
            "buck-out/v2/gen/root/<HASH>/__rule1__/out.txt"
        ]
        assert rule1["other_outputs"] == {}
        rule1_configured = rule1["configured"]["<unspecified>"]
        assert rule1_configured["success"] == "SUCCESS"
        assert _replace_hashes(rule1_configured["outputs"]["DEFAULT"]) == [
            "buck-out/v2/gen/root/<HASH>/__rule1__/out.txt"
        ]
        assert rule1_configured["other_outputs"] == {}

        rule2 = results["root//:rule2"]
        assert rule2["success"] == "SUCCESS"
        assert _replace_hashes(rule2["outputs"]["DEFAULT"]) == [
            "buck-out/v2/gen/root/<HASH>/__rule2__/out1.txt"
        ]
        assert _replace_hashes(rule2["outputs"]["out1"]) == [
            "buck-out/v2/gen/root/<HASH>/__rule2__/out1.txt"
        ]
        assert rule2["other_outputs"] == {}

        rule2_configured = rule2["configured"]["<unspecified>"]
        assert rule2_configured["success"] == "SUCCESS"
        assert _replace_hashes(rule2_configured["outputs"]["DEFAULT"]) == [
            "buck-out/v2/gen/root/<HASH>/__rule2__/out1.txt"
        ]
        assert _replace_hashes(rule2_configured["outputs"]["out1"]) == [
            "buck-out/v2/gen/root/<HASH>/__rule2__/out1.txt"
        ]
        assert rule2_configured["other_outputs"] == {}


@buck_test(inplace=False, data_dir="build_report")
async def test_build_report_format_skip_unconfigured(buck: Buck) -> None:
    await buck.build(
        "//:rule1",
        "--build-report",
        "report",
        "-c",
        "build_report.print_unconfigured_section=false",
    )
    with open(buck.cwd / "report") as file:
        report = json.load(file)

        assert report["success"]
        assert report["failures"] == {}

        results = report["results"]

        rule1 = results["root//:rule1"]
        assert "success" not in rule1
        assert "outputs" not in rule1
        assert "other_outputs" not in rule1
        rule1_configured = rule1["configured"]["<unspecified>"]
        assert rule1_configured["success"] == "SUCCESS"
        assert _replace_hashes(rule1_configured["outputs"]["DEFAULT"]) == [
            "buck-out/v2/gen/root/<HASH>/__rule1__/out.txt"
        ]
        assert rule1_configured["other_outputs"] == {}


@buck_test(inplace=False, data_dir="actions")
async def test_write_files(buck: Buck) -> None:
    result = await buck.build(
        "//write:simple",
        "//write:uses_declared_output",
        "//write:uses_declared_output_as_output",
        "//write:declares_output",
        "//write:is_executable",
        "//write:writes_array_of_commands",
        "//write:writes_command_lines",
        "//write:writes_frozen_command_lines",
    )
    build_report = result.get_build_report()

    simple = build_report.output_for_target("//write:simple", rel_path=True)

    output = build_report.output_for_target("//write:uses_declared_output")
    assert output.read_text().rstrip() == "some content"
    asserts.assert_not_executable(output)

    output = build_report.output_for_target("//write:uses_declared_output_as_output")
    assert output.read_text().rstrip() == "some content"
    asserts.assert_not_executable(output)

    output = build_report.output_for_target("//write:declares_output")
    assert output.read_text().rstrip() == "some content"
    asserts.assert_not_executable(output)

    output = build_report.output_for_target("//write:is_executable")
    assert output.read_text().rstrip() == "some content"
    asserts.assert_executable(output)

    output = build_report.output_for_target("//write:writes_array_of_commands")
    assert output.read_text().rstrip() == f"{str(simple)}\nsome content"
    asserts.assert_not_executable(output)

    output = build_report.output_for_target("//write:writes_command_lines")
    assert output.read_text().rstrip() == f"{str(simple)}\nsome content"
    asserts.assert_not_executable(output)

    output = build_report.output_for_target("//write:writes_frozen_command_lines")
    assert output.read_text().rstrip() == str(simple)
    asserts.assert_not_executable(output)

    await expect_failure(
        buck.build("//write:fails_on_invalid_contents"),
        stderr_regex="expected command line item",
    )

    await expect_failure(
        buck.build("//write:fails_on_invalid_output"),
        stderr_regex="Type of parameter `output`",
    )


@buck_test(inplace=False, data_dir="actions")
async def test_output_size(buck: Buck) -> None:
    await buck.build("//write:simple")

    output_size = None
    log = (await buck.log("show")).stdout.strip().splitlines()
    for line in log:
        o = json_get(
            line,
            "Event",
            "data",
            "SpanEnd",
            "data",
            "ActionExecution",
            "output_size",
        )

        if o is not None:
            output_size = o

    assert output_size == 8


@buck_test(inplace=False, data_dir="actions")
async def test_write_json(buck: Buck) -> None:
    await buck.build("//write_json:", "-c", "write_json.content=default")
    # we need to test that with_inputs properly flows input dependencies through to consumers
    await buck.build("//write_json:with_inputs", "-c", "write_json.content=other")


@buck_test(inplace=False, data_dir="actions")
async def test_copies_files(buck: Buck) -> None:
    result = await buck.build(
        "//copy:file_uses_declared_output",
        "//copy:file_uses_declared_output_as_output",
        "//copy:file_declares_output",
    )
    build_report = result.get_build_report()

    output = build_report.output_for_target("//copy:file_uses_declared_output")
    assert output.read_text().rstrip() == "some file"

    output = build_report.output_for_target(
        "//copy:file_uses_declared_output_as_output"
    )
    assert output.read_text().rstrip() == "some file"

    output = build_report.output_for_target("//copy:file_declares_output")
    assert output.read_text().rstrip() == "some file"

    await expect_failure(
        buck.build("//copy:fails_on_invalid_src"),
        stderr_regex="Type of parameter `src`",
    )

    await expect_failure(
        buck.build("//copy:fails_on_invalid_dest"),
        stderr_regex="Type of parameter `dest`",
    )


@buck_test(inplace=False, data_dir="actions")
async def test_symlink_dir(buck: Buck) -> None:
    result = await buck.build("//symlinked_dir:")
    build_report = result.get_build_report()
    output = build_report.output_for_target("//symlinked_dir:out")

    dest1 = output / "dir1" / "dir1_1" / "file1.txt"
    dest2 = output / "dep.txt"
    dest3 = output / "subdir" / "dir1" / "dir1_1" / "file1.txt.suffix"
    dest4 = output / "subdir" / "dep.txt.suffix"

    # Example subdir: buck-out/v2/gen/root/a59b783ba97fcd85891ddb2e62fbfebb/symlinked_dir/__out__/out/dir1/dir1_1
    expected_link1 = "../" * 10 + "symlinked_dir/dir1/dir1_1/file1.txt"
    expected_link2 = "../../__dep__/dep.txt"
    expected_link3 = "../" * 11 + "symlinked_dir/dir1/dir1_1/file1.txt"
    expected_link4 = "../../../__dep__/dep.txt"

    if platform.system() == "Windows":
        expected_link1 = expected_link1.replace("/", "\\")
        expected_link2 = expected_link2.replace("/", "\\")
        expected_link3 = expected_link3.replace("/", "\\")
        expected_link4 = expected_link4.replace("/", "\\")

    assert dest1.is_symlink()
    assert dest2.is_symlink()
    assert dest3.is_symlink()
    assert dest4.is_symlink()

    assert os.readlink(dest1) == expected_link1
    assert os.readlink(dest2) == expected_link2
    assert os.readlink(dest3) == expected_link3
    assert os.readlink(dest4) == expected_link4

    assert dest1.read_text().strip() == "dir1_1 out contents"
    assert dest2.read_text().strip() == "dep contents"
    assert dest3.read_text().strip() == "dir1_1 out contents"
    assert dest4.read_text().strip() == "dep contents"


@buck_test(inplace=False, data_dir="actions")
async def test_simple_run(buck: Buck) -> None:
    result = await buck.build("//run:runs_simple_script")
    output = result.get_build_report().output_for_target("//run:runs_simple_script")
    if platform.system() == "Windows":
        assert output.read_text() == "foo\nrun\\src.txt\nbar\n"
    else:
        assert output.read_text() == "foo\nrun/src.txt\nbar\n"

    result = await buck.build("//run:runs_script_locally")
    output = result.get_build_report().output_for_target("//run:runs_script_locally")
    assert output.read_text().strip() == socket.gethostname()

    result = await buck.build("//run:runs_script_locally_outputs_symlink")
    output = result.get_build_report().output_for_target(
        "//run:runs_script_locally_outputs_symlink"
    )
    assert output.is_symlink()

    await expect_failure(
        buck.build("//run:rejects_zero_outputs"),
        stderr_regex="expected at least one output artifact",
    )

    await expect_failure(
        buck.build("//run:rejects_bad_args"),
        stderr_regex="expected command line item to be",
    )


@buck_test(inplace=False, data_dir="actions")
async def test_dynamic_outputs(buck: Buck) -> None:
    await buck.build("//dynamic:")


@buck_test(inplace=False, data_dir="args")
async def test_args(buck: Buck) -> None:
    result = await buck.build("//:bin")
    output = result.get_build_report().output_for_target("//:bin")
    assert (
        output.read_text().rstrip()
        == "<foo_compiler> -- <foo_compiler_flags>\nlib1 -- this is lib1\nlib2 -- this is lib2"
    )


@buck_test(inplace=False, data_dir="modify")
async def test_modify_genrule(buck: Buck) -> None:
    result = await buck.build("//:writer")
    output = result.get_build_report().output_for_target("root//:writer")
    assert Path(output).read_text() == "HELLO\n"

    # Change "HELLO" in TARGETS to "GOODBYE"
    with fileinput.input(buck.cwd / "TARGETS.fixture", inplace=True) as f:
        for line in f:
            print(line.replace("HELLO", "GOODBYE"), end="")

    result = await buck.build("//:writer")
    output = result.get_build_report().output_for_target("root//:writer")
    assert Path(output).read_text() == "GOODBYE\n"


@buck_test(inplace=False, data_dir="modify")
async def test_modify_directory(buck: Buck) -> None:
    # Test for the bug reported in T99593442
    os.mkdir(buck.cwd / "a_dir")
    with open(buck.cwd / "a_dir" / "test.txt", "w") as file:
        file.write("test")
    await buck.build("//:writer")
    # Remove a directory, and change a file, so the file gets spotted,
    # and we'd better note that the directory no longer exists
    os.remove(buck.cwd / "a_dir" / "test.txt")
    os.rmdir(buck.cwd / "a_dir")
    await buck.build("//:writer")


@buck_test(inplace=False, data_dir="bql/simple")
async def test_query_rdeps(buck: Buck) -> None:
    result = await buck.query("""rdeps(root//bin:the_binary, //lib:file1)""")
    assert result.stdout == "root//lib:file1\nroot//lib:lib1\nroot//bin:the_binary\n"

    result = await buck.query("""rdeps(root//bin:the_binary, //lib:file1, 0)""")
    assert result.stdout == "root//lib:file1\n"

    result = await buck.query("""rdeps(root//bin:the_binary, //lib:file1, 1)""")
    assert result.stdout == "root//lib:file1\nroot//lib:lib1\n"

    result = await buck.query("""rdeps(root//bin:the_binary, //lib:file1, 100)""")
    assert result.stdout == "root//lib:file1\nroot//lib:lib1\nroot//bin:the_binary\n"


@buck_test(inplace=False, data_dir="bql/simple")
async def test_targets_recursive(buck: Buck) -> None:
    result = await buck.targets("--json", "ignored/...")
    assert json.loads(result.stdout) == []

    await expect_failure(buck.targets("--json", "nonexistent/..."))


@buck_test(inplace=False, data_dir="bql/simple")
async def test_target_hashing_accepts_backreferencing_relative_paths(
    buck: Buck,
) -> None:
    await buck.targets(
        ":the_binary",
        "--show-target-hash",
        "--target-hash-file-mode=paths_only",
        "--target-hash-modified-paths=../.buckconfig",
        rel_cwd=Path("bin"),
    )
    # Paths outside of the project still fail
    await expect_failure(
        buck.targets(
            ":the_binary",
            "--show-target-hash",
            "--target-hash-file-mode=paths_only",
            "--target-hash-modified-paths=../.buckconfig",
        ),
        stderr_regex="expected a normalized path but got an un-normalized path instead",
    )


def truncate(x: str, limit: int) -> str:
    if len(x) <= limit:
        return x
    else:
        return x[: limit // 2] + " <<TRUNCATED>> " + x[-(limit // 2) :]


def print_occurences_msg(
    needle: str, haystack: str, occurrences: int, success: bool
) -> None:
    OUTPUT_LIMIT = 10000
    # Hacky way to actually make sure we print the full output when a string
    # does not appear the correct number of times.
    assert success, "Expected to find {} occurrences of `{}` in `{}`".format(
        occurrences, needle, truncate(repr(haystack), OUTPUT_LIMIT)
    )


def assert_occurrences(needle: str, haystack: str, occurrences: int) -> None:
    print_occurences_msg(
        needle, haystack, occurrences, haystack.count(needle) == occurrences
    )


def assert_occurrences_regex(needle: str, haystack: str, occurrences: int) -> None:
    print_occurences_msg(
        needle,
        haystack,
        occurrences,
        len(re.findall(needle, haystack, re.MULTILINE)) == occurrences,
    )


@buck_test(inplace=False, data_dir="pass")
async def test_success_message_printed(buck: Buck) -> None:
    results = await buck.build("//:abc", "--console=simplenotty")

    assert "BUILD SUCCEEDED" in results.stderr

    results = await buck.build("//:abc", "--console=simpletty")

    assert_occurrences("\x1b[38;5;10mBUILD SUCCEEDED\x1b[39m", results.stderr, 1)

    results = await buck.build("//:abc", "--console=super")

    assert_occurrences("\x1b[38;5;10mBUILD SUCCEEDED\x1b[39m", results.stderr, 1)


@buck_test(inplace=False, data_dir="failing")
async def test_multiple_errors_print_with_simple_console(buck: Buck) -> None:
    e = await expect_failure(
        buck.build(
            "--console=simple",
            "//:foo",
            "//:bar",
            "//:using_dir",
        )
    )

    # Make sure that streamed events come back
    assert_occurrences_regex("(Build ID|Buck UI):", e.stderr, 1)
    assert_occurrences("RE Session: ", e.stderr, 1)
    assert_occurrences_regex("^BUILD FAILED", e.stderr, 1)

    execution_error = "Action failed: {} (<unspecified>) (bin_false)"
    assert_occurrences(execution_error.format("root//:foo"), e.stderr, 2)
    assert_occurrences(execution_error.format("root//:bar"), e.stderr, 2)

    exit_code = "(Local|Remote) command returned non-zero exit code 1"
    assert_occurrences_regex(exit_code, e.stderr, 6)

    build_error = "Failed to build artifact(s) for '{} (<unspecified>)'"
    assert_occurrences(build_error.format("root//:foo"), e.stderr, 1)
    assert_occurrences(build_error.format("root//:bar"), e.stderr, 1)
    # TODO(nmj): Remove this comment
    # assert_occurrences_regex("getting metadata for.*a_dir`", e.stderr, 1)

    e = await expect_failure(buck.build("--console=simple", "//:non_existent"))

    assert_occurrences_regex("^BUILD FAILED", e.stderr, 1)
    assert_occurrences(
        "Unknown target `non_existent` from package `root//`", e.stderr, 1
    )


@buck_test(inplace=False, data_dir="failing")
async def test_multiple_errors_print_with_super_console(buck: Buck) -> None:
    e = await expect_failure(
        buck.build(
            "--console=super",
            "//:foo",
            "//:bar",
            "//:using_dir",
        )
    )

    # See https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit for how to decode this
    DARK_RED = re.escape("\033[38;5;1m")

    execution_error = "Action failed: "
    assert_occurrences_regex(execution_error, e.stderr, 3)

    exit_code = "(Local|Remote) command returned non-zero exit code 1"
    assert_occurrences_regex(exit_code, e.stderr, 3)

    # These will eventually be red.
    build_error = "Failed to build artifact(s) for '{} (<unspecified>)'"
    assert_occurrences(build_error.format("root//:foo"), e.stderr, 1)
    assert_occurrences(build_error.format("root//:bar"), e.stderr, 1)
    # TODO(nmj): Remove this comment
    # assert_occurrences_regex("getting metadata for.*a_dir`", e.stderr, 1)

    assert_occurrences("\x1b[38;5;1mBUILD FAILED\x1b[39m", e.stderr, 1)

    e = await expect_failure(buck.build("--console=super", "//:non_existent"))

    target_error = f"{DARK_RED}Unknown target `non_existent` from package `root//`"
    assert_occurrences("\x1b[38;5;1mBUILD FAILED\x1b[39m", e.stderr, 1)
    assert_occurrences_regex(target_error, e.stderr, 1)


@buck_test(inplace=False, data_dir="transitive_sets")
async def test_transitive_sets(buck: Buck) -> None:
    rule = "//:bar"
    report = await buck.build(rule)
    out = report.get_build_report().output_for_target(rule)
    out = out.read_text()
    out = [line.strip() for line in out.strip().split("\n")]
    assert out == ["bar", "foo", "foo2", "foo1"]


@buck_test(inplace=False, data_dir="pass")
async def test_stderr_is_printed_for_successful_actions(buck: Buck) -> None:
    no_color_text = "warning on stderr no color"
    # Support '\r\n' which is printed on Windows.
    simple_color_stripped = "^warning on stderr with color\\r?$"
    # Make sure we reset the terminal after printing.
    simple_color = "\x1b[33mwarning on stderr with color\x1b[0m"
    # Make sure we reset the terminal after printing. Color is represented
    # slightly differently (33m vs 38;5;3m) because of how parsed / sanitized
    # colors are stored. Those are the same color, though.
    super_color = "\x1b[38;5;3mwarning on stderr with color\x1b[39m"

    superconsole_stderr_line = "stderr for {} (printer writing_stderr)"
    simpleconsole_stderr_line = "stderr:"

    # By default stderr should not be relayed on success w/o the -v2 or higher flag.

    res = await buck.build("--console=simplenotty", "//:print_stderr_simple_notty")

    assert_occurrences(no_color_text, res.stderr, 0)
    assert_occurrences_regex(simple_color_stripped, res.stderr, 0)
    assert_occurrences(simpleconsole_stderr_line, res.stderr, 0)

    res = await buck.build("--console=simpletty", "//:print_stderr_simple_tty")

    assert_occurrences(no_color_text, res.stderr, 0)
    assert_occurrences(simple_color, res.stderr, 0)
    assert_occurrences(simpleconsole_stderr_line, res.stderr, 0)

    res = await buck.build("--console=super", "//:print_stderr_super")

    assert_occurrences(no_color_text, res.stderr, 0)
    assert_occurrences(super_color, res.stderr, 0)
    assert_occurrences(
        superconsole_stderr_line.format("root//:print_stderr_super"), res.stderr, 0
    )

    res = await buck.build(
        "-v5", "--console=simplenotty", "//:v_print_stderr_simple_notty"
    )

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences_regex(simple_color_stripped, res.stderr, 1)

    res = await buck.build("-v5", "--console=simpletty", "//:v_print_stderr_simple_tty")

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences(simple_color, res.stderr, 1)
    assert_occurrences(simpleconsole_stderr_line, res.stderr, 1)

    res = await buck.build("-v5", "--console=super", "//:v_print_stderr_super")

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences(super_color, res.stderr, 1)
    assert_occurrences(
        superconsole_stderr_line.format("root//:v_print_stderr_super"), res.stderr, 1
    )

    # Things that print all the time should print w/o the verbosity flag, or with it

    res = await buck.build(
        "--console=simplenotty", "//:always_print_stderr_simple_notty"
    )

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences_regex(simple_color_stripped, res.stderr, 1)
    assert_occurrences(simpleconsole_stderr_line, res.stderr, 1)

    res = await buck.build("--console=simpletty", "//:always_print_stderr_simple_tty")

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences(simple_color, res.stderr, 1)
    assert_occurrences(simpleconsole_stderr_line, res.stderr, 1)

    res = await buck.build("--console=super", "//:always_print_stderr_super")

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences(super_color, res.stderr, 1)
    assert_occurrences(
        superconsole_stderr_line.format("root//:always_print_stderr_super"),
        res.stderr,
        1,
    )

    res = await buck.build(
        "-v5", "--console=simplenotty", "//:v_always_print_stderr_simple_notty"
    )

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences_regex(simple_color_stripped, res.stderr, 1)
    assert_occurrences(simpleconsole_stderr_line, res.stderr, 1)

    res = await buck.build(
        "-v5", "--console=simpletty", "//:v_always_print_stderr_simple_tty"
    )

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences(simple_color, res.stderr, 1)
    assert_occurrences(simpleconsole_stderr_line, res.stderr, 1)

    res = await buck.build("-v5", "--console=super", "//:v_always_print_stderr_super")

    assert_occurrences(no_color_text, res.stderr, 1)
    assert_occurrences(super_color, res.stderr, 1)
    assert_occurrences(
        superconsole_stderr_line.format("root//:v_always_print_stderr_super"),
        res.stderr,
        1,
    )


@buck_test(inplace=False, data_dir="flagfiles")
async def test_flagfiles_are_located_correctly(buck: Buck) -> None:
    out = await buck.build("@mode/dev", "cell//subdir:simple", rel_cwd=Path("cell"))

    build_report = out.get_build_report()
    output = build_report.output_for_target("cell//subdir:simple")
    assert output.read_text().rstrip() == "overridden"

    out = await buck.build("@//mode/dev", "cell//subdir:simple", rel_cwd=Path("cell"))

    build_report = out.get_build_report()
    output = build_report.output_for_target("cell//subdir:simple")
    assert output.read_text().rstrip() == "overridden"

    # Make sure that relative paths are resolved against the cell root
    # (determined from project root + cwd) if they're not found relative
    # to cwd
    out = await buck.build(
        "@mode/dev", "cell//subdir:simple", rel_cwd=Path("cell/subdir")
    )

    build_report = out.get_build_report()
    output = build_report.output_for_target("cell//subdir:simple")
    assert output.read_text().rstrip() == "overridden"
    assert (
        "`@mode/dev` was specified, but not found. Using file at `//mode/dev`.\n"
        "This behavior is being deprecated. Please use `@//mode/dev` instead"
    ) in out.stderr

    out = await buck.build("@cell//mode/dev", "cell//subdir:simple")

    build_report = out.get_build_report()
    output = build_report.output_for_target("cell//subdir:simple")
    assert output.read_text().rstrip() == "overridden"

    await expect_failure(
        buck.build("@cell/mode/missing", "cell//subdir:simple"),
        stderr_regex="Unable to read flag file at `cell/mode/missing`",
    )


@buck_test(inplace=False, data_dir="early_action_cutoff")
async def test_early_action_cutoff(buck: Buck, tmpdir: LocalPath) -> None:
    sentinel = Path(str(tmpdir)) / "sentinel"
    sentinel.touch()

    # This action is going to check that the file pointed at by "sentinel"
    # exists. Point it at a valid file.

    with open(buck.cwd / "sentinel", "w", encoding="utf-8") as f:
        f.write(str(sentinel))

    await buck.build("//:check")

    ## Now, invalidate the action, and remove the underlying sentinel file. If
    # :check executes now, it'll fail.

    with open(buck.cwd / "src", "w", encoding="utf-8") as f:
        f.write("TEXT2")

    sentinel.unlink()

    # Run it to find out

    await buck.build("//:check")


@buck_test(inplace=False, data_dir="modify_file_during_build")
async def test_modify_file_during_build(buck: Buck, tmpdir: LocalPath) -> None:
    # We need to write some random stuff to the file first so that Buck will
    # have to attempt to upload it to RE (which will fail because by that time
    # we will have overwritten it with other content).
    with open(buck.cwd / "text", "w", encoding="utf-8") as f:
        f.write(random_string())

    await expect_failure(
        buck.build("//:check"),
        stderr_regex="modified files while the build was in progress",
    )


@buck_test(inplace=False, data_dir="nested_subtargets")
async def test_build_nested_subtargets(buck: Buck, tmpdir: LocalPath) -> None:
    result = await buck.build(
        "//:nested[sub][nested_sub]",
    )
    build_report = result.get_build_report()

    output = build_report.output_for_target("//:nested", "sub|nested_sub")

    assert output.read_text().rstrip() == "foo_content"
    asserts.assert_not_executable(output)


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_hybrid_executor_threshold(buck: Buck) -> None:
    await buck.build("root//executor_threshold_tests/...")
    out = await read_what_ran(buck)

    executors = {line["identity"]: line["reproducer"]["executor"] for line in out}
    expected = {
        "root//executor_threshold_tests:big (<unspecified>) (head)": "Local",
        "root//executor_threshold_tests:cp_big (<unspecified>) (cp)": "Local",
        "root//executor_threshold_tests:small (<unspecified>) (head)": "Local",
        "root//executor_threshold_tests:cp_small (<unspecified>) (cp)": "Re",
    }
    assert executors == expected


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_hybrid_executor_fallbacks(buck: Buck) -> None:
    # Those work as they are allowed to fallback:
    await buck.build(
        "root//executor_fallback_tests:local_only",
        "root//executor_fallback_tests:local_only_full_hybrid",
    )

    # This one doesn't:
    await expect_failure(
        buck.build(
            "root//executor_fallback_tests:local_only_no_fallback",
        )
    )


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_hybrid_executor_logging(buck: Buck) -> None:
    await buck.build(
        "root//executor_fallback_tests:local_only",
    )

    log = (await buck.log("show")).stdout.strip().splitlines()
    commands = None

    for line in log:
        commands = commands or json_get(
            line,
            "Event",
            "data",
            "SpanEnd",
            "data",
            "ActionExecution",
            "commands",
        )

    assert commands is not None
    assert len(commands) == 2
    assert commands[0]["details"]["exit_code"] != 0
    assert commands[0]["status"] == {"Failure": {}}
    assert commands[1]["details"]["exit_code"] == 0
    assert commands[1]["status"] == {"Success": {}}


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_prefer_local(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "root//executor_fallback_tests:local_only_no_fallback",
        )
    )

    await buck.build(
        "root//executor_fallback_tests:local_only_no_fallback", "--prefer-local"
    )


@buck_test(inplace=False, data_dir="execution_platforms")
async def test_local_only(buck: Buck) -> None:
    await expect_failure(
        buck.build(
            "root//executor_fallback_tests:local_only_no_fallback",
        )
    )

    await buck.build(
        "root//executor_fallback_tests:local_only_no_fallback", "--local-only"
    )


@buck_test(inplace=False, data_dir="toolchain_deps")
async def test_toolchain_deps(buck: Buck) -> None:
    # This test builds two targets, both with the same `default_target_platform` platform
    # but which should resolve to different execution platforms because of toolchain deps.
    # Both targets still get configured with the `default_target_platform` of release.
    #
    # The Python toolchain works on Windows/Linux, but we prefer Linux as an exec platform.
    # The ASIC toolchain only works on Windows, so `python_and_asic` (which does both) must
    # pick Windows for Python as well.
    result = await buck.build("root//tests:python_and_asic", "root//tests:python_only")
    python_and_asic = (
        result.get_build_report()
        .output_for_target("root//tests:python_and_asic")
        .read_text()
    )
    python_only = (
        result.get_build_report()
        .output_for_target("root//tests:python_only")
        .read_text()
    )
    # If any of the selects get resolved incorrectly, the toolchain binaries below will change.
    assert python_and_asic == "python_release_windows asic\n"
    assert python_only == "python_release_linux\n"

    await buck.build("root//...", "--target-platforms=root//config:platform_windows")

    # Check we get the toolchain dependencies in uquery and cquery
    result = await buck.uquery("deps(//toolchains:python)")
    assert "//toolchains:compile_python_release_linux\n" in result.stdout
    assert "//toolchains:python_debug\n" in result.stdout
    result = await buck.cquery(
        "deps(//toolchains:python)", "--target-platforms=root//config:platform_linux"
    )
    assert "//toolchains:compile_python_release_linux " in result.stdout
    assert "//toolchains:python_debug " not in result.stdout


@buck_test(inplace=False, data_dir="prelude_import")
async def test_prelude_imported_once(buck: Buck) -> None:
    # See the comments in the relevant targets files: they explain how this
    # test works.
    await buck.build("cell1//...", "cell2//...")


if watchman_dependency_linux_only():
    # This test appears to be flaky on MacOS, where Watchman keeps hitting
    # errors :( It has never failed on Linux, which suggests that the problem
    # might indeed have to do with Watchman (this test does a lot of
    # rebuilding).

    def touch(buck: Buck, name: str) -> None:
        """
        Modify the marker in one of our files
        """
        with open(buck.cwd / name, "r", encoding="utf-8") as f:
            text = f.read()

        with open(buck.cwd / name, "w", encoding="utf-8") as f:
            f.write(text.replace("__MARKER__", "rebuild"))

    @buck_test(inplace=False, data_dir="dep_files")
    async def test_dep_files(buck: Buck) -> None:
        await buck.build("app/...")
        await expect_exec_count(buck, 1)

        touch(buck, "app/app.h")
        await buck.build("app/...")
        await expect_exec_count(buck, 1)

        touch(buck, "app/app.c")
        await buck.build("app/...")
        await expect_exec_count(buck, 1)

        touch(buck, "app/other.h")
        await buck.build("app/...")
        await expect_exec_count(buck, 0)

    # This test tombstones the hash of the dep file produced by this action.
    # This might be a bit fragile...
    @buck_test(inplace=False, data_dir="dep_files")
    @env(
        "BUCK2_TEST_TOMBSTONED_DIGESTS",
        "4de8e222928d0f0ea6b2d1d5a4fc1c1731752f5e:78",
    )
    async def test_dep_files_ignore_missing_digests(buck: Buck) -> None:
        await buck.build("app/...")
        await expect_exec_count(buck, 1)

        touch(buck, "app/other.h")
        await buck.build("app/...")
        await expect_exec_count(buck, 1)

    @buck_test(inplace=False, data_dir="invalid_dep_files")
    async def test_invalid_dep_files(buck: Buck) -> None:
        await buck.build("//:lazy")
        await expect_failure(
            buck.build("//:lazy", "-c", "test.seed=123"),
            stderr_regex="Invalid line encountered in dep file",
        )

        await buck.debug("flush-dep-files")
        await buck.build("//:lazy")

        await expect_failure(
            buck.build("//:eager", "--eager-dep-files"),
            stderr_regex="Invalid line encountered in dep file",
        )

    @buck_test(inplace=False, data_dir="hash_all_commands")
    async def test_hash_all_commands(buck: Buck) -> None:
        # Expecting a rebuild since the command wasn't hashed previously.
        await buck.build(
            "//:test",
            "-c",
            "test.seed=123",
        )
        await expect_exec_count(buck, 1)

        # No longer expecting a rebuild.
        await buck.build(
            "//:test",
            "-c",
            "test.seed=456",
        )
        await expect_exec_count(buck, 0)


@buck_test(inplace=False, data_dir="target_aliases")
async def test_target_aliases(buck: Buck) -> None:
    await buck.targets("alias")
    await buck.cquery("deps(alias)")

    await buck.targets("chain")
    await buck.cquery("deps(chain)")

    # `bad` should pass because this does not check target existence, only
    # resolves aliases.
    await buck.targets("--resolve-alias", "alias", "chain", "bad")


@buck_test(inplace=False, data_dir="build_providers")
async def test_build_providers(buck: Buck) -> None:
    await buck.build(
        "//:target",
        "-c",
        "build_report.unstable_include_other_outputs=true",
        "--build-default-info",
        "--skip-run-info",
        "--skip-test-info",
        "--build-report",
        "report",
    )

    outputs = read_all_outputs(buck, "report")
    assert any("/build" in o for o in outputs)
    assert all("/run" not in o for o in outputs)
    assert all("/test" not in o for o in outputs)

    await buck.build(
        "//:target",
        "-c",
        "build_report.unstable_include_other_outputs=true",
        "--skip-default-info",
        "--build-run-info",
        "--skip-test-info",
        "--build-report",
        "report",
    )

    outputs = read_all_outputs(buck, "report")
    assert all("/build" not in o for o in outputs)
    assert any("/run" in o for o in outputs)
    assert all("/test" not in o for o in outputs)

    await buck.build(
        "//:target",
        "-c",
        "build_report.unstable_include_other_outputs=true",
        "--skip-default-info",
        "--skip-run-info",
        "--build-test-info",
        "--build-report",
        "report",
    )

    outputs = read_all_outputs(buck, "report")
    assert all("/build" not in o for o in outputs)
    assert all("/run" not in o for o in outputs)
    assert any("/test" in o for o in outputs)


@buck_test(inplace=False, data_dir="http_deferral")
@env(
    "BUCK2_TEST_INJECTED_MISSING_DIGESTS",
    "1a45666759704bf08fc670aa96118a0415c470fc:221",
)
@env("BUCK2_TEST_DISABLE_CACHING", "true")
async def test_http_deferral_uploads(buck: Buck) -> None:
    await buck.build("//:target")


@buck_test(inplace=False, data_dir="out")
async def test_out_single_default_output(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as out:
        output = os.path.join(out, "output")
        await buck.build("//:a", "--out", output)
        with open(output) as readable:
            assert readable.read() == "a\n"


@buck_test(inplace=False, data_dir="out")
async def test_out_single_default_output_to_dir(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as out:
        await buck.build("//:a", "--out", out)
        with open(Path(out) / "a.txt") as readable:
            assert readable.read() == "a\n"


@buck_test(inplace=False, data_dir="out")
async def test_out_no_outputs(buck: Buck) -> None:
    with tempfile.NamedTemporaryFile("w") as out:
        await expect_failure(
            buck.build("//:none", "--out", out.name),
            stderr_regex="produced zero default outputs",
        )


@buck_test(inplace=False, data_dir="out")
async def test_out_multiple_outputs(buck: Buck) -> None:
    with tempfile.NamedTemporaryFile("w") as out:
        await expect_failure(
            buck.build("//:ab", "--out", out.name),
            stderr_regex="produced 2 outputs",
        )


@buck_test(inplace=False, data_dir="out")
async def test_out_multiple_targets(buck: Buck) -> None:
    with tempfile.NamedTemporaryFile("w") as out:
        await expect_failure(
            buck.build("//:a", "//:b", "--out", out.name),
            stderr_regex="command built multiple top-level targets",
        )


@buck_test(inplace=False, data_dir="out")
async def test_out_directory(buck: Buck) -> None:
    with tempfile.TemporaryDirectory() as out:
        await buck.build("//:dir", "--out", out)
        assert (Path(out) / "b.txt").exists()
        assert (Path(out) / "nested_dir" / "a.txt").exists()


@buck_test(inplace=False, data_dir="out")
async def test_out_stdout_multiple(buck: Buck) -> None:
    result = await buck.build("//:a", "//:b", "--out", "-")

    # The e2e test runner adds a `--print-build-report` flag in order to be able
    # to parse out failures. In normal usage of `--out -` there wouldn't be this
    # extra line of JSON on the stdout, we'd _just_ get the requested outputs.
    build_report, stdout = result.stdout.split("\n", 1)
    assert build_report.startswith("{")

    assert stdout == "a\nb\n" or stdout == "b\na\n"


@buck_test(inplace=False, data_dir="out")
async def test_out_stdout_none(buck: Buck) -> None:
    await buck.build("--out", "-")


@buck_test(inplace=False, data_dir="out")
async def test_out_stdout_directory(buck: Buck) -> None:
    await expect_failure(
        buck.build("//:dir", "--out", "-"),
        stderr_regex="produces a default output that is a directory, and cannot be sent to stdout",
    )


@buck_test(inplace=False, data_dir="no_output")
async def test_no_output(buck: Buck) -> None:
    results = await buck.build("//:none")

    assert "BUILD SUCCEEDED" in results.stderr
    assert "does not have any outputs: building it does nothing" in results.stderr


@buck_test(inplace=False, data_dir="no_output")
async def test_no_output_wildcard(buck: Buck) -> None:
    results = await buck.build("//...")

    assert "BUILD SUCCEEDED" in results.stderr
    assert "does not have any outputs: building it does nothing" not in results.stderr


@buck_test(inplace=False, data_dir="critical_path")
@env("BUCK2_TEST_DISABLE_CACHING", "true")
async def test_critical_path(buck: Buck) -> None:
    await buck.build("//:step_3")
    log = (await buck.log("show")).stdout.strip().splitlines()

    critical_path = None

    for line in log:
        critical_path = json_get(
            line,
            "Event",
            "data",
            "Instant",
            "data",
            "BuildGraphInfo",
            "critical_path",
        )

        if critical_path is not None:
            break

    assert critical_path is not None, "No critical path in log"

    steps = [entry["action_name"] for entry in critical_path]
    assert len(steps) == 4
    assert "root//:step_0" in steps[0]
    assert "root//:step_1" in steps[1]
    assert "root//:step_2" in steps[2]
    assert "root//:step_3" in steps[3]


@buck_test(inplace=False, data_dir="projected_artifacts")
@pytest.mark.parametrize(
    "target",
    [
        # Check building the whole thing
        "//...",
        # Check building just one target, which may reveal bugs if things are
        # materialized differently when a projected target uses them.
        "//:check_c_b_local",
    ],
)
async def test_projected_artifacts(buck: Buck, target: str) -> None:
    await buck.build(target)


@buck_test(inplace=False, data_dir="symlinks")
async def test_symlinks(buck: Buck) -> None:
    # We want to check in a symlink but given Buck is running this and symlinks
    # do not exist we need to put it back and make it be an actual symlink.
    shutil.rmtree(buck.cwd / "src/link")
    os.symlink("../dir", buck.cwd / "src/link")

    await buck.build("//:cp")
    expect_exec_count(buck, 1)

    await buck.build("//:cp")
    expect_exec_count(buck, 0)

    with open(buck.cwd / "dir/file", "w") as file:
        file.write("GOODBYE\n")

    # This isn't really behavior  we want to guarantee and we'd rather users
    # don't use symlinks, but this is very observable (and it's not worse than
    # just reading the files then pretending they are never used!)
    await buck.build("//:cp")
    expect_exec_count(buck, 1)


@buck_test(inplace=False, data_dir="upload_all_actions")
async def test_upload_all_actions(buck: Buck) -> None:
    with open(buck.cwd / "src", "w") as src:
        src.write(random_string())

    # This action includes `src` and is forced to run locally. This means RE
    # can never have seen it (and we'll check that later by asserting there is
    # only 1 cache query, excluding local actions).
    await buck.build("//:cp", "--upload-all-actions")

    what_ran = await read_what_ran(
        buck, "--emit-cache-queries", "--skip-local-executions"
    )
    assert len(what_ran) == 1

    # Now, download the action. This will succeed only if we uploaded it.
    digest = what_ran[0]["reproducer"]["details"]["digest"]
    subprocess.check_call([os.environ["RECLI"], "cas", "download-action", digest])


@buck_test(inplace=False, data_dir="symlinks")
async def test_symlinks_redirection(buck: Buck) -> None:
    shutil.rmtree(buck.cwd / "src/link")
    os.symlink("../dir", buck.cwd / "src/link")

    await buck.build("//:cp")
    await expect_exec_count(buck, 1)

    await buck.build("//:cp")
    await expect_exec_count(buck, 0)

    # We change the symlink which should invalidate all files depending on it
    os.remove(buck.cwd / "src/link")
    os.symlink("../dir2", buck.cwd / "src/link")

    await buck.build("//:cp")
    await expect_exec_count(buck, 1)


@buck_test(inplace=False, data_dir="symlinks")
async def test_symlinks_external(buck: Buck) -> None:
    shutil.rmtree(buck.cwd / "ext/link")
    top_level = tempfile.mkdtemp()

    os.mkdir(os.path.join(top_level, "nested1"))
    os.mkdir(os.path.join(top_level, "nested2"))
    with open(os.path.join(top_level, "nested1", "file"), "w") as f:
        f.write("HELLO")
    with open(os.path.join(top_level, "nested2", "file"), "w") as f:
        f.write("GOODBYE")

    os.symlink(os.path.join(top_level, "nested1"), buck.cwd / "ext/link")

    await buck.build("//:ext")
    await expect_exec_count(buck, 1)

    await buck.build("//:ext")
    await expect_exec_count(buck, 0)

    os.remove(buck.cwd / "ext/link")
    os.symlink(os.path.join(top_level, "nested2"), buck.cwd / "ext/link")

    await buck.build("//:ext")
    await expect_exec_count(buck, 1)


async def expect_exec_count(buck: Buck, n: int) -> None:
    out = await read_what_ran(buck)
    assert len(out) == n, "unexpected actions: %s" % (out,)


async def read_what_ran(buck: Buck, *args) -> typing.List[typing.Dict[str, typing.Any]]:
    out = await buck.debug("what-ran", "--format", "json", *args)
    out = [line.strip() for line in out.stdout.splitlines()]
    out = [json.loads(line) for line in out if line]
    return out


def read_all_outputs(buck: Buck, report: str) -> typing.List[str]:
    ret = []

    with open(buck.cwd / report) as f:
        report = json.load(f)
        for _target, state in report["results"].items():
            ret.extend(state["outputs"].get("DEFAULT", []))
            ret.extend(state["other_outputs"].get("DEFAULT", []))

    return ret


def json_get(data, *key):
    data = json.loads(data)

    for k in key:
        data = data.get(k)
        if data is None:
            break

    return data


def random_string():
    return "".join(random.choice(string.ascii_lowercase) for i in range(256))
