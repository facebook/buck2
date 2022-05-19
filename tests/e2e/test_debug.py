import json
import os.path
import sys
from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_replay(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/cxx/hello_world:welcome")
    replay = await buck.debug("replay")
    assert "welcome (cxx_link)" in replay.stderr


@buck_test(inplace=True)
async def test_what_ran(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/cxx/hello_world:welcome")
    out = await buck.debug("what-ran")
    assert "welcome (cxx_link)" in out.stdout

    await buck.test("fbcode//buck2/tests/targets/rules/sh_test:test")
    out = await buck.debug("what-ran")
    assert "sh_test/test.py arg1" in out.stdout


@buck_test(inplace=True)
async def test_what_ran_json_target_without_explicit_test_cases(buck: Buck) -> None:
    await buck.test("fbcode//buck2/tests/targets/rules/sh_test:test")
    out = await buck.debug("what-ran", "--format", "json")
    out = [line.strip() for line in out.stdout.splitlines()]
    out = [json.loads(line) for line in out if line]
    assert len(out) == 1, "out should have 1 line: `{}`".format(out)

    repro = out[0]
    assert repro["reason"] == "test.run"
    assert repro["identity"] == "buck2/tests/targets/rules/sh_test:test"
    assert repro["reproducer"]["executor"] == "Local"
    assert repro["reproducer"]["details"]["command"][1] == "arg1"
    assert repro["extra"]["testcases"] == []


# builds targets in an fbcode target configuration, unsupported on mac RE workers
def fbcode_linux_only() -> bool:
    return sys.platform == "linux"


if fbcode_linux_only():

    @buck_test(inplace=True)
    async def test_what_ran_json_target_with_test_cases(buck: Buck) -> None:
        await buck.test("fbcode//buck2/tests/targets/rules/go/test:test")
        out = await buck.debug("what-ran", "--format", "json")
        out = [line.strip() for line in out.stdout.splitlines()]
        out = [json.loads(line) for line in out if line]
        out = [repro for repro in out if repro.get("reason", "").startswith("test.")]
        assert len(out) == 2, "out should have 2 test lines: `{}`".format(out)

        repros = {repro["reason"]: repro for repro in out}

        # test discovery
        discovery = repros["test.discovery"]
        assert discovery["identity"] == "buck2/tests/targets/rules/go/test:test"
        assert discovery["reproducer"]["executor"] == "Local"

        # test running
        repro = repros["test.run"]
        assert repro["reason"] == "test.run"
        assert repro["identity"] == "buck2/tests/targets/rules/go/test:test"
        assert repro["reproducer"]["executor"] == "Local"
        assert repro["extra"]["testcases"] == ["TestFoo"]


@buck_test(inplace=True)
async def test_what_ran_local(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/genrule:mktemp")
    out = await buck.debug("what-ran")

    assert "\tlocal\t" in out.stdout
    assert "\tre\t" not in out.stdout


@buck_test(inplace=True)
async def test_last_log(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/rules/cxx/hello_world:welcome")
    out = await buck.debug("last-log")
    path = out.stdout.strip()
    assert os.path.exists(path)
    assert "/log/" in path


@buck_test(inplace=False, data_dir="modify_deferred_materialization_deps")
async def test_debug_materialize(buck: Buck) -> None:
    result = await buck.build("//:remote_text", "--materializations=None")
    out = result.get_build_report().output_for_target(
        "root//:remote_text", rel_path=True
    )
    assert not Path(buck.cwd, out).exists()

    await buck.debug("materialize", str(out))
    assert Path(buck.cwd, out).exists()
