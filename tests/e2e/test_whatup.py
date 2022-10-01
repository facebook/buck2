import json
import sys
import tempfile

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test, env


@buck_test(inplace=True)
async def test_whatup_command(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/whatup:simple_build"
    args = [target]
    if sys.platform == "win32":
        args.append("@mode/win")
    await buck.build(*args)

    log = (await buck.log("show")).stdout.strip()
    log_file = tempfile.NamedTemporaryFile(
        suffix=".json-lines", mode="w+", delete=False
    )
    # Truncate log when analysis started
    with log_file as f:
        lines = log.splitlines()
        for line in lines:
            f.write(line + "\n")
            if "AnalysisStage" in line:
                break
        f.close()

    ext = await buck.log("whatup", "--path", log_file.name)
    assert "running analysis" in ext.stdout


@buck_test(inplace=True)
@env("BUCK2_TEST_DISABLE_CACHING", "true")
async def test_whatup_after_command(buck: Buck) -> None:
    target = "fbcode//buck2/tests/targets/whatup:long_build"
    args = [target, "--local-only"]
    if sys.platform == "win32":
        args.append("@mode/win")
    await buck.build(*args)

    # Get event log
    log = (await buck.log("show")).stdout.strip()
    elapsed = [0, 0]

    # Get first timestamp
    lines = log.splitlines()
    first_event = json.loads(lines[1])
    first_timestamp = first_event["Event"]["timestamp"]
    # Get timestamp where rule execution starts
    for line in lines:
        if "Execute" in line:
            event = json.loads(line)
            # Calculate elapsed seconds, we add 1 to give some padding in order to catch the open span
            elapsed[0] = (event["Event"]["timestamp"][0] - first_timestamp[0]) + 1
            # Calculate elapsed millliseconds
            elapsed[1] = (
                event["Event"]["timestamp"][1] - first_timestamp[1]
            ) // 1000000
            break

    # Verify rule execution appears when running whatup at that timestamp
    action_start = (elapsed[0] * 1000) + abs(elapsed[1])
    ext = (await buck.log("whatup", "--after", str(action_start))).stdout.strip()
    assert "action (genrule)" in ext
