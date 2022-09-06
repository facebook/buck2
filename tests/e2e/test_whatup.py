import tempfile

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_whatup_command(buck: Buck) -> None:
    await buck.build("fbcode//buck2/tests/targets/whatup:simple_build")

    # Decompress log
    log = (await buck.log("show")).stdout.strip()
    log_file = tempfile.NamedTemporaryFile(suffix=".json-lines")
    # Truncate log when alaysis started
    with open(log_file.name, "w") as f:
        lines = log.splitlines()
        for line in lines:
            f.write(line + "\n")
            if "AnalysisStage" in line:
                break
        f.close()

    ext = await buck.log("whatup", "--path", log_file.name)
    assert "running analysis" in ext.stdout
