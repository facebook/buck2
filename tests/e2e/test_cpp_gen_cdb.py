import json
import platform
from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_if_windows=True)
async def test_no_quotes(buck: Buck) -> None:
    result = await buck.bxl(
        "prelude//cpp_lsp/cpp_gen_cdb.bxl:cpp_gen_cdb",
        "--",
        "--filename",
        "fbcode/buck2/tests/targets/cpp_gen_cdb/basic/src/main.cpp",
        "--os",
        platform.system().lower(),
    )
    outputs = json.loads(result.stdout)
    compdb_path = Path(outputs["compilationDatabasePath"]) / ".." / "compdb.json"

    with open(compdb_path) as f:
        commands = json.load(f)

    # check that the define is present without any shell quotes
    arguments = commands[0]["arguments"]
    assert arguments.index("-DM_FOO_BAR=1")


# TODO(marwhal): Add this back one at least one test in this file passes on Windows
@buck_test(inplace=True)
async def test_noop(buck: Buck) -> None:
    return
