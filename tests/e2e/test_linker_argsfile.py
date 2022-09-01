import os

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_linker_argsfile_valid(buck: Buck) -> None:
    args = [
        "fbcode//buck2/tests/targets/rules/cxx/hello_world:welcome[linker.argsfile]",
        "--show-full-output",
    ]
    result = await buck.build(*args)
    output_dict = result.get_target_to_build_output()
    assert len(output_dict) == 1
    output_path = next(iter(output_dict.values()))
    # Ensure that the argsfile exists and is not empty.
    assert os.path.exists(output_path)
    assert os.path.getsize(output_path) > 0
