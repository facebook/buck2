import asyncio
import asyncio.subprocess
import os
import sys
from pathlib import Path

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


@buck_test(inplace=True)
async def test_python_par_supports_pdb_debugging(buck: Buck) -> None:
    args = [
        "fbcode//buck2/tests/targets/rules/python/hello_world:welcome",
        "--show-full-output",
    ]
    if sys.platform == "darwin":
        args.append("@mode/mac")
    result = await buck.build(*args)
    output_dict = result.get_target_to_build_output()
    for _target, output in output_dict.items():
        gen_folder = output[: output.find("/gen/") + 4]
        symlink = Path(
            gen_folder,
            # v1:   buck2/tests/targets/rules/python/hello_world/welcome.par
            "fbcode/buck2/tests/targets/rules/python/hello_world/welcome.par",
        )
        assert os.path.exists(symlink)

        # Run the resulting binary under pdb
        proc = await asyncio.create_subprocess_exec(
            symlink,
            stdin=asyncio.subprocess.PIPE,
            stdout=asyncio.subprocess.PIPE,
            stderr=asyncio.subprocess.PIPE,
            env={
                "PYTHONDEBUGWITHPDB": "1",
                "PYTHONPDBINITIALCOMMANDS": "break hello_world.py:2|continue",
            },
        )

        # Send the continue command to Pdb's command prompt
        stdout, _stderr = await proc.communicate(input=b"continue\n")

        # pdb was started
        assert b"(Pdb)" in stdout

        # pdb hit a breakpoint
        assert b"Breakpoint 1 at" in stdout
