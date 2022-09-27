import asyncio
import asyncio.subprocess
import os
import platform
import sys
from pathlib import Path
from typing import Dict, Optional

from xplat.build_infra.buck_e2e.api.buck import Buck
from xplat.build_infra.buck_e2e.buck_workspace import buck_test


def extract_gen_folder(output: str) -> str:
    return output[: output.find("{0}gen{0}".format(os.path.sep)) + 4]


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_if_windows=True)
async def test_python_par_supports_pdb_debugging(buck: Buck) -> None:
    args = [
        "fbcode//buck2/tests/targets/rules/python/hello_world:welcome",
        "--show-full-output",
    ]
    if sys.platform == "darwin":
        args.append("@mode/mac")
    if sys.platform == "win32":
        args.append("@mode/win")
    result = await buck.build(*args)
    output_dict = result.get_target_to_build_output()
    for _target, output in output_dict.items():
        gen_folder = extract_gen_folder(output)
        # v1:   buck2/tests/targets/rules/python/hello_world/welcome.par
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
        assert os.path.exists(symlink)

        # Run the resulting binary under pdb
        stdout = await run(
            symlink,
            env={
                "PYTHONDEBUGWITHPDB": "1",
                "PYTHONPDBINITIALCOMMANDS": "break hello_world.py:2|continue",
            },
            input=b"continue\n",
        )

        # pdb was started
        assert b"(Pdb)" in stdout

        # pdb hit a breakpoint
        assert b"Breakpoint 1 at" in stdout


# TODO(marwhal): Fix and enable on Windows
@buck_test(inplace=True, skip_if_windows=True)
async def test_python_par_decorating_main(buck: Buck) -> None:
    args = [
        "fbcode//buck2/tests/targets/rules/python/decorate_main:decorate_main",
        "--show-full-output",
    ]
    if sys.platform == "darwin":
        args.append("@mode/mac")
    if sys.platform == "win32":
        args.append("@mode/win")
    result = await buck.build(*args)
    output_dict = result.get_target_to_build_output()
    for _target, output in output_dict.items():
        gen_folder = extract_gen_folder(output)
        # v1:   buck2/tests/targets/rules/python/decorate_main/decorate_main.par
        symlink = (
            Path(gen_folder)
            / "fbcode"
            / "buck2"
            / "tests"
            / "targets"
            / "rules"
            / "python"
            / "decorate_main"
            / "decorate_main.par"
        )
        assert os.path.exists(symlink)

        # Run the resulting binary without decorating main
        stdout = await run(symlink, env={})
        assert b"Hello from original main!" in stdout

        # Run the resulting binary and decorate main
        stdout = await run(symlink, env={"PAR_MAIN_OVERRIDE": "main_wrapper"})
        # On Windows there is an extra wrapper
        if platform.system() == "Windows":
            assert b"Hello from main wrapper for __run_pex_main__!" in stdout
        else:
            assert b"Hello from main wrapper for original_main!" in stdout


# Run the binary. Returns stdout.
async def run(
    binary: Path, env: Dict[str, str], input: Optional[bytes] = None
) -> bytes:
    args = [binary]
    if platform.system() == "Windows":
        # Par can't be executed directly
        args = [sys.executable] + args
        # CreateProcess fails without SYSTEMROOT
        for required in ["SYSTEMROOT"]:
            env[required] = os.getenv(required, "")
    proc = await asyncio.create_subprocess_exec(
        *args,
        stdin=asyncio.subprocess.PIPE,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
        env=env,
    )

    stdout, _stderr = await proc.communicate(input=input)

    return stdout


# TODO(marwhal): Add this back one at least one test in this file passes on Windows
@buck_test(inplace=True)
async def test_noop(buck: Buck) -> None:
    return
