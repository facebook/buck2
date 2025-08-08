# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import argparse
import subprocess
import sys
import sysconfig
from pathlib import Path
from shutil import which


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--nm", type=Path, help="Path to nm tool")
    parser.add_argument("output", help="Output file path")
    args = parser.parse_args()

    nm: Path | None = args.nm
    if nm is None:
        maybe_nm = which("nm")
        if not maybe_nm:
            raise ValueError("nm tool was not specified and also not available on PATH")
        nm = Path(maybe_nm)
    if not nm.exists():
        raise ValueError(f"nm tool not found at specified location: {nm!r}")

    lib_dir = Path(sysconfig.get_config_var("installed_platbase")) / str(
        sysconfig.get_config_var("platlibdir")
    )
    libpython = lib_dir / str(sysconfig.get_config_var("LDLIBRARY"))

    if not libpython.exists():
        raise RuntimeError(f"No libpython found in {libpython} ({lib_dir=})")

    # Run nm on the library file
    nm_output = subprocess.check_output([nm, libpython], text=True)

    # Process the output and write linker args
    with open(args.output, "w") as f:
        for line in nm_output.splitlines():
            # Look for lines containing ' T ' which indicates text (code) symbols
            if " T " not in line:
                continue
            # Get the symbol name (third column)
            symbol = line.split()[2]
            # Write linker arguments
            if sys.platform == "linux":
                f.write(f"-Wl,--undefined={symbol}\n")
            else:
                f.write("-Wl,-U\n")
                f.write(f"-Wl,{symbol}\n")


if __name__ == "__main__":
    main()
