# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import os
import subprocess
import sys


def check_nonempty_output(command_parts):
    # Find the output file specified after the '-o' flag
    try:
        output_index = command_parts.index("-o") + 1
        output_file = command_parts[output_index]
    except (ValueError, IndexError):
        print("Warning: Output file not specified with '-o'.")

    # Run the command with the current environment
    try:
        subprocess.run(command_parts, check=True, env=os.environ)
    except subprocess.CalledProcessError as e:
        sys.exit(e.returncode)

    # Check if the output file is not empty
    if os.path.exists(output_file) and os.path.getsize(output_file) == 0:
        print("Error: Output file is empty.")
        sys.exit(1)


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python check_nonempty_output.py <command> [args...]")
        sys.exit(1)

    # Collect all command-line arguments except the script name
    command_parts = sys.argv[1:]
    check_nonempty_output(command_parts)
