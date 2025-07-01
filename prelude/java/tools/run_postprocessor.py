# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.


import argparse
import pathlib

import utils


def _parse_args():
    parser = argparse.ArgumentParser(description="Tool to run a jar postprocessor.")

    parser.add_argument(
        "--postprocessor_cmd",
        type=str,
        required=True,
    )
    parser.add_argument(
        "--zip_scrubber",
        type=str,
        required=True,
        help="A command to run a zip scrubber",
    )
    parser.add_argument(
        "--output",
        type=pathlib.Path,
        required=True,
        help="the path that the postprocessor writes to, which is then scrubbed",
    )

    return parser.parse_args()


def main():
    args = _parse_args()

    utils.execute_command(utils.shlex_split(args.postprocessor_cmd))

    scrubber_cmd = utils.shlex_split(args.zip_scrubber) + [args.output]
    utils.execute_command(scrubber_cmd)


if __name__ == "__main__":
    main()
