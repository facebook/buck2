# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import pathlib
import subprocess
import sys

CODESIGN_BINARY = "codesign"


def _args_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser()

    parser.add_argument(
        "--path",
        metavar="<PATH>",
        type=pathlib.Path,
        required=True,
        help="Absolute path to the bundle to codesign",
    )
    parser.add_argument(
        "--identity-fingerprint",
        metavar="<IDENTITY_FINGERPRINT>",
        type=str,
        required=True,
        help="Identity fingerprint",
    )
    parser.add_argument(
        "--entitlements-path",
        metavar="<ENTITLEMENTS_PATH>",
        type=pathlib.Path,
        help="Path to entitlements file",
    )
    parser.add_argument(
        "--additional-flags",
        metavar="<ADDITIONAL_FLAGS>",
        type=str,
        nargs="+",
        help="Additional flags",
    )

    return parser


def main() -> None:
    args = _args_parser().parse_args()

    codesign_command = [
        CODESIGN_BINARY,
        "--force",
        "--sign",
        args.identity_fingerprint,
    ]

    if args.entitlements_path:
        codesign_command.extend(["--entitlements", str(args.entitlements_path)])

    if args.additional_flags:
        codesign_command.extend(args.additional_flags)

    codesign_command.append(str(args.path))

    print(" ".join(codesign_command))

    try:
        subprocess.run(codesign_command, check=True)
    except subprocess.CalledProcessError as e:
        print("Error: {}".format(e), file=sys.stderr)


if __name__ == "__main__":
    main()
