# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-ignore-all-errors

import os
import sys

from live_builder import LiveBuilder

from util import get_args_parser, get_target_info


def main():
    parser = get_args_parser(fromfile_prefix_chars="@")
    parser.add_argument("--template")
    parser.add_argument(
        "--bootstrap-output",
        help="write the archive to bootstrap script",
        required=True,
    )
    args, _ = parser.parse_known_args(sys.argv[1:])
    args.target = get_target_info(args.target)

    builder = LiveBuilder(options=args, manifest=None, linktree_suffix="#link-tree")
    bootstrap_contents = builder._gen_interp_file(args.template)
    with open(args.bootstrap_output, "w") as f:
        f.write(bootstrap_contents)
        os.chmod(f.name, 0o755)

    header_contents = builder._gen_header()
    with open(args.output, "w") as f:
        f.write(header_contents)
        os.chmod(f.name, 0o755)


if __name__ == "__main__":
    main()
