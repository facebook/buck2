# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""Write a rustdoc argfile with the right `--emit=` value.

Callers pass post-rename emit names via `--emit=...`; see
`rustdoc_emit_compat` for the rename story. The resolved value is
written into an argfile that `build.bzl` feeds to rustdoc via
`@argfile`.
"""

import argparse
import sys

from rustdoc_emit_compat import resolve_emits


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--rustdoc", required=True)
    parser.add_argument("--out", required=True)
    parser.add_argument(
        "--emit",
        required=True,
        help="Comma-separated post-rename emit names; each is substituted to the "
        "old name if the current rustdoc doesn't recognise the new one.",
    )
    args = parser.parse_args()

    resolved = resolve_emits(args.rustdoc, args.emit.split(","))
    with open(args.out, "w") as f:
        # `None` => rustdoc recognises no requested emit name; write a
        # completely empty argfile so `@argfile` contributes nothing.
        if resolved is not None:
            f.write(resolved + "\n")

    return 0


if __name__ == "__main__":
    sys.exit(main())
