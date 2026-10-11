# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import os
import sys


def main():
    (out, mode) = sys.argv[1:]
    scratch = os.environ["BUCK_SCRATCH_PATH"]
    with open(os.path.join(scratch, "big"), "wb") as f:
        f.write(b"x" * (1 << 20))
    # Where the scratch was, for the test to check after the build. A failing action reports it
    # on stderr, since it has no output.
    print("scratch=" + scratch, file=sys.stderr)
    with open(out, "w") as f:
        f.write(scratch)
    if mode == "fail":
        sys.exit(1)


if __name__ == "__main__":
    main()
