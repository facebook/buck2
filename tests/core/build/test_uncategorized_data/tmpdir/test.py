# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import os
import platform
import sys


def main():
    (out, location) = sys.argv[1:]

    if platform.system() == "Windows":
        check = ["TEMP", "TMP"]
        buck_out = "buck-out\\v2"
    else:
        check = ["TMPDIR"]
        buck_out = "buck-out/v2"

    scratch = os.environ["BUCK_SCRATCH_PATH"]
    assert not os.path.isabs(scratch), scratch
    assert buck_out in scratch, scratch
    assert os.path.isdir(scratch), scratch

    for c in check:
        v = os.environ[c]
        assert os.path.isabs(v), v

        if location == "local":
            # Check the path is "ours"
            assert buck_out in v, v
            # Check the path is the same as BUCK_SCRATCH_PATH
            rel = os.path.relpath(os.path.normpath(v))
            assert rel == scratch, rel
        elif location == "remote":
            pass
        else:
            raise Exception("invalid location: %s" % location)

    with open(out, "w"):
        pass


if __name__ == "__main__":
    main()
