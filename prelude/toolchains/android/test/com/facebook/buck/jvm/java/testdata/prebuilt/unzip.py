# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import contextlib
import os
import sys
import zipfile


def extractall(zf, path):
    for name in zf.namelist():
        if name.endswith("/"):
            try:
                os.makedirs(os.path.join(path, name))
            except Exception:
                pass
        else:
            zf.extract(name, path)


def main():
    from_ = sys.argv[1]
    to = sys.argv[2]
    if sys.platform in ("win32", "cygwin"):
        to = "\\\\?\\" + to  # use long path names.

    with contextlib.closing(zipfile.ZipFile(from_)) as zf:
        extractall(zf, to)


if __name__ == "__main__":
    main()
