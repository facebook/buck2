#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

"""
Quick and dirty wrapper to extract zip files; python 3.6.2+

extract.py my_zip_file.zip --output=output_directory
"""

import argparse
import glob
import os
import shutil
import stat
import tarfile
import tempfile
import zipfile
from pathlib import Path


# shutil.unpack_archive calls zipfile.extract which does *not* preserve file attributes
# (see https://bugs.python.org/issue15795, https://stackoverflow.com/questions/39296101/python-zipfile-removes-execute-permissions-from-binaries).
#
# We need to preserve at least the executable bit.
def extract(src: Path, dst_dir: Path) -> None:
    if src.suffixes[-2:] == [".tar", ".gz"]:
        with tempfile.TemporaryDirectory() as tmp_dir:
            with tarfile.open(src) as tf:
                tf.extractall(tmp_dir)

            # We expect the tgz to contain a single top-level dir with all the
            # items to unpack.
            (path,) = glob.glob(os.path.join(tmp_dir, "*"))
            for ent in os.listdir(path):
                fsrc = os.path.join(path, ent)
                fdst = os.path.join(dst_dir, ent)
                shutil.move(fsrc, fdst)

    else:
        with zipfile.ZipFile(src) as z:
            for info in z.infolist():
                outfile = z.extract(info.filename, dst_dir)
                execute_perms = (info.external_attr >> 16) & (
                    stat.S_IXUSR | stat.S_IXGRP | stat.S_IXOTH
                )
                if execute_perms:
                    st = os.stat(outfile)
                    new_mode = stat.S_IMODE(st.st_mode | execute_perms)
                    if new_mode != st.st_mode:
                        os.chmod(outfile, new_mode)


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Extract .zip/.tar.gz archives to a directory in a cross platform manner"
    )
    parser.add_argument(
        "--output", type=Path, required=True, help="The directory to write to"
    )
    parser.add_argument("src", type=Path, help="The archive to extract to --output")
    args = parser.parse_args()

    args.output.mkdir(parents=True, exist_ok=True)

    extract(args.src, args.output)


if __name__ == "__main__":
    main()
