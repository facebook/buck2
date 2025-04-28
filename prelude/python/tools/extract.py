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
import configparser
import glob
import json
import os
import shutil
import stat
import tarfile
import tempfile
import zipfile
from pathlib import Path
from typing import Optional


def strip_soabi_tag(path: Path) -> Optional[Path]:
    """
    Helper to strip any SOABI tag from the given extension path.  Returns `None`
    if no stripping is performed.
    """

    suffixes = path.suffixes[-2:]

    # SOABI tagged extensions should have two suffixes.
    if len(suffixes) != 2:
        return None

    # Not an extension.
    ext = ""
    for ext in (".so", ".pyd"):
        if suffixes[1] == ext:
            break
    else:
        return None

    # TODO(agallagher): Is there a better way to detect these tags?
    if not (suffixes[0].startswith(".cpython-") or suffixes[0] == ".abi3"):
        return None

    return path.with_suffix("").with_suffix(ext)


# shutil.unpack_archive calls zipfile.extract which does *not* preserve file attributes
# (see https://bugs.python.org/issue15795, https://stackoverflow.com/questions/39296101/python-zipfile-removes-execute-permissions-from-binaries).
#
# We need to preserve at least the executable bit.
def extract(src: Path, dst_dir: Path, strip_soabi_tags: bool = False) -> None:
    if src.suffixes[-2:] == [".tar", ".gz"]:
        with tempfile.TemporaryDirectory() as tmp_dir:
            with tarfile.open(src) as tf:
                tf.extractall(tmp_dir)

            # We expect the tgz to contain a single top-level dir with all the
            # items to unpack.
            (path,) = glob.glob(os.path.join(tmp_dir, "*"))
            for ent in os.listdir(path):
                fsrc = os.path.join(path, ent)
                fdst = Path(os.path.join(dst_dir, ent))
                soabi_less_dst = strip_soabi_tag(fdst)
                if soabi_less_dst is not None:
                    fdst = soabi_less_dst
                shutil.move(fsrc, fdst)

    else:
        with zipfile.ZipFile(src) as z:
            for info in z.infolist():
                outfile = Path(z.extract(info.filename, dst_dir))
                if strip_soabi_tags:
                    soabi_less_outfile = strip_soabi_tag(outfile)
                    if soabi_less_outfile is not None:
                        os.rename(outfile, soabi_less_outfile)
                        outfile = soabi_less_outfile
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
    parser.add_argument("--strip-soabi-tags", action="store_true")
    parser.add_argument("--entry-points", type=Path, help="The directory to write to")
    parser.add_argument(
        "--cxx-header-dirs",
        type=Path,
        help="A file to write out inferred C++ include dirs to",
    )
    parser.add_argument(
        "--entry-points-manifest", type=Path, help="The directory to write to"
    )
    parser.add_argument("src", type=Path, help="The archive to extract to --output")
    args = parser.parse_args()

    args.output.mkdir(parents=True, exist_ok=True)

    extract(
        src=args.src,
        dst_dir=args.output,
        strip_soabi_tags=args.strip_soabi_tags,
    )

    # Infer C++ header dirs.
    if args.cxx_header_dirs is not None:
        with open(args.cxx_header_dirs, mode="w") as f:
            for root, dirs, _files in os.walk(args.output):
                root = os.path.relpath(root, args.output)
                if "include" in dirs:
                    print(os.path.normpath(os.path.join(root, "include")), file=f)

    # Extract any "entry points" from the wheel, and generate scripts from them
    # (just like `pip install` would do).
    if args.entry_points is not None:
        entry_points = glob.glob(
            os.path.join(args.output, "*.dist-info", "entry_points.txt")
        )
        os.makedirs(args.entry_points, exist_ok=True)
        manifest = []
        if entry_points:
            (entry_points,) = entry_points
            config = configparser.ConfigParser()
            config.read(entry_points)
            if config.has_section("console_scripts"):
                for name, entry_point in config.items("console_scripts"):
                    mod, func = entry_point.split(":")
                    path = os.path.join(args.entry_points, name)
                    manifest.append(
                        (name, path, os.path.relpath(entry_points, args.output))
                    )
                    with open(path, mode="w") as bf:
                        bf.write(
                            """\
#!/usr/bin/env python
# -*- coding: utf-8 -*-
import re
import sys
from {mod} import {func}
if __name__ == '__main__':
    sys.argv[0] = re.sub(r'(-script\\.pyw|\\.exe)?$', '', sys.argv[0])
    sys.exit({func}())
""".format(mod=mod, func=func)
                        )
                    os.chmod(path, 0o777)
        with open(args.entry_points_manifest, mode="w") as f:
            json.dump(manifest, f)


if __name__ == "__main__":
    main()
