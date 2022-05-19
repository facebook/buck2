#!/usr/bin/env python3

"""
Prepares for an object-only ThinLTO link by extracting a given archive and
producing a manifest of the objects contained within.
"""

import argparse
import enum
import json
import os
import subprocess
import sys
from typing import List, Tuple


class ArchiveKind(enum.IntEnum):
    UNKNOWN = 0
    ARCHIVE = 1
    THIN_ARCHIVE = 2


def identify_file(path: str) -> Tuple[ArchiveKind, str]:
    output = subprocess.check_output(["file", "-b", path]).decode()
    if "ar archive" in output:
        return (ArchiveKind.ARCHIVE, output)
    elif "thin archive" in output:
        return (ArchiveKind.THIN_ARCHIVE, output)
    else:
        with open(path, "rb") as infile:
            head = infile.read(7)

            if head == "!<thin>".encode():
                return (ArchiveKind.THIN_ARCHIVE, output)

    return (ArchiveKind.UNKNOWN, output)


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--manifest-out")
    parser.add_argument("--objects-out")
    parser.add_argument("--ar")
    parser.add_argument("--name")
    parser.add_argument("--archive")
    args = parser.parse_args(argv[1:])

    objects_path = args.objects_out
    os.makedirs(objects_path, exist_ok=True)

    known_objects = []
    file_type, debug_output = identify_file(args.archive)
    if file_type == ArchiveKind.ARCHIVE:
        # Unfortunately, we use llvm-ar and, while binutils ar has had --output for
        # a long time, llvm-ar does not support --output and the change in llvm-ar
        # looks like it has stalled for years (https://reviews.llvm.org/D69418)
        # So, we need to invoke ar in the directory that we want it to extract into, and so
        # need to adjust some paths.

        ar_path = os.path.relpath(args.ar, start=objects_path)
        archive_path = os.path.relpath(args.archive, start=objects_path)
        output = subprocess.check_output(
            [ar_path, "xv", archive_path], cwd=objects_path
        ).decode()

        for line in output.splitlines():
            assert line.startswith("x - ")
            obj = line[4:]
            obj_path = os.path.join(objects_path, obj)
            assert os.path.exists(obj_path)
            known_objects.append(obj_path)
    elif file_type == ArchiveKind.THIN_ARCHIVE:
        output = subprocess.check_output([args.ar, "t", args.archive]).decode()
        for line in output.splitlines():
            assert os.path.exists(line)
            known_objects.append(line)
    elif file_type == ArchiveKind.UNKNOWN:
        raise AssertionError(f"unknown archive kind: {debug_output}")

    manifest = {
        "debug": debug_output,
        "objects": known_objects,
    }
    with open(os.path.join(args.manifest_out), "w") as outfile:
        json.dump(manifest, outfile, indent=2, sort_keys=True)

    return 0


sys.exit(main(sys.argv))
