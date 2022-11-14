#!/usr/bin/env fbpython
# (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

# pyre-unsafe

import json
import shutil
import subprocess
import sys


def rewrite_dep_file(always_used_files_path, used_classes_path, dst_path):
    """
    Convert a used_classes.json to a depfile suitable for use by Buck2. The files we
    rewrite are JSON where the keys are the jars that were used.
    """
    shutil.copyfile(always_used_files_path, dst_path)

    with open(used_classes_path) as f:
        used_classes_body = f.read()

    used_classes_map = json.loads(used_classes_body)

    with open(dst_path, "a") as f:
        f.write("\n")
        f.write("\n".join(used_classes_map.keys()))


def main():
    """
    First argument is a file containing a list of files that should be put directly
    into the dep file.
    Second argument is a "used_classes.json" file.
    Third argument is where the dep file should be written.
    The command follows the third argument.
    """
    ret = subprocess.call(sys.argv[4:])
    if ret == 0:
        rewrite_dep_file(sys.argv[1], sys.argv[2], sys.argv[3])
    sys.exit(ret)


if __name__ == "__main__":
    main()
