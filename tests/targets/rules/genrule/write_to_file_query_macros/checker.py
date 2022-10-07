# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import os
import sys
from pathlib import Path


def targets(file_path):
    if not file_path.is_file():
        print(
            "File {} doesn't exist, cwd is {}".format(
                file_path,
                os.getcwd(),
            ),
            file=sys.stderr,
        )
        sys.exit(1)
    txt = file_path.read_text()
    return {a.strip() for a in txt.split(" ")}


def main():
    if len(sys.argv) != 3:
        print("Wrong number of arguments", file=sys.stderr)
        sys.exit(1)

    path1 = Path(sys.argv[1].lstrip("@"))
    path2 = Path(sys.argv[2].lstrip("@"))

    targets1 = targets(path1)
    targets2 = targets(path2)

    if len(targets1 ^ targets2):
        print(
            "Target lists are not equal, elements in the 1st list which are not in the 2nd: {}, elements in the 2nd list which are not in the 1st: {}".format(
                targets1 - targets2, targets2 - targets1
            ),
            file=sys.stderr,
        )
        sys.exit(1)

    print("All good, relax!")


if __name__ == "__main__":
    main()
