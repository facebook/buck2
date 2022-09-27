# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import os
import sys
from pathlib import Path


def main():
    if len(sys.argv) != 3:
        print("Wrong number of arguments", file=sys.stderr)
        sys.exit(1)

    path = Path(sys.argv[1])
    if not path.is_file():
        print(
            "File with text doesn't exist, cwd is {}, path is {}".format(
                os.getcwd(), path
            ),
            file=sys.stderr,
        )
        sys.exit(1)

    txt = path.read_text()
    expected = sys.argv[2]
    if txt != expected:
        print("Strings not equal: {} != {}".format(txt, expected), file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
