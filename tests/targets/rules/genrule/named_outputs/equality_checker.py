#!/usr/bin/env python3
# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import sys
from pathlib import Path


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Pass 2 arguments to check files are equal", file=sys.stderr)
        sys.exit(1)

    txt1 = Path(sys.argv[1]).read_text()
    txt2 = Path(sys.argv[2]).read_text()

    if txt1.strip() != txt2.strip():
        print(
            "Files are not equal, 1st:\n==========BEGIN\n{}\n============END\n\n2nd:\n==========BEGIN\n{}\n============END".format(
                txt1, txt2
            ),
            file=sys.stderr,
        )
        sys.exit(1)
