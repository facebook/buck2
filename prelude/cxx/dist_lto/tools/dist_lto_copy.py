#!/usr/bin/env python3

import argparse
import shutil
import sys
from typing import List


def main(argv: List[str]) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--to")
    parser.add_argument("--from", dest="from_")
    args = parser.parse_args(argv[1:])
    shutil.copy(args.from_, args.to)
    return 0


sys.exit(main(sys.argv))
