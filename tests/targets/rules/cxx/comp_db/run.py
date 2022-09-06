#!/usr/bin/env python3

import argparse
import json
import os
import sys


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", default="/dev/null")
    parser.add_argument("db")
    args = parser.parse_args(argv[1:])

    with open(args.db) as f:
        db = json.load(f)

    cmd = db[0]["arguments"] + ["-o", args.output]
    os.execv(cmd[0], cmd)


sys.exit(main(sys.argv))
