#!/usr/bin/env python3

import argparse
import json
import os
import sys


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("--output", type=argparse.FileType("w"), default=sys.stdout)
    parser.add_argument("--origin", help="description of source origin")
    parser.add_argument("--prefix", help="prefix to prepend to destinations")
    parser.add_argument("extracted", help="path to directory of sources")
    args = parser.parse_args(argv[1:])

    entries = []
    for root, _, files in os.walk(args.extracted):
        for name in files:
            path = os.path.join(root, name)
            dest = os.path.relpath(path, args.extracted)
            if args.prefix is not None:
                dest = os.path.join(args.prefix, dest)
            entry = [dest, path]
            if args.origin is not None:
                entry.append(args.origin)
            entries.append(entry)

    json.dump(entries, args.output, indent=2, sort_keys=True)


sys.exit(main(sys.argv))
