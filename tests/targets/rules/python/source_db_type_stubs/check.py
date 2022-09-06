import argparse
import json
import sys


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("expected")
    parser.add_argument("actual")
    # TODO(agallagher): Currently, extra runtime files are expanded via
    # `$(location ..)` macros, so just ignore them for now...
    parser.add_argument("remainder", nargs="*")
    args = parser.parse_args(argv[1:])

    with open(args.expected) as f:
        expected = json.load(f)

    with open(args.actual) as f:
        actual = json.load(f)

    return 0 if expected == actual else 1


sys.exit(main(sys.argv))
