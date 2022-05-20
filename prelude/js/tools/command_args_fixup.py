#!/usr/bin/env fbpython
# (c) Facebook, Inc. and its affiliates. Confidential and proprietary.

import argparse
import json
import sys


def main() -> None:
    """
    Context: D36482836 + T120448845

    Before the existence of ctx.actions.write_json() we were manually creating
    JSON objects via cmd_args, and it was causing heavy regressions to
    heap allocations in iOS/Android Apps.

    Unfortunately, when we migrated to ctx.actions.write_json() it didn't support
    encoding JSON strings as a JSON object. Eventually, T120448845
    was filed because JS bundling in Facebook for Android broke.

    In D36482836 you can see the many different attempts to fix this problem,
    but the most sure-fire route was building a script which takes in a JSON
    file, fixes it, and outputs the JSON file to a new artifact.

    T121096376: Tracks the long-term solution for this 'hack'.
    """

    parser = argparse.ArgumentParser()
    parser.add_argument("filepath", help="The path to the JSON which needs fixing-up.")
    parser.add_argument("output", help="The file to write to.")
    args = parser.parse_args()

    with open(args.filepath, "r") as input_f:
        try:
            content = json.load(input_f)
        except json.JSONDecodeError:
            print(
                f"Unable to decode {args.filepath}, it should be a JSON file.",
                file=sys.stderr,
                flush=True,
            )
            sys.exit(1)

    if "extraData" in content:
        try:
            content["extraData"] = json.loads(content["extraData"])
        except json.JSONDecodeError:
            print(
                f"Unable to decode 'extraData' in {args.filepath}, it should be a JSON string.",
                file=sys.stderr,
                flush=True,
            )
            sys.exit(1)

    with open(args.output, "w") as output_f:
        output_f.write(json.dumps(content, sort_keys=True))


if __name__ == "__main__":
    main()
