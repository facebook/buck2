#!/usr/bin/env python3

import argparse
import json
import os


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "env_file", help="A JSON file containing the environment to inject"
    )
    parser.add_argument("executable")
    parser.add_argument("args", nargs="*")

    args = parser.parse_args()

    with open(args.env_file) as env_file:
        env_from_file = json.load(env_file)
        env = {**os.environ, **env_from_file}

    os.execve(args.executable, [args.executable, *args.args], env)


if __name__ == "__main__":
    main()
