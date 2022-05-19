#!/usr/bin/env python3
import os
import subprocess


def main():
    repo_root = subprocess.check_output(["hg", "root"], encoding="utf-8").strip()
    repo_root = os.path.realpath(repo_root)
    cwd = os.path.realpath(".")

    print("repo_root = {}".format(repo_root))
    print("cwd = {}".format(cwd))

    assert cwd == os.path.join(repo_root, "fbcode")


if __name__ == "__main__":
    main()
