#!/usr/bin/env python3
import argparse
import os
import subprocess


def find_root():
    """Find the repository root using `buck2 root`."""
    # TODO[AH] This assumes that buck2 is in PATH when executing the script via `buck2 run`.
    #   Consider making the name/path `buck2` configurable via an environment variable.
    return subprocess.check_output(["buck2", "root"], text=True).strip()


def conan_lock(conan, conanfile, lockfile_out, lockfile=None):
    args = [conan, "lock", "create"]
    if lockfile:
        args.extend(["--lockfile", lockfile])
    args.extend(["--lockfile-out", lockfile_out])
    args.append(conanfile)
    env = dict(os.environ)
    # Enable Conan revisions for reproducibility
    env["CONAN_REVISIONS_ENABLED"] = "1"
    subprocess.check_call(args, env=env)


def main():
    parser = argparse.ArgumentParser(
            prog = "conan_update",
            description = "Update the Conan lock-file and generate Buck2 package imporst.")
    parser.add_argument(
            "--conan",
            metavar="File",
            type=str,
            required=True,
            help="Path to the Conan executable, relative to the build root.")
    parser.add_argument(
            "--conanfile",
            metavar="FILE",
            type=str,
            required=True,
            help="Path to the Conanfile, relative to the repository root.")
    parser.add_argument(
            "--lockfile-out",
            metavar="FILE",
            type=str,
            required=True,
            help="Name oo the Conan lock-file to generate, relative to the Conanfile.")
    parser.add_argument(
            "--lockfile",
            metavar="FILE",
            type=str,
            required=False,
            help="Path to an existing Conan lock-file to base resolution on, relative to the repository root.")
    args = parser.parse_args()

    root = find_root()

    conan = args.conan
    conanfile = os.path.join(root, args.conanfile)
    lockfile_out = os.path.join(os.path.dirname(conanfile), args.lockfile_out)
    if args.lockfile:
        lockfile = os.path.join(root, args.lockfile)
    else:
        lockfile = None

    conan_lock(conan, conanfile, lockfile_out, lockfile)


if __name__ == "__main__":
    main()
