#!/usr/bin/env python3
import argparse
import json
import os
import subprocess

import conan_common


def conan_install(
        conan,
        conanfile,
        lockfile,
        install_folder,
        output_folder,
        user_home,
        manifests,
        install_info,
        trace_log):
    env = conan_common.conan_env(
            user_home=user_home,
            trace_log=trace_log)

    args = ["install"]
    args.extend(["--generator", "BucklerGenerator"])
    args.extend(["--lockfile", lockfile])
    args.extend(["--install-folder", install_folder])
    args.extend(["--output-folder", output_folder])
    args.extend(["--manifests", manifests])
    args.extend(["--json", install_info])
    args.append(conanfile)

    conan_common.run_conan(conan, *args, env=env)


def main():
    parser = argparse.ArgumentParser(
            prog = "conan_lock",
            description = "Update the Conan lock-file.")
    parser.add_argument(
            "--conan",
            metavar="FILE",
            type=str,
            required=True,
            help="Path to the Conan executable.")
    parser.add_argument(
            "--conan-init",
            metavar="PATH",
            type=str,
            required=True,
            help="Path to the base Conan user-home.")
    parser.add_argument(
            "--buckler",
            metavar="FILE",
            type=str,
            required=True,
            help="Path to the Buckler generator.")
    parser.add_argument(
            "--install-folder",
            metavar="PATH",
            type=str,
            required=True,
            help="Path to install directory to place generator files into.")
    parser.add_argument(
            "--output-folder",
            metavar="PATH",
            type=str,
            required=True,
            help="Path to the root output folder for generated and built files.")
    parser.add_argument(
            "--user-home",
            metavar="PATH",
            type=str,
            required=True,
            help="Path to the Conan base directory.")
    parser.add_argument(
            "--manifests",
            metavar="PATH",
            type=str,
            required=True,
            help="Write dependency manifests into this directory.")
    parser.add_argument(
            "--install-info",
            metavar="PATH",
            type=str,
            required=True,
            help="Write install information JSON file to this location.")
    parser.add_argument(
            "--trace-file",
            metavar="PATH",
            type=str,
            required=True,
            help="Write Conan trace log to this file.")
    parser.add_argument(
            "--conanfile",
            metavar="FILE",
            type=str,
            required=True,
            help="Path to the Conanfile.")
    parser.add_argument(
            "--lockfile",
            metavar="FILE",
            type=str,
            required=False,
            help="Path to the Conan lock-file.")
    args = parser.parse_args()

    conan_common.install_user_home(args.user_home, args.conan_init)
    conan_common.install_generator(args.user_home, args.buckler)

    os.mkdir(args.install_folder)
    os.mkdir(args.output_folder)
    os.mkdir(args.manifests)

    conan_install(
            args.conan,
            args.conanfile,
            args.lockfile,
            args.install_folder,
            args.output_folder,
            args.user_home,
            args.manifests,
            args.install_info,
            args.trace_file)
    # TODO[AH] Copy the generated bindings to a dedicated output file.


if __name__ == "__main__":
    main()
