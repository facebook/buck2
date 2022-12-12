#!/usr/bin/env python3
import argparse
import os
import shutil
import subprocess

import conan_common


def conan_profile(
        conan,
        user_home,
        trace_log):
    env = conan_common.conan_env(
            user_home=user_home,
            trace_log=trace_log)

    # TODO[AH] Do we need to support multiple profiles for cross-compilation?
    #          Consider adding dedicated rules to define profiles.
    profile_name = "default"
    # TODO[AH] Configure the compiler according to the Buck2 provided C++ toolchain.
    settings = [
        ("settings.arch", "x86_64"),
        ("settings.arch_build", "x86_64"),
        ("settings.build_type", "Release"),
        ("settings.compiler", "gcc"),
        ("settings.compiler.version", "11"),
        ("settings.compiler.libcxx", "libstdc++11"),
        ("settings.os", "Linux"),
        ("settings.os_build", "Linux"),
    ]
    # TODO[AH] Allow users to define additional remotes.
    remotes = [
        ("conancenter", "https://center.conan.io"),
    ]

    conan_common.run_conan(conan, "profile", "new", profile_name, env=env)
    for key, value in settings:
        conan_common.run_conan(conan, "profile", "update", "{}={}".format(key, value), profile_name, env=env)
    for name, url in remotes:
        conan_common.run_conan(conan, "remote", "add", "-f", name, url, env=env)


def main():
    parser = argparse.ArgumentParser(
            prog = "conan_init",
            description = "Initialise a Conan home directory.")
    parser.add_argument(
            "--conan",
            metavar="FILE",
            type=str,
            required=True,
            help="Path to the Conan executable.")
    parser.add_argument(
            "--user-home",
            metavar="PATH",
            type=str,
            required=True,
            help="Path to the Conan base directory.")
    parser.add_argument(
            "--trace-file",
            metavar="PATH",
            type=str,
            required=True,
            help="Write Conan trace log to this file.")
    # TODO[AH] Look into --settings and the like to configure a Buck2 provided toolchain.
    args = parser.parse_args()

    conan_profile(
            args.conan,
            args.user_home,
            args.trace_file)


if __name__ == "__main__":
    main()
