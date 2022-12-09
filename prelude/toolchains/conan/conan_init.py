#!/usr/bin/env python3
import argparse
import os
import shutil
import subprocess


def conan_dir():
    """Conan folder under the Conen user home."""
    return ".conan"


def conan_env(user_home, trace_log):
    """Generate environment variables needed to invoke Conan."""
    env = dict(os.environ)

    # TODO[AH] Enable Conan revisions for reproducibility
    # Enable Conan revisions for reproducibility
    #env["CONAN_REVISIONS_ENABLED"] = "1"
    # Prevent over-allocation.
    env["CONAN_CPU_COUNT"] = "1"
    # Prevent interactive prompts.
    env["CONAN_NON_INTERACTIVE"] = "1"
    # Print every `self.run` invokation.
    # TODO[AH] Remove this debug output.
    env["CONAN_PRINT_RUN_COMMANDS"] = "1"
    # Set the Conan base directory.
    env["CONAN_USER_HOME"] = os.path.abspath(user_home)
    # Disable the short paths feature on Windows.
    # TODO[AH] Enable if needed with a hermetic short path.
    env["CONAN_USER_HOME_SHORT"] = "None"
    # Enable Conan debug trace.
    env["CONAN_TRACE_FILE"] = os.path.abspath(trace_log)

    return env


def conan_profile(
        conan,
        user_home,
        trace_log):
    env = conan_env(user_home, trace_log)

    # TODO[AH] Do we need to support multiple profiles for cross-compilation?
    profile_name = "default"
    # TODO[AH] Configure the compiler according to the Buck2 provided C++ toolchain.
    settings = [
        ("settings.compiler", "gcc"),
        ("settings.compiler.version", "11"),
        ("settings.compiler.libcxx", "libstdc++11"),
    ]

    subprocess.check_call([conan, "profile", "new", profile_name], env=env)
    for key, value in settings:
        subprocess.check_call([conan, "profile", "update", "{}={}".format(key, value), profile_name], env=env)


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
