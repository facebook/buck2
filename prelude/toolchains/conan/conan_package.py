#!/usr/bin/env python3
import argparse
import os
import subprocess


def conan_install(conan, reference, lockfile, options, install_folder, output_folder, user_home):
    args = [conan, "install"]
    args.extend(["--lockfile", lockfile])
    args.extend(["--install-folder", install_folder])
    args.extend(["--output-folder", output_folder])
    # TODO options cannot be combined with lockfile.
    #for option in options:
    #    args.extend(["--options", option])
    # TODO remove revision, it's not supported on the command line.
    args.append(reference.split("#")[0] + "@")

    env = dict(os.environ)
    # TODO[AH] Enable Conan revisions for reproducibility
    # Enable Conan revisions for reproducibility
    #env["CONAN_REVISIONS_ENABLED"] = "1"
    # Prevent over-allocation.
    env["CONAN_CPU_COUNT"] = "1"
    # Prevent interactive prompts.
    env["CONAN_NON_INTERACTIVE"] = "1"
    # Print every `self.run` invokation.
    # TODO Remove this debug output.
    env["CONAN_PRINT_RUN_COMMANDS"] = "1"
    # Set the Conan base directory.
    env["CONAN_USER_HOME"] = os.path.abspath(user_home)
    # Disable the short paths feature on Windows.
    # TODO Enable if needed with a hermetic short path.
    env["CONAN_USER_HOME_SHORT"] = "None"

    # Workaround gcc/clang ABI compatibility warning.
    # TODO Solve this through the proper toolchain configuration.
    subprocess.check_call("conan profile new default".split(), env=env)
    subprocess.check_call("conan profile update settings.compiler=gcc default".split(), env=env)
    subprocess.check_call("conan profile update settings.compiler.version=11 default".split(), env=env)
    subprocess.check_call("conan profile update settings.compiler.libcxx=libstdc++11 default".split(), env=env)
    subprocess.check_call(args, env=env)


def main():
    parser = argparse.ArgumentParser(
            prog = "conan_package",
            description = "Build a Conan package.")
    parser.add_argument(
            "--conan",
            metavar="FILE",
            type=str,
            required=True,
            help="Path to the Conan executable.")
    parser.add_argument(
            "--lockfile",
            metavar="FILE",
            type=str,
            required=True,
            help="Path to the Conan lockfile.")
    parser.add_argument(
            "--reference",
            metavar="STRING",
            type=str,
            required=True,
            help="Reference of the Conan package to build.")
    parser.add_argument(
            "--option",
            metavar="STRING",
            type=str,
            required=False,
            action="append",
            help="Conan options for the package to build.")
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
            help="Path to the Conan base directory used for Conan's cache.")
    # TODO Look into --manifests and --verify to enforce buck built deps.
    # TODO Look into --no-imports.
    # TODO Look into --build-require for exec deps.
    # TODO Look into --json for machine readable output metadata.
    # TODO Look into --require-override to enforce Buck2 provided dependencies.
    # TODO Look into --build to enforce from-source builds with correct toolchain.
    # TODO Can we avoid having to pass --lockfile, or is it needed.
    # TODO If the lockfile is needed, can we shrink it to the relevant deps? To avoid unnecessary cache invalidation.
    # TODO Look into --settings and the like to configure a Buck2 provided toolchain.
    args = parser.parse_args()

    # TODO Do we need to pre-create these?
    os.mkdir(args.install_folder)
    os.mkdir(args.output_folder)
    os.mkdir(args.user_home)

    conan = args.conan
    conan_install(
            conan,
            args.reference,
            args.lockfile,
            args.option,
            args.install_folder,
            args.output_folder,
            args.user_home)


if __name__ == "__main__":
    main()
