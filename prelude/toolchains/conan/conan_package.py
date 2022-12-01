#!/usr/bin/env python3
import argparse
import os
import subprocess


def _none(s):
    if not s or s == "_":
        return None
    else:
        return s


def parse_reference(ref):
    """Parse a Conan package reference.

    These take the shape `name/version@channel/name#revision`.
    Omitted values or `_` are read as `None`.
    """
    name = None
    version = None
    user = None
    channel = None
    revision = None

    if "#" in ref:
        ref, revision = ref.split("#", 1)

    if "@" in ref:
        ref, user_channel = ref.split("@", 1)
        if "/" in user_channel:
            user, channel = user_channel.split("/", 1)
        else:
            user = user_channel

    if "/" in ref:
        name, version = ref.split("/", 1)
    else:
        name = ref

    return _none(name), _none(version), _none(user), _none(channel), _none(revision)


def conan_install(
        conan,
        reference,
        lockfile,
        options,
        install_folder,
        output_folder,
        user_home,
        manifests,
        install_info):
    args = [conan, "install"]
    args.extend(["--lockfile", lockfile])
    args.extend(["--install-folder", install_folder])
    args.extend(["--output-folder", output_folder])
    args.extend(["--manifests", manifests])
    args.extend(["--json", install_info])
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

    # TODO The build places downloads, metadata, and artifacts under a path of the form:
    #     user-home/.conan/data/<name>/<version>/<user>/<channel>/
    #   where missing fields are replaced by '_', for example:
    #     user-home/.conan/data/openssl/3.0.7/_/_
    #   the build artifacts are placed under:
    #     user-home/.conan/data/<name>/<version>/<user>/<channel>/package/<package-id>
    #   for example:
    #     user-home/.conan/data/openssl/3.0.7/_/_/package/304480252b01879c8641f79a653b593b8f26cf9f
    #   place the output directories of dependencies from previous build actions into the current build directory.

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
    os.mkdir(args.manifests)

    conan = args.conan
    conan_install(
            conan,
            args.reference,
            args.lockfile,
            args.option,
            args.install_folder,
            args.output_folder,
            args.user_home,
            args.manifests,
            args.install_info)


if __name__ == "__main__":
    main()
