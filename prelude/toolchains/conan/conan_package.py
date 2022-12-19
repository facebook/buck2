#!/usr/bin/env python3
import argparse
import os
import shutil
import subprocess

import conan_common


def conan_install(
        conan,
        reference,
        lockfile,
        options,
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
    args.extend(["--build", "missing"])
    args.extend(["--lockfile", lockfile])
    args.extend(["--install-folder", install_folder])
    args.extend(["--output-folder", output_folder])
    args.extend(["--manifests", manifests])
    args.extend(["--json", install_info])
    args.append(reference.split("#")[0] + "@")

    conan_common.run_conan(conan, *args, env=env)


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
            "--conan-init",
            metavar="PATH",
            type=str,
            required=True,
            help="Path to the base Conan user-home.")
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
            "--package-id",
            metavar="STRING",
            type=str,
            required=True,
            help="Package ID of the Conan package to build.")
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
    parser.add_argument(
            "--trace-file",
            metavar="PATH",
            type=str,
            required=True,
            help="Write Conan trace log to this file.")
    parser.add_argument(
            "--cache-out",
            metavar="PATH",
            type=str,
            required=True,
            help="Copy the package's cache directory to this path.")
    parser.add_argument(
            "--package-out",
            metavar="PATH",
            type=str,
            required=True,
            help="Copy the package directory to this path.")
    parser.add_argument(
            "--dep-reference",
            metavar="STRING",
            type=str,
            required=False,
            action="append",
            default=[],
            help="Conan package dependency reference. All --dep-* arguments must align.")
    parser.add_argument(
            "--dep-cache-out",
            metavar="PATH",
            type=str,
            required=False,
            action="append",
            default=[],
            help="Conan package dependency cache output directory. All --dep-* arguments must align.")
    # TODO[AH] Remove the unused `--manifests` and `--verify` flags and
    #   outputs.
    # TODO[AH] Should we enable the `--no-imports` flag?
    # TODO[AH] Handle packages that are build requirements and set
    #   `--build-require` in that case.
    args = parser.parse_args()

    conan_common.install_user_home(args.user_home, args.conan_init)
    assert len(args.dep_reference) == len(args.dep_cache_out), "Mismatching dependency arguments."
    for ref, cache_out in zip(args.dep_reference, args.dep_cache_out):
        conan_common.install_reference(args.user_home, ref, cache_out)

    os.mkdir(args.install_folder)
    os.mkdir(args.output_folder)
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
            args.install_info,
            args.trace_file)
    # TODO[AH] Verify that only the current package was built and that
    #   dependencies were found in the Conan cache as installed by
    #   `install_reference` above. Possible ways to do this:
    #   - Use the install-info produced with the `--json` flag and check for
    #     the `downloaded` and `built` fields of the `packages` objects.
    #   - Use the trace log produced with the `CONAN_TRACE_FILE` env-var and
    #     check for `GOT_RECIPE_FROM_LOCAL_CACHE` entries as opposed to
    #     `DOWNLOADED_PACKAGE` or `PACKAGE_BUILT_FROM_SOURCES`.
    conan_common.extract_reference(
            args.user_home,
            args.reference,
            args.cache_out)
    conan_common.extract_package(
            args.user_home,
            args.reference,
            args.package_id,
            args.package_out)


if __name__ == "__main__":
    main()
