#!/usr/bin/env python3
import argparse
import json
import os
import subprocess

import conan_common


def parse_lockfile(lockfile):
    """Parse Conan lockfile into a package collection."""
    with open(lockfile) as f:
        data = json.load(f)

    assert data["version"] == "0.4", "Unsupported Conan lockfile version"
    graph = data["graph_lock"]
    # TODO[AH] Enable Conan revisions for reproducibility
    #assert graph["revisions_enabled"] == True, "Enable revisions for reproducibility"
    nodes = graph["nodes"]

    pkgs = {}
    for key, item in nodes.items():
        if key == "0":
            # Skip the root package, it just bundles all dependencies.
            continue
        ref = item["ref"]
        name, _, _, _, _ = conan_common.parse_reference(ref)
        package_id = item["package_id"]
        options = item["options"]
        requires = item.get("requires", [])
        # context = item["context"]  # TODO[AH] Do we need this?
        pkgs[key] = {
            "name": name,
            "reference": ref,
            "package_id": package_id,
            "options": options,
            "requires": requires,
        }

    return pkgs


def generate_targets(lockfile_label, pkgs, targets_out):
    """Write Buck2 targets for the packages to bzl_out."""
    package_template = """\

conan_package(
    name = {name},
    lockfile = {lockfile},
    reference = {reference},
    package_id = {package_id},
    deps = {deps},
)
"""
    with open(targets_out, "w") as f:
        for pkg in pkgs.values():
            name = "_package_" + pkg["name"]
            reference = pkg["reference"]
            package_id = pkg["package_id"]
            deps = [":_package_" + pkgs[key]["name"] for key in pkg["requires"]]
            f.write(package_template.format(
                name = repr(name),
                # TODO[AH] Remove that lockfile and generate a minimal one in the rule.
                #   Using the full lock file means that any change to the set
                #   of Conan packages will require a rebuild of all Conan
                #   packages. Generating minimal lock files with only the
                #   required information per package will only invalidate those
                #   packages that were affected by a change. Note, the lock
                #   file also contains the Conan profile, which defines the
                #   Buck2 provided C/C++ toolchain. This information would need
                #   to be included in a minimal lockfile.
                lockfile = repr(lockfile_label),
                reference = repr(reference),
                package_id = repr(package_id),
                deps = repr(deps)))


def main():
    parser = argparse.ArgumentParser(
            prog = "lock_generate",
            description = "Generate Buck2 build targets for Conan packages.")
    parser.add_argument(
            "--lockfile",
            metavar="FILE",
            type=str,
            required=False,
            help="Path to the Conan lock-file.")
    parser.add_argument(
            "--lockfile-label",
            metavar="LABEL",
            type=str,
            required=False,
            help="Buck2 label for the Conan lock-file.")
    parser.add_argument(
            "--targets-out",
            metavar="FILE",
            type=str,
            required=False,
            help="Write the generated targets to this file.")
    args = parser.parse_args()

    pkgs = parse_lockfile(args.lockfile)
    generate_targets(args.lockfile_label, pkgs, args.targets_out)


if __name__ == "__main__":
    main()
