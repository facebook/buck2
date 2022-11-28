#!/usr/bin/env python3
import argparse
import json
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


def parse_reference(ref):
    """Parse a Conan package reference of the form name/version#revision."""
    name, version_ref = ref.split("/")
    version, ref = version_ref.split("#")
    return name, version, ref


def parse_lockfile(lockfile):
    """Parse Conan lockfile into a package collection."""
    with open(lockfile) as f:
        data = json.load(f)

    assert data["version"] == "0.4", "Unsupported Conan lockfile version"
    # TODO[AH] profile_host should match the buck2 configuration.
    graph = data["graph_lock"]
    assert graph["revisions_enabled"] == True, "Enable revisions for reproducibility"
    nodes = graph["nodes"]

    pkgs = {}
    for key, item in nodes.items():
        if key == "0":
            # Skip the root package, it just bundles all dependencies.
            continue
        ref = item["ref"]
        name, version, revision = parse_reference(ref)
        package_id = item["package_id"]
        options = item["options"]
        requires = item.get("requires", [])
        # context = item["context"]  # TODO[AH] Do we need this?
        pkgs[key] = {
            "name": name,
            "version": version,
            "revision": revision,
            "reference": ref,
            "package_id": package_id,
            "options": options,
            "requires": requires,
        }

    return pkgs


def generate_targets(pkgs, bzl_out):
    """Write Buck2 targets for the packages to bzl_out."""
    header = """\
load("@prelude//toolchains/conan:defs.bzl", "conan_package")

def conan_packages():
"""
    package_template = """\
    conan_package(
        name = {name},
        reference = {reference},
        options = {options},
        deps = {deps},
    )
"""
    with open(bzl_out, "w") as f:
        f.write(header)
        for pkg in pkgs.values():
            name = pkg["name"]
            reference = pkg["reference"]
            options = pkg["options"].split("\n")
            deps = [":" + pkgs[key]["name"] for key in pkg["requires"]]
            f.write(package_template.format(
                name = repr(name),
                reference = repr(reference),
                options = repr(options),
                deps = repr(deps)))


def main():
    parser = argparse.ArgumentParser(
            prog = "conan_update",
            description = "Update the Conan lock-file and generate Buck2 package imporst.")
    parser.add_argument(
            "--conan",
            metavar="FILE",
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
            help="Name of the Conan lock-file to generate, relative to the Conanfile.")
    parser.add_argument(
            "--lockfile",
            metavar="FILE",
            type=str,
            required=False,
            help="Path to an existing Conan lock-file to base resolution on, relative to the repository root.")
    parser.add_argument(
            "--bzl-out",
            metavar="FILE",
            type=str,
            required=False,
            help="Name of the Starlark file to generate, relative to the Conanfile.")
    args = parser.parse_args()

    root = find_root()

    conan = args.conan
    conanfile = os.path.join(root, args.conanfile)
    lockfile_out = os.path.join(os.path.dirname(conanfile), args.lockfile_out)
    if args.lockfile:
        lockfile = os.path.join(root, args.lockfile)
    else:
        lockfile = None
    bzl_out = os.path.join(os.path.dirname(conanfile), args.bzl_out)

    conan_lock(conan, conanfile, lockfile_out, lockfile)
    pkgs = parse_lockfile(lockfile_out)
    generate_targets(pkgs, bzl_out)


if __name__ == "__main__":
    main()
