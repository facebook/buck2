# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import argparse
import os
import shutil
import string

BINARY_TARGET_TEMPLATE = string.Template(
    """    .binaryTarget(name: "$name", path: "$path")"""
)

PACKAGE_TEMPLATE = string.Template(
    """// swift-tools-version:6.0

import PackageDescription

let package = Package(
  name: "$name",
  products: [
    .library(
      name: "$name",
      targets: [$targets]),
  ],
  targets: [
$binary_targets,
  ]
)
"""
)

FRAMEWORKS_SUBDIR = "Frameworks"


def _main() -> None:
    parser = argparse.ArgumentParser(
        description="""
            Tool which accepts a list of XCFramework binaries to generate
            an SPM package that wraps them all.
        """
    )
    parser.add_argument(
        "--xcframework",
        action="append",
        nargs=2,
        required=True,
        help="Name and path to an XCFramework to include.",
    )
    parser.add_argument(
        "--output-path",
        action="store",
        required=True,
        help="Output dir to write package inside",
    )
    parser.add_argument(
        "--package-name",
        action="store",
        required=True,
        help="Name to use for package in Package.swift",
    )
    args = parser.parse_args()

    os.mkdir(args.output_path)
    os.mkdir(os.path.join(args.output_path, FRAMEWORKS_SUBDIR))

    binary_targets = []
    targets = []

    for framework in args.xcframework:
        name, path = framework
        relative_path = os.path.join(FRAMEWORKS_SUBDIR, os.path.basename(path))
        package_path = os.path.join(args.output_path, relative_path)
        shutil.copytree(path, package_path, symlinks=True)
        binary_targets.append(
            BINARY_TARGET_TEMPLATE.substitute({"name": name, "path": relative_path})
        )
        targets.append(f'"{name}"')

    swift_source = PACKAGE_TEMPLATE.substitute(
        {
            "name": args.package_name,
            "targets": ", ".join(targets),
            "binary_targets": ",\n".join(binary_targets),
        }
    )
    with open(os.path.join(args.output_path, "Package.swift"), "w") as package_swift:
        package_swift.write(swift_source)


if __name__ == "__main__":
    _main()
