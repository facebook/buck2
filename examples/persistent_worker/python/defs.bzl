# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def fetch_python_libraries(pkgs):
    for name, pkg in pkgs.items():
        native.remote_file(
            name = "{}-download".format(name),
            url = pkg["url"],
            sha256 = pkg["sha256"],
            out = "{}.whl".format(name),
        )
        native.prebuilt_python_library(
            name = name,
            binary_src = ":{}-download".format(name),
            deps = [":{}".format(dep) for dep in pkg.get("deps", [])],
            visibility = ["PUBLIC"],
        )
