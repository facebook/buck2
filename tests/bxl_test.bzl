# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode//buck2/tests:buck_e2e.bzl", "buck2_e2e_test")
load("@fbcode_macros//build_defs:export_files.bzl", "export_file")

def bxl_test(src, name = None, labels = None, **kwargs):
    """
    Creates a test target from a buck2 bxl script. BXL script must use "test" as entry
    point.

    Parameters:
        src: source path of BXL script. This cannot be a target since bxl
            can only be invoked from the repo and not from buck-out.
        name: Name of the test target. If unspecified, use src as the name.
    """

    if ":" in src:
        fail("`src` cannot be a target. Found `{}` for `src`".format(src))
    if not src.endswith(".bxl"):
        fail("`src` must end in '.bxl'. Found `{}` for `src`".format(src))
    export_file_name = src + ".export_file"
    export_file(name = export_file_name, src = src, mode = "reference")

    # This is ugly but needed for buck1 compatibility
    cell = native.repository_name()[1:]
    base_path = native.package_name()
    bxl_main = "{}//{}/{}:test".format(cell, base_path, src)

    if not name:
        name = src

    buck2_e2e_test(
        name = name,
        srcs = {"fbcode//buck2/tests/e2e_util:test_bxl_template.py": "test_bxl_template.py"},
        env = {
            "BXL_MAIN": bxl_main,
            # This env var is used to properly declare a dep on the src file.
            # I didn't use `resources` or `deps` because attaching to an env var makes debugging easier if needed.
            "_BXL_SRC": "$(location :{})".format(export_file_name),
        },
        # fbcode_macros uses tags instead of labels
        tags = ["bxl_test"] + (labels if labels else []),
        test_with_compiled_buck2 = False,
        test_with_deployed_buck2 = True,
        **kwargs
    )
