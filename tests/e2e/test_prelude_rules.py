# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test


async def _values(buck: Buck, package: str) -> dict[str, str]:
    """The `value` attribute of every target in the package, by target name."""
    res = await buck.uquery("--output-attribute", "value", f"root//{package}:")
    return {
        target.split(":")[1]: attrs["value"]
        for target, attrs in json.loads(res.stdout).items()
    }


# prelude/apple/apple_bundle_destination.bzl `bundle_relative_path_for_destination`
@buck_test(inplace=False)
async def test_extensionkit_extensions_destination(buck: Buck) -> None:
    assert await _values(buck, "bundle") == {
        "iphoneos_app_extensionkit": "Extensions",
        "macosx_app_extensionkit": "Contents/Extensions",
        "maccatalyst_app_extensionkit": "Contents/Extensions",
    }


# prelude/cxx/headers.bzl `_infer_include_prefix_from_paths`
@buck_test(inplace=False)
async def test_include_prefix_needs_a_whole_path_suffix(buck: Buck) -> None:
    assert await _values(buck, "headers") == {
        "renamed_header_prefix": "None",
        "subdir_header_prefix": "third-party/foo/config",
    }
