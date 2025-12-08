#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import dotslash

dotslash.export_multi_platform_python(
    target="prelude//apple/tools/code_signing_using_manifest:codesign_bundle_using_manifest",
    install_platforms={
        dotslash.InstallPlatform.MAC_AARCH64,
        dotslash.InstallPlatform.MAC_X86_64,
    },
    destination_files=["code_signing_using_manifest"],
    oncall="apple_workflows",
)
