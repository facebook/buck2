# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@fbcode_macros//build_defs:rust_binary.bzl", "rust_binary")
load("@fbcode_macros//build_defs/lib:link_styles.bzl", "link_styles")

def buck_rust_binary(**kwargs):
    kwargs.setdefault("edition", "2021")
    if kwargs.get("link_style") == None:
        link_style = link_styles.default()
        kwargs["link_style"] = select({
            "DEFAULT": link_style,
            "ovr_config//os:macos": "static",
        })

    # JEMalloc is not (yet!) the default on MacOS so add the allocator
    # explicitly on all platforms here.
    kwargs.setdefault("allocator", "jemalloc")
    rust_binary(**kwargs)
