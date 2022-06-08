# Copyright 2004-present Facebook. All Rights Reserved.
load("@fbcode_macros//build_defs:rust_binary.bzl", "rust_binary")
load("@fbcode_macros//build_defs/lib:cpp_common.bzl", "cpp_common")

def buck_rust_binary(**kwargs):
    kwargs.setdefault("edition", "2021")
    if kwargs.get("link_style") == None:
        link_style = cpp_common.get_link_style()
        kwargs["link_style"] = select({
            "DEFAULT": link_style,
            "ovr_config//os:macos": "static",
        })

    # JEMalloc is not (yet!) the default on MacOS so add the allocator
    # explicitly on all platforms here.
    kwargs.setdefault("allocator", "jemalloc")
    rust_binary(**kwargs)
