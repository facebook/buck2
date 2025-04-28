# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:asserts.bzl", "asserts")
load("@prelude//cxx:cxx_toolchain_types.bzl", "PicBehavior")
load(
    "@prelude//linking:link_info.bzl",
    "LibOutputStyle",
    "LinkStrategy",
    "get_lib_output_style",
)
load("@prelude//linking:types.bzl", "Linkage")

def test_get_lib_output_style():
    # requested_link_style static
    asserts.equals(LibOutputStyle("archive"), get_lib_output_style(LinkStrategy("static"), Linkage("static"), PicBehavior("supported")))
    asserts.equals(LibOutputStyle("archive"), get_lib_output_style(LinkStrategy("static"), Linkage("static"), PicBehavior("not_supported")))
    asserts.equals(LibOutputStyle("pic_archive"), get_lib_output_style(LinkStrategy("static"), Linkage("static"), PicBehavior("always_enabled")))

    asserts.equals(LibOutputStyle("shared_lib"), get_lib_output_style(LinkStrategy("static"), Linkage("shared"), PicBehavior("supported")))
    asserts.equals(LibOutputStyle("shared_lib"), get_lib_output_style(LinkStrategy("static"), Linkage("shared"), PicBehavior("not_supported")))
    asserts.equals(LibOutputStyle("shared_lib"), get_lib_output_style(LinkStrategy("static"), Linkage("shared"), PicBehavior("always_enabled")))

    asserts.equals(LibOutputStyle("archive"), get_lib_output_style(LinkStrategy("static"), Linkage("any"), PicBehavior("supported")))
    asserts.equals(LibOutputStyle("archive"), get_lib_output_style(LinkStrategy("static"), Linkage("any"), PicBehavior("not_supported")))
    asserts.equals(LibOutputStyle("pic_archive"), get_lib_output_style(LinkStrategy("static"), Linkage("any"), PicBehavior("always_enabled")))

    # requested_link_style static_pic
    asserts.equals(LibOutputStyle("pic_archive"), get_lib_output_style(LinkStrategy("static_pic"), Linkage("static"), PicBehavior("supported")))
    asserts.equals(LibOutputStyle("archive"), get_lib_output_style(LinkStrategy("static_pic"), Linkage("static"), PicBehavior("not_supported")))
    asserts.equals(LibOutputStyle("pic_archive"), get_lib_output_style(LinkStrategy("static_pic"), Linkage("static"), PicBehavior("always_enabled")))

    asserts.equals(LibOutputStyle("shared_lib"), get_lib_output_style(LinkStrategy("static_pic"), Linkage("shared"), PicBehavior("supported")))
    asserts.equals(LibOutputStyle("shared_lib"), get_lib_output_style(LinkStrategy("static_pic"), Linkage("shared"), PicBehavior("not_supported")))
    asserts.equals(LibOutputStyle("shared_lib"), get_lib_output_style(LinkStrategy("static_pic"), Linkage("shared"), PicBehavior("always_enabled")))

    asserts.equals(LibOutputStyle("pic_archive"), get_lib_output_style(LinkStrategy("static_pic"), Linkage("any"), PicBehavior("supported")))
    asserts.equals(LibOutputStyle("archive"), get_lib_output_style(LinkStrategy("static_pic"), Linkage("any"), PicBehavior("not_supported")))
    asserts.equals(LibOutputStyle("pic_archive"), get_lib_output_style(LinkStrategy("static_pic"), Linkage("any"), PicBehavior("always_enabled")))

    # requested_link_style shared
    asserts.equals(LibOutputStyle("pic_archive"), get_lib_output_style(LinkStrategy("shared"), Linkage("static"), PicBehavior("supported")))
    asserts.equals(LibOutputStyle("archive"), get_lib_output_style(LinkStrategy("shared"), Linkage("static"), PicBehavior("not_supported")))
    asserts.equals(LibOutputStyle("pic_archive"), get_lib_output_style(LinkStrategy("shared"), Linkage("static"), PicBehavior("always_enabled")))

    asserts.equals(LibOutputStyle("shared_lib"), get_lib_output_style(LinkStrategy("shared"), Linkage("shared"), PicBehavior("supported")))
    asserts.equals(LibOutputStyle("shared_lib"), get_lib_output_style(LinkStrategy("shared"), Linkage("shared"), PicBehavior("not_supported")))
    asserts.equals(LibOutputStyle("shared_lib"), get_lib_output_style(LinkStrategy("shared"), Linkage("shared"), PicBehavior("always_enabled")))

    asserts.equals(LibOutputStyle("shared_lib"), get_lib_output_style(LinkStrategy("shared"), Linkage("any"), PicBehavior("supported")))
    asserts.equals(LibOutputStyle("shared_lib"), get_lib_output_style(LinkStrategy("shared"), Linkage("any"), PicBehavior("not_supported")))
    asserts.equals(LibOutputStyle("shared_lib"), get_lib_output_style(LinkStrategy("shared"), Linkage("any"), PicBehavior("always_enabled")))
