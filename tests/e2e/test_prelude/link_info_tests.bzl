# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:asserts.bzl", "asserts")
load("@prelude//cxx:cxx_toolchain_types.bzl", "LinkerType", "PicBehavior")
load(
    "@prelude//linking:link_info.bzl",
    "Archive",
    "ArchiveContentsType",
    "ArchiveLinkable",
    "DepMetadata",
    "LibOutputStyle",
    "LinkArgs",
    "LinkInfo",
    "LinkStrategy",
    "SharedLibLinkable",
    "get_lib_output_style",
    "link_args_metadata_with_flag",
    "link_info_to_args",
    "unpack_link_args",
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

def _test_link_info_append_impl(ctx: AnalysisContext) -> list[Provider]:
    artifacts = {
        name: ctx.actions.write(name, "", has_content_based_path = False)
        for name in [
            "thin.a",
            "thin.o",
            "whole.a",
            "libshared.so",
            "pre-hidden",
            "post-hidden",
            "ignored",
        ]
    }
    infos = [
        LinkInfo(
            pre_flags = [cmd_args("pre one", format = "<{}>", hidden = artifacts["pre-hidden"])],
            linkables = [
                ArchiveLinkable(
                    archive = Archive(
                        archive_contents_type = ArchiveContentsType("thin"),
                        artifact = artifacts["thin.a"],
                        external_objects = [artifacts["thin.o"]],
                    ),
                    linker_type = LinkerType("gnu"),
                )
            ],
            post_flags = [cmd_args("post one", quote = "shell", hidden = artifacts["post-hidden"])],
            metadata = [DepMetadata(version = "one")],
        ),
        LinkInfo(
            pre_flags = ["pre two", cmd_args(artifacts["ignored"], ignore_artifacts = True)],
            linkables = [
                ArchiveLinkable(
                    archive = Archive(artifact = artifacts["whole.a"]),
                    linker_type = LinkerType("gnu"),
                    link_whole = True,
                ),
                SharedLibLinkable(lib = artifacts["libshared.so"], link_without_soname = True),
            ],
            post_flags = ["post two"],
            metadata = [DepMetadata(version = "two")],
        ),
    ]
    link_args = LinkArgs(infos = infos)
    nested = link_args_metadata_with_flag(link_args, "--metadata")
    nested.add([link_info_to_args(info) for info in infos])
    appended = unpack_link_args(link_args, link_metadata_flag = "--metadata")

    # Hidden inputs do not appear in the rendered command line.
    expected_inputs = cmd_args([artifact for name, artifact in artifacts.items() if name != "ignored"]).inputs
    asserts.equals(expected_inputs, nested.inputs)
    asserts.equals(expected_inputs, appended.inputs)
    asserts.equals(nested.inputs, appended.inputs)
    asserts.equals([], appended.outputs)

    expected_commands = {"unpacked": nested}
    actual_commands = {"unpacked": appended}
    for name, prefix in [("empty_destination", []), ("prefilled_destination", ["prefix"])]:
        destination = cmd_args(prefix)
        for info in infos:
            link_info_to_args(info, args = destination)
        # An empty LinkInfo must preserve and return the supplied destination.
        returned = link_info_to_args(LinkInfo(), args = destination)
        returned.add("suffix")
        expected_commands[name] = cmd_args(prefix, [link_info_to_args(info) for info in infos], "suffix")
        actual_commands[name] = destination
        asserts.equals(expected_inputs, destination.inputs)

    nested_json = ctx.actions.write_json("nested.json", expected_commands)
    appended_json = ctx.actions.write_json("appended.json", actual_commands)
    return [
        DefaultInfo(default_outputs = [nested_json, appended_json]),
        ExternalRunnerTestInfo(type = "custom", command = ["diff", "-u", nested_json, appended_json]),
    ]

test_link_info_append = rule(impl = _test_link_info_append_impl, attrs = {})
