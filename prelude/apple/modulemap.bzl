# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolsInfo")
load(
    "@prelude//cxx:headers.bzl",
    "CHeader",  # @unused Used as a type
)
load(
    "@prelude//cxx:preprocessor.bzl",
    "CPreprocessor",
    "CPreprocessorArgs",
)

def preprocessor_info_for_modulemap(
        ctx: AnalysisContext,
        name: str,
        module_name: str,
        headers: list[CHeader],
        swift_header: Artifact | None,
        mark_headers_private: bool,
        additional_args: CPreprocessorArgs | None) -> CPreprocessor:
    preprocessor_info, _ = create_modulemap(ctx, name, module_name, headers, swift_header, mark_headers_private, additional_args)
    return preprocessor_info

def create_modulemap(
        ctx: AnalysisContext,
        name: str,
        module_name: str,
        headers: list[CHeader],
        swift_header: Artifact | None,
        mark_headers_private: bool,
        additional_args: CPreprocessorArgs | None,
        is_framework: bool = False) -> (CPreprocessor, Artifact):
    # We don't want to name this module.modulemap to avoid implicit importing
    if name == "module" and not is_framework:
        fail("Don't use the name `module` for modulemaps, this will allow for implicit importing.")

    # Create a map of header import path to artifact location
    header_map = {}
    for h in headers:
        if h.namespace:
            header_map["{}/{}".format(h.namespace, h.name)] = h.artifact
        else:
            header_map[h.name] = h.artifact

    swift_header_name = None
    if swift_header:
        # We need to include the Swift header in the symlink tree too
        swift_header_name = "{}/{}-Swift.h".format(module_name, module_name)
        header_map[swift_header_name] = swift_header

        if mark_headers_private:
            fail("You shouldn't be generating a bridging header for a private module map.")

    # Create a symlink dir for the headers to import
    symlink_tree = ctx.actions.symlinked_dir(name.replace(".", "_") + "_symlink_tree", header_map)

    # Create a modulemap at the root of that tree
    output = ctx.actions.declare_output(name + ".modulemap")
    cmd = cmd_args(ctx.attrs._apple_tools[AppleToolsInfo].make_modulemap)
    cmd.add([
        "--output",
        output.as_output(),
        "--name",
        module_name,
        "--symlink-tree",
        symlink_tree,
    ])

    if swift_header:
        cmd.add([
            "--swift-header",
            swift_header,
        ])

    if getattr(ctx.attrs, "use_submodules", False):
        cmd.add("--use-submodules")

    if is_framework:
        cmd.add("--framework")

    for hdr in sorted(header_map.keys()):
        # Don't include the Swift header in the mappings, this is handled separately.
        if hdr != swift_header_name:
            cmd.add(hdr)

    if mark_headers_private:
        cmd.add("--mark-headers-private")

    ctx.actions.run(cmd, category = "modulemap", identifier = name)

    return CPreprocessor(
        args = CPreprocessorArgs(
            args = [cmd_args(symlink_tree, format = "-I{}")] + (additional_args.args if additional_args else []),
            file_prefix_args = additional_args.file_prefix_args if additional_args else [],
        ),
        modular_args = [cmd_args(output, format = "-fmodule-map-file={}")],
        modulemap_path = cmd_args(output),
        modulemap_artifacts = [output],
    ), output
