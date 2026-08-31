# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:cxx_context.bzl", "get_cxx_toolchain_info")
load(
    "@prelude//cxx:cxx_library_utility.bzl",
    "cxx_is_gnu",
)

# Sections are added after BOLT but before build info stamping, so a target
# with `elf_sections` carries this suffix outside of `PRE_STAMPED_SUFFIX`.
PRE_ADD_ELF_SECTIONS_SUFFIX = "-pre_sectioned"

def _validate_section_name(name: str):
    if not name:
        fail("ELF section names must not be empty")
    if "=" in name:
        fail("ELF section names must not contain `=`: `{}`".format(name))

def add_elf_sections(ctx: AnalysisContext, obj: Artifact, sections: dict[str, Artifact], output: Artifact, category: str) -> Artifact:
    """
    Copy `obj` to `output`, adding each entry of `sections` as an ELF section
    named by its key holding the contents of its value.

    Sections added this way are not allocated, so they cost file size but not
    resident memory in the running binary.
    """
    toolchain = get_cxx_toolchain_info(ctx)

    cmd = cmd_args(toolchain.binary_utilities_info.objcopy)
    for name, content in sections.items():
        _validate_section_name(name)
        cmd.add("--add-section", cmd_args(content, format = "{}={{}}".format(name)))
    cmd.add(obj, output.as_output())

    # objcopy rewrites the whole file. This can be run remotely, but it's often
    # cheaper to do it locally for large binaries, especially on CI using
    # limited hybrid.
    prefer_local = not getattr(ctx.attrs, "optimize_for_action_throughput", False)

    ctx.actions.run(
        cmd,
        identifier = obj.short_path,
        category = category,
        prefer_local = prefer_local,
        prefer_remote = not prefer_local,
        allow_cache_upload = toolchain.cxx_compiler_info.allow_cache_upload,
    )

    return output

def get_elf_sections(ctx: AnalysisContext) -> dict[str, Artifact]:
    """
    The rule's `elf_sections`, or empty for rules that do not have the attr.
    """
    return getattr(ctx.attrs, "elf_sections", None) or {}

def add_elf_sections_to_executable(ctx: AnalysisContext, obj: Artifact, has_content_based_path: bool = False) -> Artifact:
    """
    Add the rule's `elf_sections` to a linked executable, if it has any.

    `obj` is expected to still carry `PRE_ADD_ELF_SECTIONS_SUFFIX`; the result
    drops it, so the next stage of the link sees the name it would have seen had
    the target declared no sections.
    """
    sections = get_elf_sections(ctx)
    if not sections:
        return obj

    if not cxx_is_gnu(ctx):
        fail("`elf_sections` is only supported for GNU ELF binaries")

    stem, ext = paths.split_extension(obj.short_path)
    if not stem.endswith(PRE_ADD_ELF_SECTIONS_SUFFIX):
        fail("expected `{}` to end in `{}`".format(obj.short_path, PRE_ADD_ELF_SECTIONS_SUFFIX))

    return add_elf_sections(
        ctx,
        obj,
        sections,
        ctx.actions.declare_output(stem.removesuffix(PRE_ADD_ELF_SECTIONS_SUFFIX) + ext, has_content_based_path = has_content_based_path),
        category = "add_elf_sections",
    )
