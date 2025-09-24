# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":cctest.bzl", "CcTestValueInfo", "CcTestTypeSizeInfo")
load("@prelude//python:toolchain.bzl", "PythonToolchainInfo")

# Provider used by embedded_file rule, which allows replacing a cmake
# substitution variable with content from another file.
CMakeEmbeddedFileInfo = provider(
    fields = {
        "files": provider_field(typing.Any, default = None),
        "prefix": provider_field(typing.Any, default = None),
        "suffix": provider_field(typing.Any, default = None),
        "item_prefix": provider_field(typing.Any, default = None),
        "item_suffix": provider_field(typing.Any, default = None),
        "delimiter": provider_field(typing.Any, default = None),
        "trailing_delimiter": provider_field(typing.Any, default = None),
    }
)

# Stores a variable which will be searched for in the cmake configure file
# template as well as a value to replace that variable with during substitution.
CMakeSubstitutionInfo = provider(
    fields = {
        "variable": provider_field(str),
        "value": provider_field(typing.Any)
    }
)

def _cmake_substitution_impl_internal(name:str, value) -> list[Provider]:
    if isinstance(value, Dependency):
        value = value[CcTestValueInfo].value

    return [
        DefaultInfo(default_outputs = []),
        CMakeSubstitutionInfo(
            variable = name,
            value = value
        )
    ]

def cmake_configure_file_impl(ctx: AnalysisContext) -> list[Provider]:
    output_dir = ctx.actions.declare_output("out", dir = True)
    output_file = output_dir.project(ctx.attrs.output if ctx.attrs.output else ctx.label.name)

    script_run_info = ctx.attrs.script[RunInfo]
    args = [
        script_run_info,
        "--input", ctx.attrs.template,
        "--output", output_file.as_output(),
    ]
    hidden = []
    print(ctx.attrs.strict)
    if ctx.attrs.strict:
        args.append("--strict")
    if ctx.attrs.at_sub:
        args.append("--enable-at-replacements")
    if ctx.attrs.var_sub:
        args.append("--enable-var-replacements")
    if ctx.attrs.escape_quotes:
        args.append("--escape-quotes")
    if ctx.attrs.copy_only:
        args.append("--copy-only")

    substitution_dictionary = {}

    for sub in ctx.attrs.substitutions:
        info = sub[CMakeSubstitutionInfo]
        substitution_dictionary[info.variable] = info.value

    config_json = ctx.actions.write_json("config.json", substitution_dictionary, pretty=True)
    args.extend([
        "--substitution-file",
        config_json
    ])

    for key, value in ctx.attrs.embeddings.items():
        value = value[CMakeEmbeddedFileInfo]
        action = {
            "label": key,
            "files": value.files,
            "prefix": value.prefix,
            "suffix": value.suffix,
            "item_prefix": value.item_prefix,
            "item_suffix": value.item_suffix,
            "delimiter": value.delimiter,
            "trailing_delimiter": value.trailing_delimiter,
        }
        embedding_json_name = "{}.embedding.json".format(key)
        embedding_json = ctx.actions.write_json(embedding_json_name, action, pretty=True)
        args.append("--embeddings")
        args.append(embedding_json)
        hidden.extend(value.files)

    ctx.actions.run(
        cmd_args(args, hidden=hidden),
        category = "cmake_configure_file",
        identifier = ctx.attrs.template.short_path(),
    )

    sub_targets = {"outdir": [DefaultInfo(default_outputs=[output_dir])]}
    return [
        DefaultInfo(
            default_outputs = [output_file],
            sub_targets = sub_targets
        )
    ]

def cmake_embedding_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        CMakeEmbeddedFileInfo(
            files = ctx.attrs.files,
            prefix = ctx.attrs.prefix,
            suffix = ctx.attrs.suffix,
            item_prefix = ctx.attrs.item_prefix,
            item_suffix = ctx.attrs.item_suffix,
            delimiter = ctx.attrs.delimiter,
            trailing_delimiter = ctx.attrs.trailing_delimiter
        )
    ]

def cmake_substitution_impl(ctx: AnalysisContext) -> list[Provider]:
    return _cmake_substitution_impl_internal(ctx.attrs.name, ctx.attrs.value)

def cmake_type_size_substitution_impl(ctx: AnalysisContext) -> list[Provider]:
    name = ctx.attrs.name
    size = ctx.attrs.value[CcTestTypeSizeInfo].size
    return _cmake_substitution_impl_internal(f"{name}_CODE", f"#define {name} {size}")

