# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//utils/value.bzl", "GenericValueInfo")
load("@prelude//python:toolchain.bzl", "PythonToolchainInfo")

# Stores a variable which will be searched for in the cmake configure file
# template as well as a value to replace that variable with during substitution.
CMakeSubstitutionInfo = provider(
    fields = {
        "variable": provider_field(str),
        "value": provider_field(typing.Any)
    }
)

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
        entry = {
            "value": info.value,
            "type": "embed" if isinstance(info.value, Artifact) else "subst"
        }

        substitution_dictionary[info.variable] = entry

    config_json = ctx.actions.write_json("config.json", substitution_dictionary, with_inputs=True, pretty=True)
    args.extend([
        "--substitution-file",
        config_json
    ])

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

def _cmake_substitution_impl_internal(name:str, variable:str|None, value) -> list[Provider]:
    if variable == None:
        variable = name


    return [
        DefaultInfo(),
        CMakeSubstitutionInfo(
            variable = variable,
            value = value
        )
    ]

def _get_integer_value(value:Dependency|None|int) -> int|None:
    if isinstance(value, Dependency):
        return _get_integer_value(value[GenericValueInfo].value)

    if isinstance(value, int):
        return value

    if value == None:
        return value

    fail(f"Unsupported value type {value} for integral argument")
    return None

def cmake_type_size_substitution_impl(ctx: AnalysisContext) -> list[Provider]:
    variable = ctx.attrs.variable or ctx.attrs.name

    key = f"{variable}_CODE"

    size = _get_integer_value(ctx.attrs.size)
    if size == None:
        value = None
    else:
        value = f"#define {variable} {size}"
    return _cmake_substitution_impl_internal(ctx.attrs.name, key, value)

def cmake_substitution_impl(ctx: AnalysisContext) -> list[Provider]:
    return _cmake_substitution_impl_internal(ctx.attrs.name, ctx.attrs.variable, ctx.attrs.value[GenericValueInfo].value)

def cmake_immediate_substitution_impl(ctx: AnalysisContext) -> list[Provider]:
    return _cmake_substitution_impl_internal(ctx.attrs.name, ctx.attrs.variable, ctx.attrs.value)
