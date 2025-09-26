# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

GenericValueInfo = provider(
    fields = {
        "value": provider_field(typing.Any)
    }
)

def _make_value(v) -> GenericValueInfo:
    if isinstance(v, Dependency):
        if GenericValueInfo in v:
            return v[GenericValueInfo]

    if not isinstance(v, [int, bool, str, None, Artifact]):
        actual = type(v)
        fail(f"Unsupported type {actual} for generic simple value.  Support types are [int, bool, str]")
    return GenericValueInfo(value = v)

def generic_simple_value_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        _make_value(ctx.attrs.value)
    ]

def generic_list_value_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        GenericValueInfo(value = [_make_value(v).value for v in ctx.attrs.values])
    ]

def generic_file_value_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        GenericValueInfo(value = ctx.attrs.file)
    ]

def generic_value_mapping_impl(ctx: AnalysisContext) -> list[Provider]:
    original = ctx.attrs.original[GenericValueInfo].value

    if original not in ctx.attrs.new:
        fail(f"Value {original} does not contain a mapping.")

    return [
        DefaultInfo(),
        _make_value(ctx.attrs.new[original])
    ]

def _collect_dynamic_inputs(outputs:list, inputs: list):
    for v in inputs:
        inner_value = v
        if isinstance(inner_value, Dependency):
            inner_value = v[GenericValueInfo].value

        if isinstance(inner_value, list):
            _collect_dynamic_inputs(outputs, inner_value)
        elif isinstance(inner_value, Artifact):
            outputs.append(inner_value)

def _get_artifact_content(artifacts, item:Artifact) -> str:
        return artifacts[item].read_string()

def _get_dependency_content(artifacts, item) -> str:
    if isinstance(item, Dependency):
        value = item[GenericValueInfo].value
    else:
        value = item

    if isinstance(value, Artifact):
        return _get_artifact_content(artifacts, value)
    elif isinstance(value, Dependency):
        return _get_dependency_content(artifacts, value)
    elif isinstance(value, GenericValueInfo):
        return str(value.value)
    else:
        return str(value)


def _flatten(items:list) -> list:
    result = []

    for item in items:
        if isinstance(item, list):
            result += _flatten(item)
        elif isinstance(item, Dependency):
            result += _flatten(item[GenericValueInfo].value)
        elif isinstance(item, GenericValueInfo):
            result.append(item)
        else:
            result.append(GenericValueInfo(value=item))

    return result

def generic_value_join_list_impl(ctx: AnalysisContext) -> list[Provider]:
    output = ctx.actions.declare_output("joined_content.txt")
    
    flattened_values = _flatten(ctx.attrs.values)

    def _impl(ctx, artifacts, outputs, flattened_values=flattened_values, output=output):
        content = ""
        if ctx.attrs.prefix:
            content += ctx.attrs.prefix
        
        
        count = len(flattened_values)
        index = 0
        for value in flattened_values:
            if ctx.attrs.item_prefix:
                content += ctx.attrs.item_prefix
            content += _get_dependency_content(artifacts, value)
            if ctx.attrs.item_suffix:
                content += ctx.attrs.item_suffix

            if index < (count-1) and ctx.attrs.delimiter:
                content += ctx.attrs.delimiter
            index += 1
        
        if ctx.attrs.suffix:
            content += ctx.attrs.suffix

        ctx.actions.write(
            outputs[output],
            content
        )

    dynamic = []
    _collect_dynamic_inputs(dynamic, flattened_values)

    ctx.actions.dynamic_output(
        dynamic = dynamic,
        inputs = [],
        outputs = [output.as_output()],
        f = _impl
    )

    return [
        DefaultInfo(default_outputs=[output]),
        _make_value(output)
    ]
