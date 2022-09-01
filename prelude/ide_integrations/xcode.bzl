XCODE_DATA_SUB_TARGET = "xcode-data"
_XCODE_DATA_FILE_NAME = "xcode_data.json"

XcodeDataInfo = provider(fields = [
    "data",  # {str.type: _a}
])

def generate_xcode_data(
        ctx: "context",
        rule_type: str.type,
        output: ["artifact", None],
        populate_rule_specific_attributes_func: ["function", None] = None,
        **kwargs) -> (["DefaultInfo"], XcodeDataInfo.type):
    data = {
        "rule_type": rule_type,
        "target": ctx.label,
    }
    if output:
        data["output"] = output
    if populate_rule_specific_attributes_func:
        data.update(populate_rule_specific_attributes_func(ctx, **kwargs))

    json_file = ctx.actions.write_json(_XCODE_DATA_FILE_NAME, data)
    return [DefaultInfo(default_outputs = [json_file])], XcodeDataInfo(data = data)
