# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//user:rule_spec.bzl", "RuleRegistrationSpec")

_execution_types = ["launch", "test", "profile"]

XcodeSchemeSettingsInfo = provider(
    fields = {
        "json": provider_field(Artifact),
    },
)

XCODE_SCHEME_SETTINGS_ATTR_NAME = "xcode_scheme_settings"
XCODE_SCHEME_SETTINGS_ATTR_TYPE = attrs.option(attrs.dep(providers = [XcodeSchemeSettingsInfo]), default = None, doc = "Optional settings to set on schemes when this target is represented in Xcode.")

def _impl(ctx: AnalysisContext) -> list[Provider]:
    data = {}
    for execution_type in _execution_types:
        execution_settings = getattr(ctx.attrs, execution_type, None)
        if execution_settings:
            data[execution_type] = execution_settings

    scheme_settings_json = ctx.actions.write_json("scheme_settings.json", data)

    return [DefaultInfo(default_output = scheme_settings_json), XcodeSchemeSettingsInfo(json = scheme_settings_json)]

def _attribs():
    """
    Example:

    xcode_scheme_settings(
        name = "ExampleSchemeSettings",
        launch = {
            "command_line_arguments": [
                ("-NSDoubleLocalizedStrings YES", False),
            ],
            "environment_variables": [
                ("IDELogRedirectionPolicy", "oslogToStdio", False)
            ],
        },
    )
    """

    command_line_argument_attr = attrs.tuple(attrs.string(), attrs.bool(), doc = "The command line argument and whether it is enabled by default. Example: ('-NSDoubleLocalizedStrings YES', False)")
    environment_variable_attr = attrs.tuple(attrs.string(), attrs.string(), attrs.bool(), doc = "The environment variable (key, value) and whether it is enabled by default. Example: ('IDELogRedirectionPolicy', 'oslogToStdio', False)")
    setting_type = attrs.enum(["command_line_arguments", "environment_variables"])

    execution_attr = attrs.dict(
        setting_type,
        attrs.one_of(
            attrs.list(command_line_argument_attr, default = []),
            attrs.list(environment_variable_attr, default = []),
        ),
    )

    attribs = {execution_type: attrs.option(execution_attr, default = None, doc = "Xcode {} scheme settings".format(execution_type)) for execution_type in _execution_types}
    return attribs

registration_spec = RuleRegistrationSpec(
    name = "xcode_scheme_settings",
    impl = _impl,
    attrs = _attribs(),
)
