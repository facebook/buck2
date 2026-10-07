# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# Rule extensions: functions supplied by a target that run after a prelude rule's
# own implementation and attach extra validations to the target being analysed.

RuleExtensionInfo = provider(fields = {
    # Called as `func(actions, label, attrs, providers) -> list[ValidationSpec]`.
    "func": provider_field(typing.Callable),
    # Prelude rule names (e.g. `cxx_library`) the extension applies to.
    "rules": provider_field(list[str]),
})

RuleExtensionsInfo = provider(fields = {
    "extensions": provider_field(list[RuleExtensionInfo]),
})

RULE_EXTENSIONS_ATTR = "_rule_extensions"
RULE_EXTENSIONS_DEFAULT = "prelude//rule_extensions:none"

def run_with_extensions(impl: typing.Callable, rule_type: str, ctx: AnalysisContext):
    providers = impl(ctx)
    extensions = [
        extension
        for extension in getattr(ctx.attrs, RULE_EXTENSIONS_ATTR)[RuleExtensionsInfo].extensions
        if rule_type in extension.rules
    ]
    if not extensions:
        return providers
    if type(providers) != "list":
        fail("Rule extensions require `{}` to return a list of providers".format(rule_type))

    specs = []
    for extension in extensions:
        specs.extend(extension.func(ctx.actions, ctx.label, ctx.attrs, providers))
    if not specs:
        return providers

    existing = [p.validations for p in providers if isinstance(p, ValidationInfo)]
    others = [p for p in providers if not isinstance(p, ValidationInfo)]
    return others + [ValidationInfo(validations = (existing[0] if existing else []) + specs)]

def _rule_extensions_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        RuleExtensionsInfo(extensions = [e[RuleExtensionInfo] for e in ctx.attrs.extensions]),
    ]

rule_extensions = rule(
    impl = _rule_extensions_impl,
    attrs = {
        "extensions": attrs.list(attrs.toolchain_dep(providers = [RuleExtensionInfo]), default = []),
    },
    is_toolchain_rule = True,
)
