# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":types.bzl", "ConditionalModifierInfo", "Modifier", "ModifiersMatch", "ModifiersMatchInfo")

def _get_constraint_setting(constraint_settings: dict[TargetLabel, None], modifier: ModifiersMatch) -> TargetLabel:
    if len(constraint_settings) == 0:
        fail("Conditional modifier cannot be empty. Found empty modifier `{}`".format(modifier))
    if len(constraint_settings) > 1:
        fail(
            "A single modifier can only modify a single constraint setting.\n" +
            "Modifier `{}` is found to modify the following constraint settings:\n".format(
                modifier,
            ) + "\n".join([str(k) for k in constraint_settings.keys()]),
        )
    return list(constraint_settings.keys())[0]

def get_modifier_info(
        modifier: Modifier,
        key_to_provider: dict[str, ConfigurationInfo],
        value_to_provider: dict[str, ConditionalModifierInfo]) -> ConditionalModifierInfo | None:
    """
    Converts a modifier to a provider based on providers for keys and values.
    """
    if modifier == None:
        return None

    if isinstance(modifier, str):
        return value_to_provider[modifier]

    default = None
    modifiers_match_info = []
    constraint_settings = {}  # Used like a set

    for key, sub_modifier in modifier.items():
        if key == "DEFAULT":
            if sub_modifier:
                sub_modifier_info = get_modifier_info(sub_modifier, key_to_provider, value_to_provider)
                constraint_settings[sub_modifier_info.key] = None
                default = sub_modifier_info.inner
        else:
            cfg_info = key_to_provider[key]
            if sub_modifier:
                sub_modifier_info = get_modifier_info(sub_modifier, key_to_provider, value_to_provider)
                constraint_settings[sub_modifier_info.key] = None
                sub_modifier_info = sub_modifier_info.inner
            else:
                sub_modifier_info = None
            modifiers_match_info.append((cfg_info, sub_modifier_info))

    constraint_setting = _get_constraint_setting(constraint_settings, modifier)
    return ConditionalModifierInfo(
        key = constraint_setting,
        inner = ModifiersMatchInfo(
            default = default,
            selector = modifiers_match_info,
        ),
    )

def _impl(ctx: AnalysisContext) -> list[Provider]:
    key_to_provider = {str(dep.label.raw_target()): dep.get(ConfigurationInfo) for dep in ctx.attrs._conditional_keys}
    value_to_provider = {str(dep.label.raw_target()): dep.get(ConditionalModifierInfo) for dep in ctx.attrs._conditional_values}
    conditional_modifier_info = get_modifier_info(ctx.attrs.modifier, key_to_provider, value_to_provider)
    return [DefaultInfo(), conditional_modifier_info]

_conditional_modifier = rule(
    impl = _impl,
    doc = """
        A `_conditional_modifier` rule instantiates a conditional modifier that can be set on a target.
        This is a private rule. You want to instantiate it through  the `conditional_modifier` function wrapper.

        The conditional modifier allows one to express a modifier in a dictionary form similar to a select, where
        each key in the dictionary is a set of constraints to match on and each value in the dictionary is a modifier. 

        For example, below is a modifier for expressing "msvc if windows else clang".

        ```python
        conditional_modifier(
            name = "clang-msvc",
            modifier = {
                "DEFAULT": "prelude//constraints/compiler:clang",
                "prelude//constraints/os:windows": "prelude//constraints/compiler:msvc", 
            },
        )
        ```

        A conditional modifier can be nested similar to a nested select. 

        ```python
        conditional_modifier(
            name = "clang-msvc-gcc",
            modifier = {
                "DEFAULT": {
                    "DEFAULT": "prelude//constraints/compiler:clang",
                    "prelude//constraints/os:linux": "prelude//constraints/compiler:gcc",
                },
                "prelude//constraints/os:windows": "prelude//constraints/compiler:msvc",
            },
        )
        ```

        A conditional modifier can also take in another conditional modifier. For example, the modifier from the
        previous example can also be expressed as follows.

        ```python
        conditional_modifier(
            name = "clang-msvc-gcc2",
            modifier = {
                "DEFAULT": ":clang-msvc":
                "prelude//constraints/os:linux": "prelude//constraints/compiler:gcc",
            }
        )
        ```
    """,
    attrs = {
        "modifier": attrs.any(doc = "A conditional modifier to set"),
        "_conditional_keys": attrs.list(attrs.dep(providers = [ConfigurationInfo]), doc = "internal attribute"),
        "_conditional_values": attrs.list(attrs.dep(providers = [ConditionalModifierInfo]), doc = "internal attribute"),
    },
    is_configuration_rule = True,
)

def _get_conditional_keys(modifier: ModifiersMatch) -> list[str]:
    result = [k for k in modifier.keys() if k != "DEFAULT"]
    for v in modifier.values():
        if isinstance(v, dict):
            result += _get_conditional_keys(v)
        elif isinstance(v, str):
            result.append(v)
        elif v:
            fail("`{}` is not a valid modifier".format(v))
    return result

def _get_conditional_values(modifier: ModifiersMatch) -> list[str]:
    result = []
    for v in modifier.values():
        if isinstance(v, dict):
            result += _get_conditional_values(v)
        elif isinstance(v, str):
            result.append(v)
        elif v:
            fail("`{}` is not a valid modifier".format(v))
    return result

def _fully_qualify(modifier: Modifier) -> Modifier:
    if modifier == None:
        return None

    if isinstance(modifier, str):
        if modifier.startswith(":"):
            return "{}//{}{}".format(get_cell_name(), package_name(), modifier)
        if modifier.startswith("//"):
            return "{}{}".format(get_cell_name(), modifier)
        return modifier

    fully_qualified = {}
    for k, v in modifier.items():
        if k == "DEFAULT":
            fully_qualified[k] = _fully_qualify(v)
        else:
            fully_qualified[_fully_qualify(k)] = _fully_qualify(v)
    return fully_qualified

def conditional_modifier(name: str, modifier: ModifiersMatch):
    _conditional_modifier(
        name = name,
        _conditional_keys = _get_conditional_keys(modifier),
        _conditional_values = _get_conditional_values(modifier),
        modifier = _fully_qualify(modifier),
    )
