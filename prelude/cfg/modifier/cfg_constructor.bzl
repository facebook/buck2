# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//utils:graph_utils.bzl", "post_order_traversal")
load(
    ":common.bzl",
    "get_constraint_setting_deps",
    "get_modifier_info",
    "json_to_tagged_modifiers",
    "modifier_to_refs",
    "resolve_alias",
    "resolve_modifier",
)
load(":name.bzl", "cfg_name")
load(
    ":types.bzl",
    "Modifier",  # @unused
    "ModifierCliLocation",
    "ModifierTargetLocation",
    "TaggedModifiers",
)

PostConstraintAnalysisParams = record(
    legacy_platform = PlatformInfo | None,
    # Merged modifier from PACKAGE, target, and cli modifiers.
    merged_modifiers = list[TaggedModifiers],
)

def cfg_constructor_pre_constraint_analysis(
        *,
        legacy_platform: PlatformInfo | None,
        # dict[str, typing.Any] is JSON dictionary form of `TaggedModifier` passed from buck2 core
        package_modifiers: list[dict[str, typing.Any]] | None,
        # typing.Any is JSON form of modifier
        target_modifiers: list[Modifier] | None,
        cli_modifiers: list[str],
        rule_name: str,
        aliases: struct,
        **_kwargs) -> (list[str], PostConstraintAnalysisParams):
    """
    First stage of cfg constructor for modifiers.

    Args:
        legacy_platform:
            PlatformInfo from legacy target platform resolution, if one is specified
        package_modifiers:
            A list of modifiers specified from all parent PACKAGE files
        target_modifier:
            A list of modifiers specified from buildfile via `metadata` attribute.
        cli_modifiers:
            modifiers specified from `--modifier` flag, `?modifier`, or BXL
        aliases:
            A struct that contains mapping of modifier aliases to modifier.

    Returns `(refs, PostConstraintAnalysisParams)`, where `refs` is a list of fully qualified configuration
    targets we need providers for.
    """
    package_modifiers = package_modifiers or []
    target_modifiers = target_modifiers or []

    # Convert JSONs back to TaggedModifiers
    package_modifiers = [json_to_tagged_modifiers(modifier_json) for modifier_json in package_modifiers]

    # Filter PACKAGE modifiers based on rule name.
    # This only filters out PACKAGE modifiers from `extra_cfg_modifiers_per_rule` argument of `set_cfg_modifiers` function.
    package_modifiers = [tagged_modifiers for tagged_modifiers in package_modifiers if tagged_modifiers.rule_name == None or tagged_modifiers.rule_name == rule_name]
    merged_modifiers = package_modifiers

    # Add target modifiers as `TaggedModifiers`
    if target_modifiers:
        merged_modifiers.append(TaggedModifiers(modifiers = target_modifiers, location = ModifierTargetLocation(), rule_name = None))

    # Resolve all aliases in CLI modifiers
    cli_modifiers = [resolved_modifier for modifier in cli_modifiers for resolved_modifier in resolve_alias(modifier, aliases)]

    # Convert CLI modifiers to `TaggedModifier`
    if cli_modifiers:
        merged_modifiers.append(TaggedModifiers(modifiers = cli_modifiers, location = ModifierCliLocation(), rule_name = None))

    refs = []
    for tagged_modifiers in merged_modifiers:
        for modifier in tagged_modifiers.modifiers:
            refs.extend(modifier_to_refs(modifier, tagged_modifiers.location))

    return refs, PostConstraintAnalysisParams(
        legacy_platform = legacy_platform,
        merged_modifiers = merged_modifiers,
    )

def cfg_constructor_post_constraint_analysis(
        *,
        refs: dict[str, ProviderCollection],
        params: PostConstraintAnalysisParams) -> PlatformInfo:
    """
    Second stage of cfg constructor for modifiers.

    Args:
        refs: a dictionary of fully qualified target labels for configuration targets with their providers
        params: `PostConstraintAnalysisParams` returned from first stage of cfg constructor

    Returns a PlatformInfo
    """

    if not params.merged_modifiers:
        # If there is no modifier and legacy platform is specified,
        # then return the legacy platform as is without changing the label or
        # configuration.
        return params.legacy_platform or PlatformInfo(
            # Empty configuration
            label = "<empty>",
            configuration = ConfigurationInfo(
                constraints = {},
                values = {},
            ),
        )

    constraint_setting_to_modifier_infos = {}

    if params.legacy_platform:
        for constraint_setting, constraint_value_info in params.legacy_platform.configuration.constraints.items():
            constraint_setting_to_modifier_infos[constraint_setting] = [constraint_value_info]

    for tagged_modifiers in params.merged_modifiers:
        for modifier in tagged_modifiers.modifiers:
            if modifier:
                constraint_setting_label, modifier_info = get_modifier_info(
                    refs = refs,
                    modifier = modifier,
                    location = tagged_modifiers.location,
                )
                modifier_infos = constraint_setting_to_modifier_infos.get(constraint_setting_label) or []
                modifier_infos.append(modifier_info)
                constraint_setting_to_modifier_infos[constraint_setting_label] = modifier_infos

    # Modifiers are resolved in topological ordering of modifier selects. For example, if the CPU modifier
    # is a modifier_select on OS constraint, then the OS modifier must be resolved before the CPU modifier.
    # To determine this order, we first construct a dep graph of constraint settings based on the modifier
    # selects. Then we perform a post order traversal of the said graph.
    modifier_dep_graph = {
        constraint_setting: [
            dep
            for modifier_info in modifier_infos
            for dep in get_constraint_setting_deps(modifier_info)
        ]
        for constraint_setting, modifier_infos in constraint_setting_to_modifier_infos.items()
    }

    # For topo-sort, we need to fill in empty edges for nodes that have no deps
    for deps in modifier_dep_graph.values():
        for dep in deps:
            if dep not in modifier_dep_graph:
                modifier_dep_graph[dep] = []

    constraint_setting_order = post_order_traversal(modifier_dep_graph)

    cfg = ConfigurationInfo(
        constraints = {},
        values = {},
    )

    for constraint_setting in constraint_setting_order:
        for modifier_info in constraint_setting_to_modifier_infos.get(constraint_setting) or ():
            constraint_value = resolve_modifier(cfg, modifier_info)
            if constraint_value:
                cfg.constraints[constraint_setting] = constraint_value

    name = cfg_name(cfg)
    return PlatformInfo(
        label = name,
        configuration = cfg,
    )
