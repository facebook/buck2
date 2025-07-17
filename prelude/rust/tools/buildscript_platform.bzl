# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:prelude.bzl", "native")

# Used like this:
#
#     transition_alias(
#         name = "anyhow-1.0.100-build-script-build-riscv64gc",
#         actual = ":anyhow-1.0.100-build-script-build",
#         incoming_transition = ":buildscript_for_platform=[riscv64gc]",
#     )
#
# This allows `anyhow-1.0.100-build-script-run` to take an exec dependency on
# this target, transition to the build-script-run's execution platform (which
# discards knowledge of build-script-run's target platform), and transition
# again to set a riscv64gc constraint in the execution platform.
#
# [WORKAROUND] If Buck gets an "Execution Modifiers" feature, this might no
# longer be necessary. Execution modifiers are expected to persist from the
# target platform to the execution platform during an execution transition.
transition_alias = rule(
    impl = lambda ctx: ctx.attrs.actual.providers,
    attrs = {
        "actual": attrs.dep(),
    },
    supports_incoming_transition = True,
)

def _anon_constraint_value_impl(ctx: AnalysisContext) -> list[Provider]:
    constraint = ConstraintValueInfo(
        setting = ctx.attrs.constraint_setting[ConstraintSettingInfo],
        label = ctx.label.raw_target(),
    )

    def transition_impl(platform: PlatformInfo) -> PlatformInfo:
        return PlatformInfo(
            label = platform.label,
            configuration = ConfigurationInfo(
                constraints = platform.configuration.constraints | {
                    constraint.setting.label: constraint,
                },
                values = platform.configuration.values,
            ),
        )

    return [
        DefaultInfo(),
        ConfigurationInfo(
            constraints = {constraint.setting.label: constraint},
            values = {},
        ),
        TransitionInfo(impl = transition_impl),
    ]

# An anonymous value of the "prelude//rust/tools:buildscript_for_platform"
# constraint setting.
#
# This is anonymous so that different Reindeer roots using the same platforms
# end up with identical execution platforms after performing the incoming
# transition above. Otherwise, common dependencies of build scripts in two
# different packages would get configured and compiled redundantly.
_anon_constraint_value = anon_rule(
    impl = _anon_constraint_value_impl,
    attrs = {
        "constraint_setting": attrs.dep(),
    },
    artifact_promise_mappings = {},
)

def _buildscript_platform_constraints_impl(ctx: AnalysisContext) -> Promise:
    reindeer_platforms = ctx.attrs.reindeer_platforms
    anon_targets = ctx.actions.anon_targets([
        (
            _anon_constraint_value,
            {
                "constraint_setting": ctx.attrs.constraint_setting,
                "name": ":{}".format(plat),
            },
        )
        for plat in reindeer_platforms
    ])

    def process(anon_providers: list[ProviderCollection]) -> list[Provider]:
        return [
            DefaultInfo(
                sub_targets = {
                    plat: [
                        DefaultInfo(),
                        provider_collection[ConfigurationInfo],
                        provider_collection[TransitionInfo],
                    ]
                    for plat, provider_collection in zip(reindeer_platforms, anon_providers)
                },
            ),
        ]

    return anon_targets.promise.map(process)

# Subtargets such as ":buildscript_for_platform=[riscv64gc]" for each Reindeer
# platform set in the current package's PACKAGE file.
#
# This is used in `select` (as ConfigurationInfo) and in `incoming_transition`
# (as TransitionInfo).
#
#     buildscript_genrule(
#         name = "anyhow-1.0.100-build-script-run",
#         buildscript = select({
#             ":buildscript_for_platform=[riscv64gc]": ":anyhow-1.0.100-build-script-build-riscv64gc",
#             ...
#         }),
#         ...
#     )
#
_buildscript_platform_constraints = rule(
    impl = _buildscript_platform_constraints_impl,
    attrs = {
        "constraint_setting": attrs.default_only(attrs.dep(default = "prelude//rust/tools:buildscript_for_platform")),
        "reindeer_platforms": attrs.set(attrs.string()),
    },
)

# [WORKAROUND] Wrap the above rule in a configured alias that replaces the
# current platform with an empty one. Otherwise the rule is unable to
# instantiate anonymous targets when configured for the <unbound> platform, such
# as when used in a `select` or `incoming_transition`:
#
#    > Can't find toolchain_dep execution platform using configuration `<unspecified_exec>`
#
# Hopefully this can be fixed in Buck by allowing configuration rules to
# instantiate anonymous targets.
def buildscript_platform_constraints(
        name: str,
        reindeer_platforms: set[str]) -> None:
    _buildscript_platform_constraints(
        name = "_{}".format(name),
        reindeer_platforms = list(reindeer_platforms),
        visibility = [],
    )

    native.configured_alias(
        name = name,
        actual = ":_{}".format(name),
        platform = "prelude//platforms:void",
        visibility = [],
    )
