# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@fbcode_macros//build_defs/lib/typescript:providers.bzl",
    "TypeScriptDependency",
    "TypeScriptDependencyTSet",
    "TypeScriptPackageInfo",
    "TypeScriptRuntimeTSet",
    "TypeScriptSourceInfo",
    "TypeScriptTypecheckInfo",
    "TypeScriptValidationTSet",
)
load("@fbcode_macros//build_defs/lib/typescript:toolchain.bzl", "TypeScriptToolchainInfo")

def _identity_typescript_package(ctx: AnalysisContext, compiler_identity: str, toolchain_identity: str) -> list[Provider]:
    declaration = ctx.actions.write("mismatch.d.ts", "export declare const mismatch: string;\n")
    declarations = ctx.actions.symlinked_dir("declarations", {"mismatch.d.ts": declaration})
    dependency = TypeScriptDependency(
        ambient = False,
        package_name = "mismatch",
        logical_path = "node_modules/mismatch",
        canonical_owner = str(ctx.label.raw_target()),
        declaration_package = declarations,
        declaration_root = ".",
    )
    return [
        DefaultInfo(default_outputs = [declarations]),
        TypeScriptPackageInfo(
            compiler_identity = compiler_identity,
            declaration_entry_point = "mismatch.d.ts",
            direct_declarations = declarations,
            direct_runtime = None,
            has_runtime = False,
            logical_path = "node_modules/mismatch",
            package_name = "mismatch",
            runtime_entry_point = "",
            runtime_module_format = "",
            runtime_platform = "",
            toolchain_identity = toolchain_identity,
            transitive_declarations = ctx.actions.tset(TypeScriptDependencyTSet, value = dependency),
            transitive_runtime = ctx.actions.tset(TypeScriptRuntimeTSet),
        ),
    ]

def _mismatched_typescript_package_impl(ctx: AnalysisContext) -> list[Provider]:
    toolchain = ctx.attrs._typescript_toolchain[TypeScriptToolchainInfo]
    return _identity_typescript_package(ctx, "typescript@mismatch", toolchain.toolchain_identity)

mismatched_typescript_package = rule(
    impl = _mismatched_typescript_package_impl,
    attrs = {
        "_typescript_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:typescript", providers = [TypeScriptToolchainInfo])),
    },
)

def _mismatched_toolchain_typescript_package_impl(ctx: AnalysisContext) -> list[Provider]:
    toolchain = ctx.attrs._typescript_toolchain[TypeScriptToolchainInfo]
    return _identity_typescript_package(ctx, toolchain.compiler_identity, "toolchain@mismatch")

mismatched_toolchain_typescript_package = rule(
    impl = _mismatched_toolchain_typescript_package_impl,
    attrs = {
        "_typescript_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:typescript", providers = [TypeScriptToolchainInfo])),
    },
)

def _empty_typecheck_provider_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        TypeScriptTypecheckInfo(
            validation_markers = ctx.actions.tset(TypeScriptValidationTSet),
        ),
    ]

empty_typecheck_provider = rule(
    impl = _empty_typecheck_provider_impl,
    attrs = {},
)

def _typescript_source_fixture_impl(ctx: AnalysisContext) -> list[Provider]:
    source = ctx.actions.write("shared/index.ts", "export const value = true;\n")
    return [DefaultInfo(default_outputs = [source])]

typescript_source_fixture = rule(
    impl = _typescript_source_fixture_impl,
    attrs = {},
)

def _source_info_consumer_impl(ctx: AnalysisContext) -> list[Provider]:
    source_info = ctx.attrs.dep[TypeScriptSourceInfo]
    has_runtime = source_info.canonical_runtime != None
    if has_runtime != bool(source_info.canonical_runtime_entry_point):
        fail("TypeScriptSourceInfo canonical_runtime and canonical_runtime_entry_point must be present together")
    if has_runtime != ctx.attrs.expect_runtime:
        fail("expected canonical runtime presence={}, got {}".format(ctx.attrs.expect_runtime, has_runtime))
    return [DefaultInfo(default_outputs = source_info.source_artifacts)]

source_info_consumer = rule(
    impl = _source_info_consumer_impl,
    attrs = {
        "dep": attrs.dep(providers = [TypeScriptSourceInfo]),
        "expect_runtime": attrs.bool(),
    },
)

def _package_info_consumer_impl(ctx: AnalysisContext) -> list[Provider]:
    package = ctx.attrs.dep[TypeScriptPackageInfo]
    observed = ctx.actions.write_json(
        "package-info.json",
        {
            "direct_runtime_present": package.direct_runtime != None,
            "has_runtime": package.has_runtime,
            "runtime_entry_point": package.runtime_entry_point,
            "runtime_module_format": package.runtime_module_format,
            "runtime_platform": package.runtime_platform,
        },
    )
    return [DefaultInfo(default_outputs = [observed])]

package_info_consumer = rule(
    impl = _package_info_consumer_impl,
    attrs = {
        "dep": attrs.dep(providers = [TypeScriptPackageInfo]),
    },
)
