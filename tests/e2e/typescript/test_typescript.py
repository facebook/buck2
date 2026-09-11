# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

import json
import re

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.asserts import expect_failure
from buck2.tests.e2e_util.buck_workspace import buck_test


FIXTURE = "fbcode//buck2/tests/targets/typescript/first_class"
PREBUILT_FIXTURE = "fbcode//buck2/tests/targets/typescript/prebuilt"
FBCODE_FACADE_FIXTURE = "fbcode//buck2/tests/targets/typescript/fbcode_facade"


@buck_test(inplace=True)
async def test_typecheck_aggregates_dependency_markers(buck: Buck) -> None:
    typecheck_target = f"{FIXTURE}:typecheck"
    typecheck = await buck.build(typecheck_target)
    validation_markers = typecheck.get_build_report().outputs_for_target(
        typecheck_target
    )
    assert sorted(marker.parent.name for marker in validation_markers) == sorted(
        {
            "__ambient-consumer__",
            "__ambient-provider__",
            "__app__",
            "__common__",
            "__diamond-left__",
            "__diamond-right__",
            "__diamond-root__",
        }
    )
    assert all(marker.name == "success.json" for marker in validation_markers)


@buck_test(inplace=True)
async def test_typecheck_rejects_dependencies_without_validation_markers(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.build(f"{FIXTURE}:empty-typecheck"),
        stderr_regex="dependencies provide no TypeScript validation markers",
    )


@buck_test(inplace=True)
async def test_library_publishes_declarations(buck: Buck) -> None:
    app_target = f"{FIXTURE}:app"
    declarations = await buck.build(f"{app_target}[declarations]")
    declaration_dir = declarations.get_build_report().output_for_target(
        app_target, "declarations"
    )
    declaration = declaration_dir / "src" / "app" / "index.d.ts"
    assert declaration.is_file()
    assert "render(value: string): string" in declaration.read_text()
    assert json.loads((declaration_dir / "package.json").read_text()) == {
        "name": "@e2e-example/ts-app",
        "types": "src/app/index.d.ts",
        "version": "0.0.0",
    }


@buck_test(inplace=True)
async def test_library_publishes_runtime(buck: Buck) -> None:
    app_target = f"{FIXTURE}:app"
    runtime = await buck.build(f"{app_target}[runtime]")
    runtime_dir = runtime.get_build_report().output_for_target(app_target, "runtime")
    assert (runtime_dir / "src" / "app" / "index.js").is_file()


@buck_test(inplace=True)
async def test_source_info_reports_runtime_and_declaration_only_sources(
    buck: Buck,
) -> None:
    source_info_target = f"{FIXTURE}:source-info-consumer"
    declaration_only_target = f"{FIXTURE}:declaration-only-source-info-consumer"
    source_info = await buck.build(
        source_info_target,
        declaration_only_target,
    )
    source_artifacts = source_info.get_build_report().outputs_for_target(
        source_info_target
    )
    assert len(source_artifacts) == 1
    assert source_artifacts[0].name == "index.ts"
    declaration_only_artifacts = source_info.get_build_report().outputs_for_target(
        declaration_only_target
    )
    assert len(declaration_only_artifacts) == 1
    assert declaration_only_artifacts[0].name == "index.d.ts"


@buck_test(inplace=True)
async def test_fbcode_typescript_facade_binary(buck: Buck) -> None:
    binary = await buck.run(f"{FBCODE_FACADE_FIXTURE}:binary")
    assert binary.stdout.strip() == "facade:binary"


@buck_test(inplace=True)
async def test_fbcode_typescript_facade_custom_run(buck: Buck) -> None:
    custom = await buck.run(f"{FBCODE_FACADE_FIXTURE}:custom-run")
    assert custom.stdout.strip().splitlines() == [
        "console.log('facade:bundle');",
        "facade:custom-runtime",
    ]


@buck_test(inplace=True)
async def test_fbcode_typescript_facade_canonical_modules_run(buck: Buck) -> None:
    canonical_modules = await buck.run(f"{FBCODE_FACADE_FIXTURE}:canonical-modules-run")
    assert canonical_modules.stdout.strip() == "facade:canonical-module-bundle"


@buck_test(inplace=True)
async def test_fbcode_typescript_facade_custom_compiled_run(buck: Buck) -> None:
    compiled = await buck.run(f"{FBCODE_FACADE_FIXTURE}:custom-compiled-run")
    assert compiled.stdout.strip().splitlines() == [
        "console.log('facade:compiled-modules');",
        "console.log('facade:module-bundle');",
        "facade:bun-runtime",
    ]


@buck_test(inplace=True)
async def test_fbcode_typescript_facade_prebuilt_binary(buck: Buck) -> None:
    prebuilt = await buck.run(f"{FBCODE_FACADE_FIXTURE}:prebuilt-binary")
    assert prebuilt.stdout.strip() == "prebuilt:binary"


@buck_test(inplace=True)
async def test_fbcode_typescript_facade_typecheck(buck: Buck) -> None:
    typecheck_target = f"{FBCODE_FACADE_FIXTURE}:typecheck"
    typecheck = await buck.build(typecheck_target)
    markers = typecheck.get_build_report().outputs_for_target(typecheck_target)
    assert len(markers) == 1
    assert markers[0].name == "success.json"


@buck_test(inplace=True)
async def test_transitive_diamond_is_deduplicated(buck: Buck) -> None:
    target = f"{FIXTURE}:diamond-root"
    result = await buck.build(
        f"{target}[declarations]",
        f"{target}[resolver-manifest]",
    )
    declaration_dir = result.get_build_report().output_for_target(
        target, "declarations"
    )
    declaration = declaration_dir / "src" / "diamond_root" / "index.d.ts"
    assert declaration.is_file()
    assert "renderDiamond(value: string): string" in declaration.read_text()
    resolver_manifest = result.get_build_report().output_for_target(
        target, "resolver-manifest"
    )
    common_packages = [
        package
        for package in json.loads(resolver_manifest.read_text())["packages"]
        if package["package_name"] == "@e2e-example/ts-common"
    ]
    assert len(common_packages) == 1
    assert common_packages[0]["owner"] == f"{FIXTURE}:common"


@buck_test(inplace=True)
async def test_typed_ambient_dependency(buck: Buck) -> None:
    target = f"{FIXTURE}:ambient-consumer"
    declarations = await buck.build(f"{target}[declarations]")
    declaration_dir = declarations.get_build_report().output_for_target(
        target, "declarations"
    )
    declaration = declaration_dir / "src" / "ambient_consumer" / "index.d.ts"
    assert "export declare const ambientGreeting: string;" in declaration.read_text()


@buck_test(inplace=True)
async def test_package_info_runtime_contracts(buck: Buck) -> None:
    expected_contracts = {
        "runtime-package-info-consumer": {
            "direct_runtime_present": True,
            "has_runtime": True,
            "runtime_entry_point": "src/app/index.js",
            "runtime_module_format": "commonjs",
            "runtime_platform": "node",
        },
        "declaration-only-package-info-consumer": {
            "direct_runtime_present": False,
            "has_runtime": False,
            "runtime_entry_point": "",
            "runtime_module_format": "",
            "runtime_platform": "",
        },
        "transitive-runtime-package-info-consumer": {
            "direct_runtime_present": False,
            "has_runtime": False,
            "runtime_entry_point": "",
            "runtime_module_format": "commonjs",
            "runtime_platform": "node",
        },
    }
    targets = [f"{FIXTURE}:{name}" for name in expected_contracts]
    result = await buck.build(*targets)
    build_report = result.get_build_report()

    for target, expected_contract in zip(targets, expected_contracts.values()):
        output = build_report.output_for_target(target)
        assert json.loads(output.read_text()) == expected_contract


@buck_test(inplace=True)
async def test_declaration_only_library_has_no_runtime_subtarget(buck: Buck) -> None:
    await expect_failure(
        buck.build(f"{FIXTURE}:ambient-provider[runtime]"),
        stderr_regex="requested sub target named `runtime`.*is not available",
    )


@buck_test(inplace=True)
async def test_declaration_only_library_rejects_runtime_emission(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.build(f"{FIXTURE}:declaration-only-with-runtime"),
        stderr_regex="emit_runtime=True.*set emit_runtime=False",
    )


@buck_test(inplace=True)
async def test_invalid_package_name_is_rejected_during_analysis(buck: Buck) -> None:
    await expect_failure(
        buck.build(f"{FIXTURE}:invalid-package-name"),
        stderr_regex=re.escape("invalid package_name '../invalid'"),
    )
    for target, package_name in [
        ("invalid-three-component-package-name", "invalid/package/name"),
        ("invalid-dot-scope-package-name", "@./pkg"),
        ("invalid-dotdot-scope-package-name", "@../pkg"),
        ("invalid-character-package-name", "invalid package:name*"),
        ("invalid-leading-dot-package-name", ".invalid"),
        ("invalid-leading-underscore-package-name", "_invalid"),
        ("invalid-leading-dot-scope-package-name", "@.scope/pkg"),
        ("invalid-leading-underscore-scoped-package-name", "@scope/_pkg"),
        ("invalid-tilde-package-name", "invalid~name"),
    ]:
        await expect_failure(
            buck.build(f"{FIXTURE}:{target}"),
            stderr_regex=re.escape(f"invalid package_name '{package_name}'"),
        )


@buck_test(inplace=True)
async def test_declaration_entry_point_is_rejected_for_runtime_package(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.build(f"{FIXTURE}:declaration-entry-with-runtime-source"),
        stderr_regex="entry_point.*produces no runtime output.*runtime-producing entry point.*emit_runtime=False",
    )


@buck_test(inplace=True)
async def test_runtime_output_collision_is_rejected_during_analysis(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.build(f"{FIXTURE}:runtime-output-collision"),
        stderr_regex=re.escape(
            "produce the same runtime output 'src/collision/index.js'"
        ),
    )


@buck_test(inplace=True)
async def test_declaration_output_collisions_are_rejected_during_analysis(
    buck: Buck,
) -> None:
    for target, output in [
        ("declaration-output-collision-no-runtime", "src/collision/index.d.ts"),
        (
            "declaration-source-output-collision",
            "src/declaration_collision/foo.d.ts",
        ),
    ]:
        await expect_failure(
            buck.build(f"{FIXTURE}:{target}"),
            stderr_regex=re.escape(f"produce the same declaration output '{output}'"),
        )
    await expect_failure(
        buck.build(f"{FIXTURE}:declaration-prefix-output-collision"),
        stderr_regex=re.escape(
            "conflicting declaration outputs "
            "'src/declaration_prefix_collision/a.d.ts' and "
            "'src/declaration_prefix_collision/a.d.ts/child.d.ts'"
        ),
    )


@buck_test(inplace=True)
async def test_duplicate_source_is_rejected_during_analysis(buck: Buck) -> None:
    await expect_failure(
        buck.build(f"{FIXTURE}:duplicate-source"),
        stderr_regex=re.escape(
            "lists TypeScript source path 'src/common/index.ts' more than once"
        ),
    )


@buck_test(inplace=True)
async def test_distinct_artifacts_with_same_source_path_are_rejected(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.build(f"{FIXTURE}:distinct-artifact-source-collision"),
        stderr_regex=re.escape("both map to TypeScript source path 'shared/index.ts'"),
    )


@buck_test(inplace=True)
async def test_dependency_identity_mismatch_is_rejected_during_analysis(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.build(f"{FIXTURE}:identity-mismatch"),
        stderr_regex=re.escape(
            "uses compiler identity 'typescript@mismatch', expected '"
        )
        + "[^']+'",
    )
    await expect_failure(
        buck.build(f"{FIXTURE}:toolchain-identity-mismatch"),
        stderr_regex=re.escape(
            "uses toolchain identity 'toolchain@mismatch', expected '"
        )
        + "[^']+'",
    )


@buck_test(inplace=True)
async def test_composable_compile_bundle_and_runtime_stages(buck: Buck) -> None:
    compiled = await buck.run(f"{FIXTURE}:compiled-program-run")
    assert compiled.stdout.strip() == "app:compiled"

    node = await buck.run(f"{FIXTURE}:source-bundle-node")
    assert node.stdout.strip() == "source bundle"

    bun_like = await buck.run(f"{FIXTURE}:source-bundle-bun")
    assert bun_like.stdout.strip().splitlines() == [
        "console.log('source bundle');",
        "bun-like:source bundle",
    ]

    custom_compiler = await buck.run(f"{FIXTURE}:custom-compiler-run")
    assert custom_compiler.stdout.strip() == "custom compiler bundle"


@buck_test(inplace=True)
async def test_custom_stages_keep_typechecking_and_validate_compatibility(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.build(f"{FIXTURE}:invalid-custom-bundle"),
        stderr_regex="Type 'number' is not assignable to type 'string'",
    )
    runtime_failure = await expect_failure(
        buck.build(f"{FIXTURE}:incompatible-source-bundle-runtime"),
        stderr_regex=(
            "adapter 'fake-browser-runtime-v1' for target "
            f"'{FIXTURE}:source-bundle.*does not accept platform 'node'"
        ),
    )
    assert (
        f"for target '{FIXTURE}:incompatible-source-bundle-runtime"
        not in runtime_failure.stderr
    )
    await expect_failure(
        buck.build(f"{FIXTURE}:incompatible-compiler-bundle"),
        stderr_regex="does not accept module format 'esm'",
    )


@buck_test(inplace=True)
async def test_binary_compiles_bundles_runs_and_rejects_compiler_bypass(
    buck: Buck,
) -> None:
    native = await buck.run(f"{FIXTURE}:native-binary")
    assert native.stdout.strip() == "app:native"

    default_entry_point = await buck.run(f"{FIXTURE}:default-entry-point-binary")
    assert default_entry_point.stdout.strip() == "default:entry-point"
    source_info_target = f"{FIXTURE}:default-entry-point-source-info-consumer"
    source_info = await buck.build(source_info_target)
    source_artifacts = source_info.get_build_report().outputs_for_target(
        source_info_target
    )
    assert [artifact.name for artifact in source_artifacts] == ["index.ts"]

    esm = await buck.run(f"{FIXTURE}:esm-source-binary")
    assert esm.stdout.strip() == "source bundle esm"

    await expect_failure(
        buck.build(f"{FIXTURE}:compiler-bypass"),
        stderr_regex="selects compiler.*but source bundler.*would bypass it",
    )


@buck_test(inplace=True)
async def test_custom_compiler_rejects_incompatible_runtime_dependency(
    buck: Buck,
) -> None:
    await expect_failure(
        buck.build(f"{FIXTURE}:incompatible-custom-compiler-dependency"),
        stderr_regex=(
            "dependency whose runtime modules are incompatible with compiled output "
            "module format 'esm' and platform 'neutral'"
        ),
    )
    await expect_failure(
        buck.build(f"{FIXTURE}:malformed-runtime-capability-dependency"),
        stderr_regex="has_runtime=True but has no runtime module format and platform capability",
    )
    await expect_failure(
        buck.build(f"{FIXTURE}:partial-runtime-capability-dependency"),
        stderr_regex="must define runtime module format and platform together",
    )
    await expect_failure(
        buck.build(f"{FIXTURE}:incompatible-transitive-runtime-dependency"),
        stderr_regex=(
            "dependency whose runtime modules are incompatible with compiled output "
            "module format 'esm' and platform 'neutral'"
        ),
    )


@buck_test(inplace=True)
async def test_prebuilt_packages_typecheck_and_run(buck: Buck) -> None:
    declaration_target = f"{PREBUILT_FIXTURE}:declaration-consumer-typecheck"
    declaration_result = await buck.build(declaration_target)
    declaration_markers = declaration_result.get_build_report().outputs_for_target(
        declaration_target
    )
    assert len(declaration_markers) == 1
    assert declaration_markers[0].name == "success.json"

    declaration_binary = await buck.run(f"{PREBUILT_FIXTURE}:declaration-only-binary")
    assert declaration_binary.stdout.strip() == "declaration-only"

    compatible_target = f"{PREBUILT_FIXTURE}:compatible-cjs"
    compatible = await buck.run(f"{PREBUILT_FIXTURE}:compatible-binary")
    assert compatible.stdout.strip() == "prebuilt:compatible:subpath"
    declarations = await buck.build(f"{compatible_target}[declarations]")
    declarations_dir = declarations.get_build_report().output_for_target(
        compatible_target, "declarations"
    )
    assert json.loads((declarations_dir / "package.json").read_text()) == {
        "name": "@prebuilt/cjs",
        "types": "lib/index.d.ts",
        "version": "0.0.0",
    }

    nested_target = f"{PREBUILT_FIXTURE}:nested-consumer-typecheck"
    nested = await buck.build(nested_target)
    assert len(nested.get_build_report().outputs_for_target(nested_target)) == 1


@buck_test(inplace=True)
async def test_prebuilt_incompatible_runtime_typechecks_but_cannot_run(
    buck: Buck,
) -> None:
    await buck.build(f"{PREBUILT_FIXTURE}:incompatible-consumer-typecheck")
    await expect_failure(
        buck.build(f"{PREBUILT_FIXTURE}:incompatible-consumer-binary"),
        stderr_regex=(
            "dependency whose runtime modules are incompatible with compiled output "
            "module format 'commonjs' and platform 'node'"
        ),
    )


@buck_test(inplace=True)
async def test_prebuilt_rejects_malformed_layouts(buck: Buck) -> None:
    for target, error in [
        (
            "missing-declaration-entry",
            "declaration entry point 'missing.d.ts' does not exist",
        ),
        ("invalid-declaration-entry", "invalid declaration_entry_point"),
        ("runtime-without-entry", "must set runtime and runtime_entry_point together"),
        (
            "runtime-without-format",
            "must set module_format and platform exactly when runtime is set",
        ),
        (
            "format-without-runtime",
            "must set module_format and platform exactly when runtime is set",
        ),
        ("invalid-package-path", "must end with package_name"),
        (
            "invalid-scoped-node-modules-package-path",
            "invalid package_name '@scope/node_modules'",
        ),
        ("non-directory-declarations", "must be a directory"),
        ("invalid-prebuilt-typecheck", "TypeScriptTypecheckInfo"),
    ]:
        await expect_failure(
            buck.build(f"{PREBUILT_FIXTURE}:{target}"),
            stderr_regex=error,
        )
