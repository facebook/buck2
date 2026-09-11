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


FIXTURE = "fbcode//buck2/tests/e2e/typescript/first_class"


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
