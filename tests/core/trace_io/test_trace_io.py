# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict


from __future__ import annotations

import json
import os
import re
import subprocess
from pathlib import Path
from tempfile import NamedTemporaryFile, TemporaryDirectory

from buck2.tests.e2e_util.api.buck import Buck
from buck2.tests.e2e_util.buck_workspace import buck_test, env


def assert_path_in_manifest(path: str, manifest_paths: list[str]) -> None:
    assert path in manifest_paths, f"expected manifest to contain {path}"


def assert_link_in(
    needle: dict[str, str | None], haystack: list[dict[str, str | None]]
) -> None:
    assert (
        needle in haystack
    ), f'expected haystack to contain link: {needle["link"]} --> {needle["target"]}'


def assert_path_exists(path: str) -> None:
    assert os.path.exists(path), f"expected {path} to exist"


def assert_buck_out_paths_materialized(buck_cwd: str, paths: list[str]) -> None:
    for path in paths:
        if re.match(r"buck-out\/.+\/{gen,offline-cache}/.+\/.+\/.+", path) is not None:
            assert_path_exists(os.path.join(buck_cwd, path))


def hg_init(cwd: Path) -> None:
    subprocess.run(["hg", "init"], check=True, cwd=cwd)
    hg_config_reponame(cwd)


def hg_config_reponame(cwd: Path) -> None:
    subprocess.run(
        ["hg", "config", "remotefilelog.reponame", "--local", "no-repo"],
        check=True,
        cwd=cwd,
    )


def _setup_buckconfig_digest_algorithms(buck: Buck) -> None:
    # The digests in `//cas_artifact:` require the buckconfig.
    with open(buck.cwd / ".buckconfig", "a") as buckconfig:
        buckconfig.write("[buck2]\n")
        buckconfig.write("digest_algorithms = BLAKE3-KEYED,SHA1\n")


# Tracing I/O not implemented for Windows.
@buck_test(skip_for_os=["windows"])
async def test_simple_binary_build(buck: Buck) -> None:
    # Since this is an inplace test, we need to fake an hg repo so that export-manifest
    # can extract the repo revision.
    hg_init(cwd=buck.cwd)

    await buck.debug("trace-io", "enable")
    await buck.build("root//hello_world:welcome")
    out = await buck.debug("trace-io", "export-manifest")
    manifest = json.loads(out.stdout)

    assert (
        manifest["repository"]["revision"] == "0000000000000000000000000000000000000000"
    ), "expected manifest to be at null revision"
    assert (
        manifest["repository"]["name"] == "no-repo"
    ), "expected repo name to be no-repo"

    assert_path_in_manifest("hello_world/main.cpp", manifest["paths"])


@buck_test(skip_for_os=["windows"])
async def test_external_buckconfig_path_included_in_manifest(buck: Buck) -> None:
    hg_init(cwd=buck.cwd)

    with NamedTemporaryFile("w") as tmp:
        tmpname = tmp.name
        tmp.writelines(
            [
                "[buck2]",
                "  foo = bar",
            ]
        )

        await buck.debug("trace-io", "enable")
        await buck.build("root//hello_world:welcome", "--config-file", tmpname)
        out = await buck.debug("trace-io", "export-manifest")

    manifest = json.loads(out.stdout)

    assert_path_in_manifest(str(Path(tmpname).resolve()), manifest["external_paths"])


# More complicated example with binary depending on multiple libraries.
@buck_test(skip_for_os=["windows"])
async def test_binary_with_deps(buck: Buck) -> None:
    hg_init(cwd=buck.cwd)

    await buck.debug("trace-io", "enable")
    await buck.build("root//linking:root")
    out = await buck.debug("trace-io", "export-manifest")
    manifest = json.loads(out.stdout)

    assert (
        manifest["repository"]["revision"] == "0000000000000000000000000000000000000000"
    ), "expected manifest to be at null revision"
    assert (
        manifest["repository"]["name"] == "no-repo"
    ), "expected repo name to be no-repo"

    assert_path_in_manifest("linking/main.cpp", manifest["paths"])
    assert_path_in_manifest("linking/static.cpp", manifest["paths"])
    assert_path_in_manifest("linking/static.h", manifest["paths"])
    assert_path_in_manifest("linking/shared.h", manifest["paths"])


# Multiple builds should be logical union of all input files of all builds.
@buck_test(skip_for_os=["windows"])
async def test_multiple_builds(buck: Buck) -> None:
    hg_init(cwd=buck.cwd)

    await buck.debug("trace-io", "enable")
    await buck.build("root//linking:root")
    await buck.build("root//hello_world:welcome")
    out = await buck.debug("trace-io", "export-manifest")
    manifest = json.loads(out.stdout)

    # From first build
    assert_path_in_manifest("linking/shared.h", manifest["paths"])
    # From second build
    assert_path_in_manifest("hello_world/main.cpp", manifest["paths"])


# Symlinks should show up in the *_symlinks attributes of the manifest.
@buck_test(setup_eden=True, skip_for_os=["windows"])
async def test_symlinks(buck: Buck) -> None:
    hg_config_reponame(cwd=buck.cwd)

    def symlink(link: str, target: str) -> None:
        """
        Symlinks link --> target. Assumes we're based in the buck cwd, so link must be relative.
        """
        os.symlink(target, os.path.join(buck.cwd, link))

    # Set up symlinks during the test; buck will read everything behind symlinks while
    # setting up for the test otherwise.
    # Symlinks for root//symlinks:relative_link
    symlink("symlinks/main.cpp", "../hello_world/main.cpp")

    # Symlinks for root//symlinks:external_link
    with TemporaryDirectory() as tempdir:
        t = Path(tempdir)
        absolute_target = t / "include" / "clang" / "Basic" / "Visibility.h"
        absolute_target.parent.mkdir(parents=True)
        absolute_target.touch()

        traverses_symlink = t / "include" / "llvm" / "PassRegistry.h"
        traverses_symlink.parent.mkdir(parents=True)
        traverses_symlink.touch()

        symlink("symlinks/PassRegistry.h", str(absolute_target))
        symlink("symlinks/include", str(t / "include"))

        await buck.debug("trace-io", "enable")
        await buck.build("root//symlinks:relative_link")
        await buck.build("root//symlinks:external_link")

    out = await buck.debug("trace-io", "export-manifest")
    manifest = json.loads(out.stdout)

    assert_link_in(
        {"link": "symlinks/main.cpp", "target": "hello_world/main.cpp"},
        manifest["relative_symlinks"],
    )
    assert_link_in(
        {
            "link": "symlinks/PassRegistry.h",
            "target": str(absolute_target),
            "remaining_path": None,
        },
        manifest["external_symlinks"],
    )
    assert_link_in(
        {
            "link": "symlinks/include",
            "target": str(t / "include"),
            "remaining_path": "clang/Basic/Visibility.h",
        },
        manifest["external_symlinks"],
    )
    assert_path_in_manifest("symlinks/other.cpp", manifest["paths"])


# Validate that manifest includes downloaded http_archive path in buck-out.
@buck_test(skip_for_os=["windows"])
async def test_includes_http_archive_in_manifest(buck: Buck) -> None:
    hg_init(cwd=buck.cwd)

    await buck.debug("trace-io", "enable")
    await buck.build("root//http_archive:test_zip")
    out = await buck.debug("trace-io", "export-manifest")
    manifest = json.loads(out.stdout)

    assert any(
        re.match(
            r"buck-out/.+/offline-cache/.+/http_archive/__test_zip__/download", path
        )
        for path in manifest["paths"]
    ), "manifest should contain http_archive cached output"
    assert_buck_out_paths_materialized(buck.cwd, manifest["paths"])


# Ensure offline-cache buck-out dir is _not_ created when not doing I/O tracing.
@buck_test(skip_for_os=["windows"])
async def test_no_tracing_does_not_write_offline_cache_for_http_archive(
    buck: Buck,
) -> None:
    await buck.build("root//http_archive:test_zip")
    assert not os.path.exists(
        os.path.join(buck.cwd, "buck-out/offline-cache")
    ), "offline cache should not exist when not doing I/O tracing"


# Validate that when buckconfig use_network_action_output_cache=true is set we use the
# offline-cache action output instead of fetching from the network.
@buck_test(
    skip_for_os=["windows"],
    extra_buck_config={"buck2": {"sqlite_materializer_state": "false"}},
)
@env("BUCK_LOG", "buck2_execute_impl::materializers=trace")
async def test_fake_offline_http_archive_uses_offline_cache(buck: Buck) -> None:
    hg_init(cwd=buck.cwd)

    # This should materialize the offline-cache dir.
    target = "root//http_archive:test_zip"
    await buck.debug("trace-io", "enable")
    result = await buck.build(target)
    print("stderr:", result.stderr)
    assert (
        "/offline-cache/" in result.stderr
    ), "materializer should declare offline-cache materialization"

    # Validate that offline-cache path doesn't exist prior to manifest export.
    http_download_path = result.get_build_report().output_for_target(target)
    # This is hacky, but there's no other good way to discover the offline-cache path.
    offline_cache_path = (
        Path(str(http_download_path).replace("/gen/", "/offline-cache/")).parent
        / "download"
    )
    assert (
        not offline_cache_path.exists()
    ), "offline cache path should not exist before manifest export"

    # Ensure buck-out/offline-cache paths are materialized.
    await buck.debug("trace-io", "export-manifest")
    assert (
        offline_cache_path.exists()
    ), "offline cache path should exist after manifest export"

    await buck.kill()

    result = await buck.build(
        "root//http_archive:test_zip",
        "--config",
        "buck2.use_network_action_output_cache=true",
        "--no-remote-cache",
        "--local-only",
    )
    assert "LocalCopy" in result.stderr, "offline-cache path should be copied to output"
    assert http_download_path.exists(), "http download output path should exist"


@buck_test(skip_for_os=["windows"])
async def test_includes_cas_artifact_in_manifest(buck: Buck) -> None:
    hg_init(cwd=buck.cwd)

    _setup_buckconfig_digest_algorithms(buck)

    await buck.debug("trace-io", "enable")
    await buck.build("//cas_artifact:tree")
    out = await buck.debug("trace-io", "export-manifest")
    manifest = json.loads(out.stdout)

    assert any(
        re.match(
            r"buck-out\/.+\/offline-cache/root\/.+\/cas_artifact/__tree__/tree", path
        )
        is not None
        for path in manifest["paths"]
    ), "offline cache should contain cas artifact tree"

    assert_buck_out_paths_materialized(buck.cwd, manifest["paths"])


# Ensure offline-cache buck-out dir is _not_ created when not doing I/O tracing.
@buck_test(skip_for_os=["windows"])
async def test_no_tracing_does_not_write_offline_cache_for_cas_artifact(
    buck: Buck,
) -> None:
    _setup_buckconfig_digest_algorithms(buck)

    await buck.build("//cas_artifact:tree")
    assert not os.path.exists(
        os.path.join(buck.cwd, "buck-out/offline-cache")
    ), "offline cache should not exist when not doing I/O tracing"


# Validate that when buckconfig use_network_action_output_cache=true is set we use the
# offline-cache action output instead of fetching from the network.
@buck_test(
    skip_for_os=["windows"],
    extra_buck_config={"buck2": {"sqlite_materializer_state": "false"}},
)
@env("BUCK_LOG", "buck2_execute_impl::materializers=trace")
async def test_fake_offline_cas_artifact_uses_offline_cache(buck: Buck) -> None:
    hg_init(cwd=buck.cwd)

    _setup_buckconfig_digest_algorithms(buck)

    # This should materialize the offline-cache dir.
    target = "root//cas_artifact:tree"
    await buck.debug("trace-io", "enable")
    result = await buck.build(target)
    print("stderr:", result.stderr)
    assert (
        "/offline-cache/" in result.stderr
    ), "materializer should declare offline-cache materialization"

    # Validate that offline-cache path doesn't exist prior to manifest export.
    cas_download_path = result.get_build_report().output_for_target(target)
    # This is hacky, but there's no other good way to discover the offline-cache path.
    offline_cache_path = (
        Path(str(cas_download_path).replace("/gen/", "/offline-cache/")).parent / "tree"
    )
    assert (
        not offline_cache_path.exists()
    ), "offline cache path should not exist before manifest export"

    # Ensure buck-out/offline-cache paths are materialized.
    await buck.debug("trace-io", "export-manifest")
    assert (
        offline_cache_path.exists()
    ), "offline cache path should exist after manifest export"

    await buck.kill()

    result = await buck.build(
        target,
        "--config",
        "buck2.use_network_action_output_cache=true",
        "--no-remote-cache",
        "--local-only",
    )
    assert "LocalCopy" in result.stderr, "offline-cache path should be copied to output"
    assert cas_download_path.exists(), "cas action output path should exist"


# No-op test for windows.
@buck_test()
async def test_noop(buck: Buck) -> None:
    return
