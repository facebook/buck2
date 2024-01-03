# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:artifacts.bzl", "ArtifactGroupInfo")
load("@prelude//go:toolchain.bzl", "GoToolchainInfo")
load("@prelude//utils:utils.bzl", "value_or")
load(":coverage.bzl", "GoCoverageMode")

GoPkg = record(
    # Built w/ `-shared`.
    shared = field(Artifact),
    # Built w/o `-shared`.
    static = field(Artifact),
    cgo = field(bool, default = False),
    coverage_static = field(dict[GoCoverageMode, (Artifact, cmd_args)]),
    coverage_shared = field(dict[GoCoverageMode, (Artifact, cmd_args)]),
)

def go_attr_pkg_name(ctx: AnalysisContext) -> str:
    """
    Return the Go package name for the given context corresponding to a rule.
    """
    return value_or(ctx.attrs.package_name, ctx.label.package)

def merge_pkgs(pkgss: list[dict[str, typing.Any]]) -> dict[str, typing.Any]:
    """
    Merge mappings of packages into a single mapping, throwing an error on
    conflicts.
    """

    all_pkgs = {}

    for pkgs in pkgss:
        for name, path in pkgs.items():
            if name in pkgs and pkgs[name] != path:
                fail("conflict for package {!r}: {} and {}".format(name, path, all_pkgs[name]))
            all_pkgs[name] = path

    return all_pkgs

def pkg_artifact(pkg: GoPkg, shared: bool, coverage_mode: [GoCoverageMode, None]) -> Artifact:
    if coverage_mode:
        artifact = pkg.coverage_shared if shared else pkg.coverage_static
        return artifact[coverage_mode][0]
    return pkg.shared if shared else pkg.static

def pkg_coverage_vars(name: str, pkg: GoPkg, shared: bool, coverage_mode: [GoCoverageMode, None]) -> [cmd_args, None]:
    if coverage_mode:
        artifact = pkg.coverage_shared if shared else pkg.coverage_static
        if coverage_mode not in artifact:
            fail("coverage variables don't exist for {}".format(name))
        return artifact[coverage_mode][1]
    fail("coverage variables were requested but coverage_mode is None")

def pkg_artifacts(pkgs: dict[str, GoPkg], shared: bool, coverage_mode: [GoCoverageMode, None] = None) -> dict[str, Artifact]:
    """
    Return a map package name to a `shared` or `static` package artifact.
    """
    return {
        name: pkg_artifact(pkg, shared, coverage_mode)
        for name, pkg in pkgs.items()
    }

def stdlib_pkg_artifacts(toolchain: GoToolchainInfo, shared: bool = False) -> dict[str, Artifact]:
    """
    Return a map package name to a `shared` or `static` package artifact of stdlib.
    """

    prebuilt_stdlib = toolchain.prebuilt_stdlib_shared if shared else toolchain.prebuilt_stdlib
    stdlib_pkgs = prebuilt_stdlib[ArtifactGroupInfo].artifacts

    if len(stdlib_pkgs) == 0:
        fail("Stdlib for current platfrom is missing from toolchain.")

    pkgs = {}
    for pkg in stdlib_pkgs:
        # remove first directory like `pgk`
        _, _, temp_path = pkg.short_path.partition("/")

        # remove second directory like `darwin_amd64`
        # now we have name like `net/http.a`
        _, _, pkg_relpath = temp_path.partition("/")
        name = pkg_relpath.removesuffix(".a")  # like `net/http`
        pkgs[name] = pkg

    return pkgs
