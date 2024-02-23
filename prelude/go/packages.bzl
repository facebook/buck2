# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//go:toolchain.bzl", "GoToolchainInfo")
load("@prelude//utils:utils.bzl", "value_or")
load(":coverage.bzl", "GoCoverageMode")

GoPkg = record(
    cgo = field(bool, default = False),
    pkg = field(Artifact),
    pkg_with_coverage = field(dict[GoCoverageMode, (Artifact, cmd_args)]),
)

GoStdlib = provider(
    fields = {
        "importcfg": provider_field(Artifact),
        "pkgdir": provider_field(Artifact),
    },
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

def pkg_artifact(pkg: GoPkg, coverage_mode: [GoCoverageMode, None]) -> Artifact:
    if coverage_mode:
        artifact = pkg.pkg_with_coverage
        return artifact[coverage_mode][0]
    return pkg.pkg

def pkg_coverage_vars(name: str, pkg: GoPkg, coverage_mode: [GoCoverageMode, None]) -> [cmd_args, None]:
    if coverage_mode:
        artifact = pkg.pkg_with_coverage
        if coverage_mode not in artifact:
            fail("coverage variables don't exist for {}".format(name))
        return artifact[coverage_mode][1]
    fail("coverage variables were requested but coverage_mode is None")

def pkg_artifacts(pkgs: dict[str, GoPkg], coverage_mode: [GoCoverageMode, None] = None) -> dict[str, Artifact]:
    """
    Return a map package name to a `shared` or `static` package artifact.
    """
    return {
        name: pkg_artifact(pkg, coverage_mode)
        for name, pkg in pkgs.items()
    }

def make_importcfg(
        ctx: AnalysisContext,
        root: str,
        pkg_name: str,
        own_pkgs: dict[str, typing.Any],
        with_importmap: bool) -> cmd_args:
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    stdlib = ctx.attrs._go_stdlib[GoStdlib]

    content = []
    for name_, pkg_ in own_pkgs.items():
        # Hack: we use cmd_args get "artifact" valid path and write it to a file.
        content.append(cmd_args("packagefile ", name_, "=", pkg_, delimiter = ""))

        # Future work: support importmap in buck rules insted of hacking here.
        if with_importmap and name_.startswith("third-party-source/go/"):
            real_name_ = name_.removeprefix("third-party-source/go/")
            content.append(cmd_args("importmap ", real_name_, "=", name_, delimiter = ""))

    own_importcfg = ctx.actions.declare_output(root, "{}.importcfg".format(pkg_name))
    ctx.actions.write(own_importcfg, content)

    final_importcfg = ctx.actions.declare_output(root, "{}.final.importcfg".format(pkg_name))
    ctx.actions.run(
        [
            go_toolchain.concat_files[RunInfo],
            "--output",
            final_importcfg.as_output(),
            stdlib.importcfg,
            own_importcfg,
        ],
        category = "concat_importcfgs",
        identifier = "{}/{}".format(root, pkg_name),
    )

    return cmd_args(final_importcfg).hidden(stdlib.pkgdir).hidden(own_pkgs.values())
