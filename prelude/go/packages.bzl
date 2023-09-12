# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//utils:utils.bzl", "value_or")
load(":toolchain.bzl", "GoToolchainInfo")

GoPkg = record(
    # Built w/ `-shared`.
    shared = field(Artifact),
    # Built w/o `-shared`.
    static = field(Artifact),
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

def pkg_artifacts(pkgs: dict[str, GoPkg], shared: bool = False) -> dict[str, Artifact]:
    """
    Return a map package name to a `shared` or `static` package artifact.
    """
    return {
        name: pkg.shared if shared else pkg.static
        for name, pkg in pkgs.items()
    }

def make_importcfg(
        ctx: AnalysisContext,
        all_pkgs: dict[str, typing.Any],
        toolchain: GoToolchainInfo,
        shared: bool,
        prefix: str,
        with_importmap: bool) -> cmd_args:
    if shared:
        stdlib = toolchain.stdlib_shared
        suffix = "shared"
    else:
        stdlib = toolchain.stdlib_static
        suffix = "static"

    content = []
    for name_, pkg_ in all_pkgs.items():
        # Hack: we use cmd_args get "artifact" valid path and write it to a file.
        content.append(cmd_args("packagefile ", name_, "=", pkg_, delimiter = ""))

        # Future work: support importmap in buck rules insted of hacking here.
        if with_importmap and name_.startswith("third-party-source/go/"):
            real_name_ = name_.removeprefix("third-party-source/go/")
            content.append(cmd_args("importmap ", real_name_, "=", name_, delimiter = ""))

    importcfg_pks = ctx.actions.write("importcfg/{}_{}.all_pkgs".format(prefix, suffix), content)

    importcfg = ctx.actions.declare_output("importcfg/{}_{}".format(prefix, suffix))
    ctx.actions.run(
        [
            toolchain.concat_files,
            "--output",
            importcfg.as_output(),
            stdlib.importcfg,
            importcfg_pks,
        ],
        category = "concat_importcfgs",
        identifier = "{}_{}".format(prefix, suffix),
    )

    return cmd_args(importcfg).hidden(stdlib.stdlib).hidden(all_pkgs.values())
