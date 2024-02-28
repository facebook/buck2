# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//go:toolchain.bzl", "GoToolchainInfo")
load("@prelude//utils:utils.bzl", "value_or")

GoPkg = record(
    cgo = field(bool, default = False),
    pkg = field(Artifact),
    coverage_vars = field(cmd_args | None, default = None),
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

def pkg_artifacts(pkgs: dict[str, GoPkg]) -> dict[str, Artifact]:
    """
    Return a map package name to a `shared` or `static` package artifact.
    """
    return {
        name: pkg.pkg
        for name, pkg in pkgs.items()
    }

def make_importcfg(
        ctx: AnalysisContext,
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

    own_importcfg = ctx.actions.declare_output("{}.importcfg".format(pkg_name))
    ctx.actions.write(own_importcfg, content)

    final_importcfg = ctx.actions.declare_output("{}.final.importcfg".format(pkg_name))
    ctx.actions.run(
        [
            go_toolchain.concat_files,
            "--output",
            final_importcfg.as_output(),
            stdlib.importcfg,
            own_importcfg,
        ],
        category = "concat_importcfgs",
        identifier = pkg_name,
    )

    return cmd_args(final_importcfg).hidden(stdlib.pkgdir).hidden(own_pkgs.values())
