# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:headers.bzl", "prepare_headers")
load(
    "@prelude//cxx:preprocessor.bzl",
    "CPreprocessor",
    "CPreprocessorArgs",
)
load("@prelude//go:toolchain.bzl", "GoToolchainInfo")
load("@prelude//utils:utils.bzl", "value_or")

# Information about a package for GOPACKAGESDRIVER
GoPackageInfo = provider(
    fields = {
        "build_out": provider_field(Artifact),
        "cgo_gen_dir": provider_field(Artifact),
        "go_list_out": provider_field(Artifact),
        "package_name": provider_field(str),
        "package_root": provider_field(str),
    },
)

GoPkg = record(
    # We have to produce allways shared (PIC) and non-shared (non-PIC) archives
    pkg = field(Artifact),
    pkg_shared = field(Artifact),
    coverage_vars = field(cmd_args),
    srcs_list = field(cmd_args),
)

GoStdlib = provider(
    fields = {
        "importcfg": provider_field(Artifact),
        "importcfg_shared": provider_field(Artifact),
        "pkgdir": provider_field(Artifact),
        "pkgdir_shared": provider_field(Artifact),
    },
)

def go_attr_pkg_name(ctx: AnalysisContext) -> str:
    """
    Return the Go package name for the given context corresponding to a rule.
    """
    return value_or(ctx.attrs.package_name, ctx.label.package)

def merge_pkgs(pkgss: list[dict[str, GoPkg]]) -> dict[str, GoPkg]:
    """
    Merge mappings of packages into a single mapping, throwing an error on
    conflicts.
    """

    all_pkgs = {}

    for pkgs in pkgss:
        for name, pkg in pkgs.items():
            if name in all_pkgs and all_pkgs[name] != pkg:
                fail("conflict for package {!r}: {} and {}".format(name, pkg, all_pkgs[name]))
            all_pkgs[name] = pkg

    return all_pkgs

def pkg_artifacts(pkgs: dict[str, GoPkg], shared: bool) -> dict[str, Artifact]:
    """
    Return a map package name to a `shared` or `static` package artifact.
    """
    return {
        name: pkg.pkg_shared if shared else pkg.pkg
        for name, pkg in pkgs.items()
    }

def make_importcfg(
        ctx: AnalysisContext,
        prefix_name: str,
        own_pkgs: dict[str, GoPkg],
        shared: bool,
        with_importmap: bool) -> cmd_args:
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    stdlib = ctx.attrs._go_stdlib[GoStdlib]
    suffix = "__shared" if shared else ""  # suffix to make artifacts unique

    content = []
    pkg_artifacts_map = pkg_artifacts(own_pkgs, shared)
    for name_, pkg_ in pkg_artifacts_map.items():
        # Hack: we use cmd_args get "artifact" valid path and write it to a file.
        content.append(cmd_args("packagefile ", name_, "=", pkg_, delimiter = ""))

        # Note: matters for packages which do not specify package_name
        # Future work: support importmap in buck rules instead of hacking here.
        # BUG: Should use go.vendor_path instead of hard-coding values.
        for vendor_prefix in ["third-party-source/go/", "third-party-go/vendor/"]:
            if with_importmap and name_.startswith(vendor_prefix):
                real_name_ = name_.removeprefix(vendor_prefix)
                content.append(cmd_args("importmap ", real_name_, "=", name_, delimiter = ""))

    own_importcfg = ctx.actions.declare_output("{}{}.importcfg".format(prefix_name, suffix))
    ctx.actions.write(own_importcfg, content)

    final_importcfg = ctx.actions.declare_output("{}{}.final.importcfg".format(prefix_name, suffix))
    ctx.actions.run(
        [
            go_toolchain.concat_files,
            "--output",
            final_importcfg.as_output(),
            stdlib.importcfg_shared if shared else stdlib.importcfg,
            own_importcfg,
        ],
        category = "concat_importcfgs",
        identifier = prefix_name + suffix,
    )

    return cmd_args(final_importcfg, hidden = [stdlib.pkgdir_shared if shared else stdlib.pkgdir, pkg_artifacts_map.values()])

# Return "_cgo_export.h" to expose exported C declarations to non-Go rules
def cgo_exported_preprocessor(ctx: AnalysisContext, pkg_info: GoPackageInfo) -> CPreprocessor:
    return CPreprocessor(args = CPreprocessorArgs(args = [
        "-I",
        prepare_headers(
            ctx,
            {"{}/{}.h".format(ctx.label.package, ctx.label.name): pkg_info.cgo_gen_dir.project("_cgo_export.h")},
            "cgo-exported-headers",
        ).include_path,
    ]))
