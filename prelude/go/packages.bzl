# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load("@prelude//cxx:headers.bzl", "prepare_headers")
load(
    "@prelude//cxx:preprocessor.bzl",
    "CPreprocessor",
    "CPreprocessorArgs",
)
load("@prelude//utils:utils.bzl", "value_or")

# Information about a package for GOPACKAGESDRIVER
GoPackageInfo = provider(
    fields = {
        "build_out": provider_field(Artifact),
        "cgo_gen_dir": provider_field(Artifact),
        "go_list_out": provider_field(Artifact),
        "package_name": provider_field(str),
        "package_root": provider_field(str),
        # Full list of package source files
        # including generated and files of the package under test
        "srcs": provider_field(list[Artifact]),
    },
)

GoPkg = record(
    # We have to produce allways shared (PIC) and non-shared (non-PIC) archives
    pkg = field(Artifact),
    pkg_shared = field(Artifact),
    # The content of export_file and export_file_shared is likely to be the same
    # but we need to produce both to avoid running compilation twice.
    export_file = field(Artifact),
    export_file_shared = field(Artifact),
    coverage_vars = field(cmd_args),
    test_go_files = field(cmd_args),
)

StdPkg = record(
    a_file = field(Artifact),
    a_file_shared = field(Artifact),
)

GoStdlibDynamicValue = provider(
    fields = {
        "pkgs": provider_field(dict[str, StdPkg]),
    },
)

GoStdlib = provider(
    fields = {
        "dynamic_value": provider_field(DynamicValue),  # GoStdlibDynamicValue inside
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

def export_files(pkgs: dict[str, GoPkg], shared: bool) -> dict[str, Artifact]:
    """
    Return a map package name to a `shared` or `static` package artifact.
    """
    return {
        name: pkg.export_file_shared if shared else pkg.export_file
        for name, pkg in pkgs.items()
    }

def make_importcfg(
        actions: AnalysisActions,
        stdlib: GoStdlibDynamicValue,
        own_pkgs: dict[str, GoPkg],
        shared: bool,
        link: bool) -> Artifact:
    content = []
    a_files = []
    pkg_artifacts_map = pkg_artifacts(own_pkgs, shared) if link else export_files(own_pkgs, shared)
    for name_, pkg_ in pkg_artifacts_map.items():
        # Hack: we use cmd_args get "artifact" valid path and write it to a file.
        content.append(cmd_args("packagefile ", name_, "=", pkg_, delimiter = "", hidden = [pkg_]))
        a_files.append(pkg_)

    for name_, pkg_ in stdlib.pkgs.items():
        a_file = pkg_.a_file_shared if shared else pkg_.a_file
        content.append(cmd_args("packagefile ", name_, "=", a_file, delimiter = "", hidden = [a_file]))
        a_files.append(a_file)

    importcfg = actions.declare_output("{}.importcfg".format("shared" if shared else "non_shared"), has_content_based_path = True)
    actions.write(importcfg, content)

    return importcfg.with_associated_artifacts(a_files)

# Return "_cgo_export.h" to expose exported C declarations to non-Go rules
def cgo_exported_preprocessor(ctx: AnalysisContext, pkg_info: GoPackageInfo) -> CPreprocessor:
    cxx_toolchain_info = ctx.attrs._cxx_toolchain[CxxToolchainInfo]
    return CPreprocessor(args = CPreprocessorArgs(args = [
        "-I",
        prepare_headers(
            ctx.actions,
            cxx_toolchain_info,
            {"{}/{}.h".format(ctx.label.package, ctx.label.name): pkg_info.cgo_gen_dir.project("_cgo_export.h")},
            "cgo-exported-headers",
            uses_content_based_paths = True,
        ).include_path,
    ]))
