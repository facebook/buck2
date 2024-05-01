# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load(":toolchain.bzl", "GoToolchainInfo", "get_toolchain_env_vars")

GoListOut = record(
    go_files = field(list[Artifact], default = []),
    h_files = field(list[Artifact], default = []),
    c_files = field(list[Artifact], default = []),
    cxx_files = field(list[Artifact], default = []),
    cgo_files = field(list[Artifact], default = []),
    s_files = field(list[Artifact], default = []),
    test_go_files = field(list[Artifact], default = []),
    x_test_go_files = field(list[Artifact], default = []),
    embed_files = field(list[Artifact], default = []),
    cgo_cflags = field(list[str], default = []),
    cgo_cppflags = field(list[str], default = []),
)

def go_list(ctx: AnalysisContext, pkg_name: str, srcs: list[Artifact], package_root: str, force_disable_cgo: bool, with_tests: bool, asan: bool) -> Artifact:
    go_toolchain = ctx.attrs._go_toolchain[GoToolchainInfo]
    env = get_toolchain_env_vars(go_toolchain, force_disable_cgo = force_disable_cgo)
    env["GO111MODULE"] = "off"

    go_list_out = ctx.actions.declare_output(paths.basename(pkg_name) + "_go_list.json")

    # Create file structure that `go list` can recognize
    # Use copied_dir, because embed doesn't work with symlinks
    srcs_dir = ctx.actions.copied_dir(
        "__{}_srcs_dir__".format(paths.basename(pkg_name)),
        {src.short_path.removeprefix(package_root).lstrip("/"): src for src in srcs},
    )
    tags = go_toolchain.tags + ctx.attrs._tags
    if asan:
        tags.append("asan")

    required_felds = "GoFiles,CgoFiles,HFiles,CFiles,CXXFiles,SFiles,EmbedFiles,CgoCFLAGS,CgoCPPFLAGS"
    if with_tests:
        required_felds += ",TestGoFiles,XTestGoFiles"

    go_list_args = [
        go_toolchain.go_wrapper,
        go_toolchain.go,
        ["--workdir", srcs_dir],
        ["--output", go_list_out.as_output()],
        "list",
        "-e",
        "-json=" + required_felds,
        ["-tags", ",".join(tags) if tags else []],
        ".",
    ]

    identifier = paths.basename(pkg_name)
    ctx.actions.run(go_list_args, env = env, category = "go_list", identifier = identifier)

    return go_list_out

def parse_go_list_out(srcs: list[Artifact], package_root: str, go_list_out: ArtifactValue) -> GoListOut:
    go_list = go_list_out.read_json()
    go_files, cgo_files, h_files, c_files, cxx_files, s_files, test_go_files, x_test_go_files, embed_files = [], [], [], [], [], [], [], [], []

    for src in srcs:
        # remove package_root prefix from src artifact path to match `go list` output format
        src_path = src.short_path.removeprefix(package_root).lstrip("/")
        if src_path in go_list.get("GoFiles", []):
            go_files.append(src)
        if src_path in go_list.get("CgoFiles", []):
            cgo_files.append(src)
        if src_path in go_list.get("HFiles", []):
            h_files.append(src)
        if src_path in go_list.get("CFiles", []):
            c_files.append(src)
        if src_path in go_list.get("CXXFiles", []):
            cxx_files.append(src)
        if src_path in go_list.get("SFiles", []):
            s_files.append(src)
        if src_path in go_list.get("TestGoFiles", []):
            test_go_files.append(src)
        if src_path in go_list.get("XTestGoFiles", []):
            x_test_go_files.append(src)
        if _any_starts_with(go_list.get("EmbedFiles", []), src_path):
            embed_files.append(src)

    cgo_cflags = go_list.get("CgoCFLAGS", [])
    cgo_cppflags = go_list.get("CgoCPPFLAGS", [])

    return GoListOut(
        go_files = go_files,
        h_files = h_files,
        c_files = c_files,
        cxx_files = cxx_files,
        cgo_files = cgo_files,
        s_files = s_files,
        test_go_files = test_go_files,
        x_test_go_files = x_test_go_files,
        embed_files = embed_files,
        cgo_cflags = cgo_cflags,
        cgo_cppflags = cgo_cppflags,
    )

def _any_starts_with(files: list[str], path: str):
    for file in files:
        if paths.starts_with(file, path):
            return True

    return False
