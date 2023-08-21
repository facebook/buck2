# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

GoToolchainInfo = provider(fields = [
    "assembler",
    "cgo_wrapper",
    "compile_wrapper",
    "compiler",
    "compiler_flags_shared",
    "compiler_flags_static",
    "concat_files",
    "cover",
    "cover_srcs",
    "cxx_toolchain_for_linking",
    "external_linker_flags",
    "filter_srcs",
    "linker",
    "linker_flags_shared",
    "linker_flags_static",
    "packer",
    "tags",
    "base",  # BaseGoToolchainInfo
    "stdlib_shared",
    "stdlib_static",
])

GoStdlib = record(
    stdlib = field("artifact"),
    importcfg = field("artifact"),
)

BaseGoToolchainInfo = record(
    env_go_arch = str,
    env_go_os = str,
    env_go_arm = [str, None],
    env_go_root = cmd_args,
    go = RunInfo.type,
    go_wrapper = RunInfo.type,
    cgo = [RunInfo.type, None],
)

def get_toolchain_cmd_args(base: BaseGoToolchainInfo, go_root = True, cgo_enabled_default = False) -> cmd_args:
    cmd = cmd_args("env")
    if base.env_go_arch != None:
        cmd.add("GOARCH={}".format(base.env_go_arch))
    if base.env_go_os != None:
        cmd.add("GOOS={}".format(base.env_go_os))
    if base.env_go_arm != None:
        cmd.add("GOARM={}".format(base.env_go_arm))
    if go_root and base.env_go_root != None:
        cmd.add(cmd_args(base.env_go_root, format = "GOROOT={}"))

    # CGO is enabled by default for native compilation, but we need to set it
    # explicitly for cross-builds:
    # https://go-review.googlesource.com/c/go/+/12603/2/src/cmd/cgo/doc.go
    if not cgo_enabled_default and base.cgo != None:
        cmd.add("CGO_ENABLED=1")

    return cmd
