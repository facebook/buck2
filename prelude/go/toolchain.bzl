# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

GoToolchainInfo = provider(
    # @unsorted-dict-items
    fields = {
        "assembler": provider_field(typing.Any, default = None),
        "base": provider_field(typing.Any, default = None),  # BaseGoToolchainInfo
        "cgo": provider_field(typing.Any, default = None),
        "cgo_wrapper": provider_field(typing.Any, default = None),
        "compile_wrapper": provider_field(typing.Any, default = None),
        "compiler": provider_field(typing.Any, default = None),
        "compiler_flags_shared": provider_field(typing.Any, default = None),
        "compiler_flags_static": provider_field(typing.Any, default = None),
        "concat_files": provider_field(typing.Any, default = None),
        "cover": provider_field(typing.Any, default = None),
        "cover_srcs": provider_field(typing.Any, default = None),
        "cxx_toolchain_for_linking": provider_field(typing.Any, default = None),
        "external_linker_flags": provider_field(typing.Any, default = None),
        "filter_srcs": provider_field(typing.Any, default = None),
        "linker": provider_field(typing.Any, default = None),
        "linker_flags_shared": provider_field(typing.Any, default = None),
        "linker_flags_static": provider_field(typing.Any, default = None),
        "packer": provider_field(typing.Any, default = None),
        "stdlib_shared": provider_field(typing.Any, default = None),
        "stdlib_static": provider_field(typing.Any, default = None),
        "tags": provider_field(typing.Any, default = None),
    },
)

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
    cmd.add("GOARCH={}".format(base.env_go_arch))
    cmd.add("GOOS={}".format(base.env_go_os))
    if base.env_go_arm != None:
        cmd.add("GOARM={}".format(base.env_go_arm))
    if go_root:
        cmd.add(cmd_args(base.env_go_root, format = "GOROOT={}"))

    # CGO is enabled by default for native compilation, but we need to set it
    # explicitly for cross-builds:
    # https://go-review.googlesource.com/c/go/+/12603/2/src/cmd/cgo/doc.go
    if not cgo_enabled_default and base.cgo != None:
        cmd.add("CGO_ENABLED=1")

    return cmd
