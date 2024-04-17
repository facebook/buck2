# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")

GoToolchainInfo = provider(
    # @unsorted-dict-items
    fields = {
        "assembler": provider_field(RunInfo),
        "assembler_flags": provider_field(typing.Any, default = None),
        "c_compiler_flags": provider_field(typing.Any, default = None),
        "cgo": provider_field(RunInfo),
        "cgo_wrapper": provider_field(RunInfo),
        "gen_stdlib_importcfg": provider_field(RunInfo),
        "go_wrapper": provider_field(RunInfo),
        "compiler": provider_field(RunInfo),
        "compiler_flags": provider_field(typing.Any, default = None),
        "concat_files": provider_field(RunInfo),
        "cover": provider_field(RunInfo),
        "cxx_toolchain_for_linking": provider_field(CxxToolchainInfo | None, default = None),
        "env_go_arch": provider_field(str),
        "env_go_os": provider_field(str),
        "env_go_arm": provider_field(str | None, default = None),
        "env_go_root": provider_field(typing.Any, default = None),
        "env_go_debug": provider_field(dict[str, str], default = {}),
        "external_linker_flags": provider_field(typing.Any, default = None),
        "go": provider_field(RunInfo),
        "linker": provider_field(RunInfo),
        "linker_flags": provider_field(typing.Any, default = None),
        "packer": provider_field(RunInfo),
        "tags": provider_field(list[str], default = []),
    },
)

def get_toolchain_env_vars(toolchain: GoToolchainInfo, force_disable_cgo = False) -> dict[str, str | cmd_args]:
    env = {
        "GOARCH": toolchain.env_go_arch,
        # opt-out from Go1.20 coverage redesign
        "GOEXPERIMENT": "nocoverageredesign",
        "GOOS": toolchain.env_go_os,
    }

    if toolchain.env_go_arm != None:
        env["GOARM"] = toolchain.env_go_arm
    if toolchain.env_go_root != None:
        env["GOROOT"] = toolchain.env_go_root
    if toolchain.env_go_debug:
        godebug = ",".join(["{}={}".format(k, v) for k, v in toolchain.env_go_debug.items()])
        env["GODEBUG"] = godebug

    if force_disable_cgo:
        env["CGO_ENABLED"] = "0"
    else:
        # CGO is enabled by default for native compilation, but we need to set it
        # explicitly for cross-builds:
        # https://go-review.googlesource.com/c/go/+/12603/2/src/cmd/cgo/doc.go
        cxx_toolchain_available = toolchain.cxx_toolchain_for_linking != None
        if cxx_toolchain_available:
            env["CGO_ENABLED"] = "1"

    return env

# Sets default value of cgo_enabled attribute based on the presence of C++ toolchain.
def evaluate_cgo_enabled(toolchain: GoToolchainInfo, cgo_enabled: [bool, None]) -> bool:
    cxx_toolchain_available = toolchain.cxx_toolchain_for_linking != None

    if cgo_enabled and not cxx_toolchain_available:
        fail("Cgo requires a C++ toolchain. Set cgo_enabled=None|False.")

    if cgo_enabled != None:
        return cgo_enabled

    # Return True if cxx_toolchain available for current configuration, otherwise to False.
    return cxx_toolchain_available
