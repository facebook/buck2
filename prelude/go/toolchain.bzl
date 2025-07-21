# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

GoDistrInfo = provider(
    # @unsorted-dict-items
    fields = {
        "bin_go": provider_field(RunInfo),
        "go_root": provider_field(Artifact | None),
        "tool_asm": provider_field(RunInfo),
        "tool_compile": provider_field(RunInfo),
        "tool_cover": provider_field(RunInfo),
        "tool_cgo": provider_field(RunInfo),
        "tool_link": provider_field(RunInfo),
    },
)

GoToolchainInfo = provider(
    # @unsorted-dict-items
    fields = {
        "assembler": provider_field(RunInfo),
        "assembler_flags": provider_field(typing.Any, default = None),
        "cxx_compiler_flags": provider_field(typing.Any, default = None),
        "cgo": provider_field(RunInfo),
        "gen_stdlib_importcfg": provider_field(RunInfo),
        "go_wrapper": provider_field(RunInfo),
        "compiler": provider_field(RunInfo),
        "compiler_flags": provider_field(typing.Any, default = None),
        "concat_files": provider_field(RunInfo),
        "cover": provider_field(RunInfo),
        "env_go_arch": provider_field(str),
        "env_go_os": provider_field(str),
        "env_go_arm": provider_field(str | None, default = None),
        "env_go_root": provider_field(Artifact | None, default = None),
        "env_go_debug": provider_field(dict[str, str], default = {}),
        "external_linker_flags": provider_field(typing.Any, default = None),
        "go": provider_field(RunInfo),
        "linker": provider_field(RunInfo),
        "linker_flags": provider_field(typing.Any, default = None),
        "packer": provider_field(RunInfo),
        "build_tags": provider_field(list[str], default = []),
    },
)

def get_toolchain_env_vars(toolchain: GoToolchainInfo) -> dict[str, str | cmd_args | Artifact]:
    env = {
        "GOARCH": toolchain.env_go_arch,
        "GOEXPERIMENT": "",
        "GOOS": toolchain.env_go_os,
    }

    if toolchain.env_go_arm != None:
        env["GOARM"] = toolchain.env_go_arm
    if toolchain.env_go_debug:
        godebug = ",".join(["{}={}".format(k, v) for k, v in toolchain.env_go_debug.items()])
        env["GODEBUG"] = godebug

    return env

# Sets default value of cgo_enabled attribute based on availability of CxxToolchain
def evaluate_cgo_enabled(cxx_toolchain_available: bool, cgo_enabled: [bool, None], override_cgo_enabled: [bool, None] = None) -> bool:
    if not cxx_toolchain_available and (cgo_enabled or override_cgo_enabled):
        fail("CGo can't be enabled because CxxToolchain is not available for current configuration")

    if override_cgo_enabled != None:
        return override_cgo_enabled

    if cgo_enabled != None:
        return cgo_enabled

    return cxx_toolchain_available
