# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//csharp:toolchain.bzl", "CSharpToolchainInfo")

def _system_csharp_toolchain_impl(ctx):
    if not host_info().os.is_windows:
        fail("csharp toolchain only supported on windows for now")

    return [
        DefaultInfo(),
        CSharpToolchainInfo(
            csc = RunInfo(args = ctx.attrs.csc),
        ),
    ]

system_csharp_toolchain = rule(
    impl = _system_csharp_toolchain_impl,
    doc = """Example system C# toolchain that invokes csc using the current environment path. Usage:
  system_csharp_toolchain(
      name = "csharp",
      csc = "csc.exe",
      visibility = ["PUBLIC"],
  )""",
    attrs = {
        "csc": attrs.string(default = "csc.exe"),
    },
    is_toolchain_rule = True,
)
