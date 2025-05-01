# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//decls:core_rules.bzl", "PlatformExePlatform", "TargetCpuType")

Os = enum(
    "fat_mac_linux",
    "freebsd",
    "unknown",
    *PlatformExePlatform
)

ScriptLanguage = enum(
    "sh",  # Unix shell script
    "bat",  # Windows batch file
)

OsLookup = provider(fields = {
    "cpu": str | None,
    "os": Os,
    "script": ScriptLanguage,
})

def _os_lookup_impl(ctx: AnalysisContext):
    return [
        DefaultInfo(),
        OsLookup(
            cpu = ctx.attrs.cpu,
            os = Os(ctx.attrs.os),
            script = ScriptLanguage(ctx.attrs.script),
        ),
    ]

os_lookup = rule(impl = _os_lookup_impl, attrs = {
    "cpu": attrs.option(attrs.enum(TargetCpuType), default = None),
    "os": attrs.enum(Os.values()),
    "script": attrs.enum(ScriptLanguage.values()),
})
