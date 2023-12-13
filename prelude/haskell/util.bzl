# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load(
    "@prelude//cxx:cxx_toolchain_types.bzl",
    "CxxPlatformInfo",
)
load(
    "@prelude//linking:link_info.bzl",
    "LinkStyle",
)
load("@prelude//utils:platform_flavors_util.bzl", "by_platform")
load("@prelude//utils:utils.bzl", "flatten")

HASKELL_EXTENSIONS = [
    ".hs",
    ".lhs",
    ".hsc",
    ".chs",
    ".x",
    ".y",
]

# We take a named_set for srcs, which is sometimes a list, sometimes a dict.
# In future we should only accept a list, but for now, cope with both.
def srcs_to_pairs(srcs) -> list[(str, Artifact)]:
    if type(srcs) == type({}):
        return srcs.items()
    else:
        return [(src.short_path, src) for src in srcs]

def is_haskell_src(x: str) -> bool:
    _, ext = paths.split_extension(x)
    return ext in HASKELL_EXTENSIONS

def src_to_module_name(x: str) -> str:
    base, _ext = paths.split_extension(x)
    return base.replace("/", ".")

def _by_platform(ctx: AnalysisContext, xs: list[(str, list[typing.Any])]) -> list[typing.Any]:
    platform = ctx.attrs._cxx_toolchain[CxxPlatformInfo].name
    return flatten(by_platform([platform], xs))

def attr_deps(ctx: AnalysisContext) -> list[Dependency]:
    return ctx.attrs.deps + _by_platform(ctx, ctx.attrs.platform_deps)

def _link_style_extensions(link_style: LinkStyle) -> (str, str):
    if link_style == LinkStyle("shared"):
        return ("dyn_o", "dyn_hi")
    elif link_style == LinkStyle("static_pic"):
        return ("o", "hi")  # is this right?
    elif link_style == LinkStyle("static"):
        return ("o", "hi")
    fail("unknown LinkStyle")

def output_extensions(
        link_style: LinkStyle,
        profiled: bool) -> (str, str):
    osuf, hisuf = _link_style_extensions(link_style)
    if profiled:
        return ("p_" + osuf, "p_" + hisuf)
    else:
        return (osuf, hisuf)

# Single place to build the suffix used in artifacts (e.g. package directories,
# lib names) considering attributes like link style and profiling.
def get_artifact_suffix(link_style: LinkStyle, enable_profiling: bool, suffix: str = "") -> str:
    artifact_suffix = link_style.value
    if enable_profiling:
        artifact_suffix += "-prof"
    return artifact_suffix + suffix
