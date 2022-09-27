load("@fbcode_macros//build_defs:fbcode_toolchains.bzl", "fbcode_toolchains")
load("@fbcode_macros//build_defs:third_party_config.bzl", "third_party_config")
load("@fbsource//tools/build_defs:selects.bzl", "selects")
load("@prelude//go:toolchain.bzl", "GoToolchainInfo")

# Select a GOARCH using FB contraints set on a target platform.
_GO_ARCH = select({
    "ovr_config//cpu/constraints:arm64": "arm64",
    "ovr_config//cpu/constraints:x86_64": "amd64",
})

# Select a GOOS using FB contraints set on a target platform.
_GO_OS = select({
    "ovr_config//os/constraints:linux": "linux",
})

# Select a Go platform name using FB contraints set on a target platform.
_GO_PLATFORM = selects.apply_n(
    [_GO_OS, _GO_ARCH],
    lambda os, arch: "{}_{}".format(os, arch),
)

_GO_FBCODE_PKG_HOST = "fbcode//third-party-buck/{host_fbcode_platform}/tools/go"
_GO_FBCODE_PKG_TARGET = "fbcode//third-party-buck/{target_fbcode_platform}/tools/go"

# Map architecture name to their Go equivalents.
_ARCH_MAP = {
    "aarch64": "arm64",
    "x86_64": "amd64",
}

def _go_pkg_tool(name, tool):
    return fbcode_toolchains.tool_wrapper(
        base_name = name + "-" + tool,
        exe = lambda host_fbcode_platform, _target_fbode_platform, _arch: (
            (_GO_FBCODE_PKG_HOST + ":pkg/tool/linux_{arch}/{tool}").format(
                host_fbcode_platform = host_fbcode_platform,
                arch = _ARCH_MAP[third_party_config["platforms"][host_fbcode_platform]["architecture"]],
                tool = tool,
            )
        ),
    )

def _go_bin_tool(name, tool):
    return fbcode_toolchains.tool_wrapper(
        base_name = name + "-" + tool,
        exe = _GO_FBCODE_PKG_HOST + ":bin/{}".format(tool),
    )

def _go_root():
    return fbcode_toolchains.fmt(
        selects.apply(
            _GO_PLATFORM,
            lambda gp: "{}:goroot-{}".format(_GO_FBCODE_PKG_TARGET, gp),
        ),
    )

def go_fbcode_toolchain(name, assembler_flags = [], **kwargs):
    """
    Create a Go toolchain for fbcode platforms.
    """

    go_toolchain(
        name = name,
        assembler = _go_pkg_tool(name, "asm"),
        assembler_flags = fbcode_toolchains.fmtl([
            "-I",
            "$(location {}:pkg/include)".format(_GO_FBCODE_PKG_TARGET),
        ]) + assembler_flags,
        compiler = _go_pkg_tool(name, "compile"),
        cgo = _go_pkg_tool(name, "cgo"),
        go = _go_bin_tool(name, "go"),
        linker = _go_pkg_tool(name, "link"),
        packer = _go_pkg_tool(name, "pack"),
        env_go_root = _go_root(),
        env_go_arch = _GO_ARCH,
        env_go_os = _GO_OS,
        compatible_with = [
            "ovr_config//runtime:fbcode",
        ],
        **kwargs
    )

def _go_toolchain_impl(ctx):
    return [
        DefaultInfo(),
        GoToolchainInfo(
            assembler = cmd_args(ctx.attrs.assembler[RunInfo]).add(ctx.attrs.assembler_flags),
            cgo = ctx.attrs.cgo[RunInfo],
            compile_wrapper = ctx.attrs.compile_wrapper,
            compiler = cmd_args(ctx.attrs.compiler[RunInfo]).add(ctx.attrs.compiler_flags),
            external_linker_flags = ctx.attrs.external_linker_flags,
            filter_srcs = ctx.attrs.filter_srcs,
            cgo_wrapper = ctx.attrs.cgo_wrapper,
            go = ctx.attrs.go[RunInfo],
            env_go_arch = ctx.attrs.env_go_arch,
            env_go_os = ctx.attrs.env_go_os,
            env_go_root = ctx.attrs.env_go_root,
            linker = cmd_args(ctx.attrs.linker[RunInfo]).add(ctx.attrs.linker_flags),
            packer = cmd_args(ctx.attrs.packer[RunInfo]),
            tags = ctx.attrs.tags,
        ),
    ]

go_toolchain = rule(
    impl = _go_toolchain_impl,
    is_toolchain_rule = True,
    attrs = {
        "assembler": attrs.exec_dep(providers = [RunInfo]),
        "assembler_flags": attrs.list(attrs.arg(), default = []),
        "cgo": attrs.exec_dep(providers = [RunInfo]),
        "cgo_wrapper": attrs.exec_dep(providers = [RunInfo], default = "prelude//go/tools:cgo_wrapper"),
        "compile_wrapper": attrs.exec_dep(providers = [RunInfo], default = "prelude//go/tools:compile_wrapper"),
        "compiler": attrs.exec_dep(providers = [RunInfo]),
        "compiler_flags": attrs.list(attrs.arg(), default = []),
        "env_go_arch": attrs.option(attrs.string()),
        "env_go_os": attrs.option(attrs.string()),
        "env_go_root": attrs.option(attrs.source()),
        "external_linker_flags": attrs.list(attrs.arg(), default = []),
        "filter_srcs": attrs.exec_dep(providers = [RunInfo], default = "prelude//go/tools:filter_srcs"),
        "go": attrs.exec_dep(providers = [RunInfo]),
        "linker": attrs.exec_dep(providers = [RunInfo]),
        "linker_flags": attrs.list(attrs.arg(), default = []),
        "packer": attrs.exec_dep(providers = [RunInfo]),
        "tags": attrs.list(attrs.string()),
    },
)
