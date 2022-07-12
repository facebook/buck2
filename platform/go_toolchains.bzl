load("@fbcode//buck2/platform:utils.bzl", "flags_attr", "string_attr")
load("@fbcode//buck2/prelude:paths.bzl", "paths")
load("@fbcode//buck2/prelude/go:toolchain.bzl", "GoToolchainInfo")

def config_backed_go_toolchain(name, flavor, **kwargs):
    section = "go#" + flavor
    go_platform = "linux_amd64"
    root = paths.normalize(string_attr.reader(section, "root"))
    go_toolchain(
        name = name,
        assembler = "fbcode//{}:pkg/tool/{}/asm.exe".format(root, go_platform),
        compiler = "fbcode//{}:pkg/tool/{}/compile.exe".format(root, go_platform),
        compiler_flags = [],
        external_linker_flags = flags_attr.reader(section, "external_linker_flags"),
        go = "fbcode//{}:bin/go.exe".format(root, go_platform),
        linker = "fbcode//{}:pkg/tool/{}/link.exe".format(root, go_platform),
        linker_flags = [],
        packer = "fbcode//{}:pkg/tool/{}/pack.exe".format(root, go_platform),
        **kwargs
    )

def _go_toolchain_impl(ctx):
    return [
        DefaultInfo(),
        GoToolchainInfo(
            assembler = cmd_args(ctx.attrs.assembler[RunInfo]).add(ctx.attrs.assembler_flags),
            compile_wrapper = ctx.attrs.compile_wrapper,
            compiler = cmd_args(ctx.attrs.compiler[RunInfo]).add(ctx.attrs.compiler_flags),
            external_linker_flags = ctx.attrs.external_linker_flags,
            filter_srcs = ctx.attrs.filter_srcs,
            go = ctx.attrs.go[RunInfo],
            env_go_arch = ctx.attrs.env_go_arch,
            env_go_os = ctx.attrs.env_go_os,
            env_go_root = ctx.attrs.env_go_root,
            linker = cmd_args(ctx.attrs.linker[RunInfo]).add(ctx.attrs.linker_flags),
            packer = cmd_args(ctx.attrs.packer[RunInfo]),
        ),
    ]

go_toolchain = rule(
    impl = _go_toolchain_impl,
    attrs = {
        "assembler": attr.dep(providers = [RunInfo]),
        "assembler_flags": attr.list(attr.arg(), default = []),
        "compile_wrapper": attr.dep(providers = [RunInfo], default = "fbcode//buck2/prelude/go/tools:compile_wrapper"),
        "compiler": attr.dep(providers = [RunInfo]),
        "compiler_flags": attr.list(attr.arg(), default = []),
        "env_go_arch": attr.option(attr.string()),
        "env_go_os": attr.option(attr.string()),
        "env_go_root": attr.option(attr.source()),
        "external_linker_flags": attr.list(attr.arg(), default = []),
        "filter_srcs": attr.dep(providers = [RunInfo], default = "fbcode//buck2/prelude/go/tools:filter_srcs"),
        "go": attr.dep(providers = [RunInfo]),
        "linker": attr.dep(providers = [RunInfo]),
        "linker_flags": attr.list(attr.arg(), default = []),
        "packer": attr.dep(providers = [RunInfo]),
    },
)
