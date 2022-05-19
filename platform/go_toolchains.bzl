load("@fbcode//buck2/platform:utils.bzl", "flags_attr", "string_attr")
load("@fbcode//buck2/prelude:paths.bzl", "paths")
load("@fbcode//buck2/prelude/go:toolchain.bzl", "GoToolchainInfo")

def config_backed_go_toolchain(name, flavor, **kwargs):
    section = "go#" + flavor
    go_platform = "linux_amd64"
    root = paths.normalize(string_attr.reader(section, "root"))
    _config_backed_go_toolchain_rule(
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

def _config_backed_go_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        GoToolchainInfo(
            assembler = cmd_args(ctx.attr.assembler[RunInfo]),
            compile_wrapper = ctx.attr.compile_wrapper,
            compiler = cmd_args(ctx.attr.compiler[RunInfo]).add(ctx.attr.compiler_flags),
            external_linker_flags = ctx.attr.external_linker_flags,
            filter_srcs = ctx.attr.filter_srcs,
            go = ctx.attr.go[RunInfo],
            linker = cmd_args(ctx.attr.linker[RunInfo]).add(ctx.attr.linker_flags),
            packer = cmd_args(ctx.attr.packer[RunInfo]),
        ),
    ]

_config_backed_go_toolchain_rule = rule(
    implementation = _config_backed_go_toolchain_rule_impl,
    attrs = {
        "assembler": attr.dep(providers = [RunInfo]),
        "compile_wrapper": attr.dep(providers = [RunInfo], default = "fbcode//buck2/prelude/go/tools:compile_wrapper"),
        "compiler": attr.dep(providers = [RunInfo]),
        "compiler_flags": attr.list(attr.arg()),
        "external_linker_flags": attr.list(attr.arg()),
        "filter_srcs": attr.dep(providers = [RunInfo], default = "fbcode//buck2/prelude/go/tools:filter_srcs"),
        "go": attr.dep(providers = [RunInfo]),
        "linker": attr.dep(providers = [RunInfo]),
        "linker_flags": attr.list(attr.arg()),
        "packer": attr.dep(providers = [RunInfo]),
    },
)
