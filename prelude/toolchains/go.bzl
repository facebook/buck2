load("@prelude//http_archive/exec_deps.bzl", "HttpArchiveExecDeps")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load("@prelude//decls/common.bzl", "buck")
load("@prelude//go:toolchain.bzl", "GoToolchainInfo")

def _system_go_toolchain_impl(ctx):
    go_root = ctx.attrs.go_root
    go_binary = go_root + "/bin/go"

    arch = host_info().arch
    if arch.is_aarch64:
        go_arch = "arm64"
    elif arch.is_x86_64:
        go_arch = "amd64"
    else:
        fail("Unsupported go arch: {}".format(arch))
    os = host_info().os
    if os.is_macos:
        go_os = "darwin"
    elif os.is_linux:
        go_os = "linux"
    else:
        fail("Unsupported go os: {}".format(os))

    get_go_tool = lambda go_tool: "{}/pkg/tool/{}_{}/{}".format(go_root, go_os, go_arch, go_tool)
    return [
        DefaultInfo(),
        GoToolchainInfo(
            assembler = get_go_tool("asm"),
            cgo = get_go_tool("cgo"),
            cgo_wrapper = ctx.attrs.cgo_wrapper,
            compile_wrapper = ctx.attrs.compile_wrapper,
            concat_files = ctx.attrs.concat_files,
            compiler = get_go_tool("compile"),
            cover = get_go_tool("cover"),
            cover_srcs = ctx.attrs.cover_srcs,
            cxx_toolchain_for_linking = None,
            env_go_arch = go_arch,
            env_go_os = go_os,
            env_go_root = go_root,
            external_linker_flags = None,
            filter_srcs = ctx.attrs.filter_srcs,
            gen_stdlib_importcfg = ctx.attrs.gen_stdlib_importcfg,
            go = go_binary,
            go_wrapper = ctx.attrs.go_wrapper,
            linker = get_go_tool("link"),
            packer = get_go_tool("pack"),
            tags = [],
            linker_flags = [],
            assembler_flags = [],
            compiler_flags = [],
        ),
    ]

system_go_toolchain = rule(
    impl = _system_go_toolchain_impl,
    doc = """Example system go toolchain rules (WIP). Usage:
  system_go_toolchain(
      name = "go",
      go_root = "/opt/homebrew/Cellar/go/1.20.4/libexec",
      visibility = ["PUBLIC"],
  )""",
    attrs = {
        "cgo_wrapper": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:cgo_wrapper")),
        "compile_wrapper": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:compile_wrapper")),
        "concat_files": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:concat_files")),
        "cover_srcs": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:cover_srcs")),
        "filter_srcs": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:filter_srcs")),
        "gen_stdlib_importcfg": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:gen_stdlib_importcfg")),
        "go_wrapper": attrs.default_only(attrs.dep(providers = [RunInfo], default = "prelude//go/tools:go_wrapper")),
        "go_root": attrs.string(),
    },
    is_toolchain_rule = True,
)
