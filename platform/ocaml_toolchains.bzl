load("@fbcode_macros//build_defs:fbcode_toolchains.bzl", "fbcode_toolchains")
load("@prelude//ocaml:providers.bzl", "OCamlPlatformInfo", "OCamlToolchainInfo")

def _tp2(platform, project, rule):
    return "fbcode//third-party-buck/{}/build/{}:{}".format(platform, project, rule)

def _tool(name, tool, exe):
    return fbcode_toolchains.tool_wrapper(
        base_name = "{}-{}".format(name, tool),
        exe = _tp2("{host_fbcode_platform}", "supercaml", exe + "-exe"),
    )

# Create compiler wrappers, which delegate to bytecode-built OCaml compilers
# to support cross-compiling.
def _cross_tool(name, tool, exe):
    return fbcode_toolchains.tool_wrapper(
        base_name = "{}-{}".format(name, tool),
        exe = lambda host_fbcode_platform, target_fbcode_platform, _arch: (
            _tp2(host_fbcode_platform, "supercaml", "ocamlrun-exe") if host_fbcode_platform != target_fbcode_platform else _tp2(host_fbcode_platform, "supercaml", exe + "-exe")
        ),
        args = lambda host_fbcode_platform, target_fbcode_platform, _arch: (
            ["$(location {})".format(_tp2(target_fbcode_platform, "supercaml", exe + ".byte.real"))] if host_fbcode_platform != target_fbcode_platform else []
        ),
    )

def _file(name, src_name, src):
    return fbcode_toolchains.file_wrapper(
        base_name = "{}-{}".format(name, src_name),
        src = src,
    )

def ocaml_fbcode_toolchain(name, **kwargs):
    _ocaml_toolchain(
        name = name,
        ocaml_bytecode_compiler = _cross_tool(name, "ocaml_bytecode_compiler", "ocamlc"),
        # debug (there is no 'ocamldebug' in bin; sub with 'ocamlc')
        debug = _cross_tool(name, "debug", "ocamlc"),
        dep_tool = _tool(name, "dep_tool", "ocamldep"),
        lex_compiler = _tool(name, "lex_compiler", "ocamllex"),
        ocaml_compiler = _cross_tool(name, "ocaml_compiler", "ocamlopt"),
        yacc_compiler = _tool(name, "yacc_compiler", "ocamlyacc"),
        # interop_includes (this is a location not a binary!; fix this (somehow) later)
        interop_includes = fbcode_toolchains.fmt(_tp2("{target_fbcode_platform}", "supercaml", "interop_includes")),
        binutils_ld = _file(name, "binutils_ld", _tp2("{host_fbcode_platform}", "binutils", "{arch}-facebook-linux/bin/ld")),
        binutils_as = _file(name, "binutils_as", _tp2("{host_fbcode_platform}", "binutils", "{arch}-facebook-linux/bin/as")),
        libasmrun = fbcode_toolchains.fmt(_tp2("{target_fbcode_platform}", "supercaml", "libasmrun.a")),
        **kwargs
    )

def _ocaml_toolchain_impl(ctx):
    return [
        DefaultInfo(),
        OCamlToolchainInfo(
            debug = ctx.attrs.debug[RunInfo],
            binutils_ld = ctx.attrs.binutils_ld,
            binutils_as = ctx.attrs.binutils_as,
            dep_tool = ctx.attrs.dep_tool[RunInfo],
            interop_includes = ctx.attrs.interop_includes,
            lex_compiler = ctx.attrs.lex_compiler[RunInfo],
            libasmrun = ctx.attrs.libasmrun,
            ocaml_bytecode_compiler = ctx.attrs.ocaml_bytecode_compiler[RunInfo],
            ocaml_compiler = ctx.attrs.ocaml_compiler[RunInfo],
            warnings_flags = ctx.attrs.warnings_flags,
            ocaml_compiler_flags = ctx.attrs.ocaml_compiler_flags,
            yacc_compiler = ctx.attrs.yacc_compiler[RunInfo],
        ),
        OCamlPlatformInfo(
            name = ctx.attrs.name,
        ),
    ]

_ocaml_toolchain = rule(
    impl = _ocaml_toolchain_impl,
    is_toolchain_rule = True,
    attrs = {
        "binutils_as": attrs.exec_dep(providers = [DefaultInfo]),
        "binutils_ld": attrs.exec_dep(providers = [DefaultInfo]),
        "debug": attrs.exec_dep(providers = [RunInfo]),
        "dep_tool": attrs.exec_dep(providers = [RunInfo]),
        "interop_includes": attrs.source(),
        "lex_compiler": attrs.exec_dep(providers = [RunInfo]),
        "libasmrun": attrs.source(),
        "ocaml_bytecode_compiler": attrs.exec_dep(providers = [RunInfo]),
        "ocaml_compiler": attrs.exec_dep(providers = [RunInfo]),
        "ocaml_compiler_flags": attrs.list(attrs.string(), default = []),
        "warnings_flags": attrs.string(),  # must be a single argument
        "yacc_compiler": attrs.exec_dep(providers = [RunInfo]),
    },
)
