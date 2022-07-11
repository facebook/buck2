load("@fbcode//buck2/platform:utils.bzl", "read_bool")
load("@fbcode//buck2/prelude/python:toolchain.bzl", "PythonPlatformInfo", "PythonToolchainInfo")
load("@fbcode//buck2/prelude/utils:utils.bzl", "value_or")

# TODO(nmj): When upstream buildifier is landed, add types back
#def config_backed_python_toolchain(flavor: str.type) -> None:
def config_backed_python_toolchain(flavor, **kwargs):
    sections = ["python#" + flavor, "python"]

    keys = {
        "cache_binaries": read_bool,
        "interpreter": native.read_config,
        "native_link_strategy": native.read_config,
        "package_style": native.read_config,
        "path_to_pex": native.read_config,
        "pex_extension": native.read_config,
        "version": native.read_config,
    }

    for key, reader in keys.items():
        if key in kwargs:
            continue
        for section in sections:
            val = reader(section, key, None)
            if val != None:
                kwargs[key] = val
                break

    _config_backed_python_toolchain_rule(
        name = flavor,
        **kwargs
    )

# TODO(nmj): When upstream buildifier is landed, add types back
#def _config_backed_python_toolchain_rule_impl(ctx: "context") -> ["provider"]:
def _config_backed_python_toolchain_rule_impl(ctx):
    return [
        DefaultInfo(),
        PythonToolchainInfo(
            compile = ctx.attr.compile,
            interpreter = ctx.attr.interpreter,
            version = ctx.attr.version,
            package_style = ctx.attr.package_style,
            make_source_db = ctx.attr.make_source_db,
            make_pex_inplace = ctx.attr.path_to_pex_inplace,
            make_pex_standalone = ctx.attr.path_to_pex,
            make_pex_modules = ctx.attr.path_to_pex_modules,
            native_link_strategy = ctx.attr.native_link_strategy,
            pex_executor = ctx.attr.path_to_pex_executer,
            pex_extension = ctx.attr.pex_extension,
            # In v1, fbcode python platforms set `python.cache_binaries = false`
            # for modes building standalone packages to avoid scaling issues
            # due to size and to stamp them with non-deterministic build info
            # (e.g. T10696178).  We apply the same semantics here based on that
            # config via building these binaries locally.
            build_standalone_binaries_locally = not value_or(ctx.attr.cache_binaries, True),
        ),
        PythonPlatformInfo(
            name = ctx.attr.name,
        ),
    ]

_config_backed_python_toolchain_rule = rule(
    impl = _config_backed_python_toolchain_rule_impl,
    attrs = {
        "cache_binaries": attr.bool(default = True),
        "compile": attr.source(default = "fbcode//buck2/prelude/python/tools:compile.py"),
        "interpreter": attr.arg(),
        "make_source_db": attr.dep(default = "fbcode//buck2/prelude/python/tools:make_source_db"),
        "native_link_strategy": attr.enum(native.python.NativeLinkStrategy.values()),
        "package_style": attr.enum(native.python.PackageStyle.values()),
        "path_to_pex": attr.dep(default = "@fbcode//tools/make_par:buck_make_par", providers = [RunInfo]),
        "path_to_pex_executer": attr.option(attr.dep(providers = [RunInfo])),
        "path_to_pex_inplace": attr.dep(default = "@fbcode//buck2/prelude/python/tools:make_pex_inplace", providers = [RunInfo]),
        "path_to_pex_modules": attr.dep(default = "@fbcode//buck2/prelude/python/tools:make_pex_modules", providers = [RunInfo]),
        "pex_extension": attr.string(default = ".par"),
        "version": attr.string(),
    },
)
