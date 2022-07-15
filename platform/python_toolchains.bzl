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
            compile = ctx.attrs.compile,
            interpreter = ctx.attrs.interpreter,
            version = ctx.attrs.version,
            package_style = ctx.attrs.package_style,
            make_source_db = ctx.attrs.make_source_db,
            make_pex_inplace = ctx.attrs.path_to_pex_inplace,
            make_pex_standalone = ctx.attrs.path_to_pex,
            make_pex_modules = ctx.attrs.path_to_pex_modules,
            native_link_strategy = ctx.attrs.native_link_strategy,
            pex_executor = ctx.attrs.path_to_pex_executer,
            pex_extension = ctx.attrs.pex_extension,
            # In v1, fbcode python platforms set `python.cache_binaries = false`
            # for modes building standalone packages to avoid scaling issues
            # due to size and to stamp them with non-deterministic build info
            # (e.g. T10696178).  We apply the same semantics here based on that
            # config via building these binaries locally.
            build_standalone_binaries_locally = not value_or(ctx.attrs.cache_binaries, True),
        ),
        PythonPlatformInfo(
            name = ctx.attrs.name,
        ),
    ]

_config_backed_python_toolchain_rule = rule(
    impl = _config_backed_python_toolchain_rule_impl,
    attrs = {
        "cache_binaries": attrs.bool(default = True),
        "compile": attrs.source(default = "fbcode//buck2/prelude/python/tools:compile.py"),
        "interpreter": attrs.arg(),
        "make_source_db": attrs.dep(default = "fbcode//buck2/prelude/python/tools:make_source_db"),
        "native_link_strategy": attrs.enum(native.python.NativeLinkStrategy.values()),
        "package_style": attrs.enum(native.python.PackageStyle.values()),
        "path_to_pex": attrs.dep(default = "@fbcode//tools/make_par:buck_make_par", providers = [RunInfo]),
        "path_to_pex_executer": attrs.option(attrs.dep(providers = [RunInfo])),
        "path_to_pex_inplace": attrs.dep(default = "@fbcode//buck2/prelude/python/tools:make_pex_inplace", providers = [RunInfo]),
        "path_to_pex_modules": attrs.dep(default = "@fbcode//buck2/prelude/python/tools:make_pex_modules", providers = [RunInfo]),
        "pex_extension": attrs.string(default = ".par"),
        "version": attrs.string(),
    },
)
