load("@fbcode//buck2/platform:utils.bzl", "read_bool")
load("@prelude//python:toolchain.bzl", "PythonPlatformInfo", "PythonToolchainInfo")
load("@prelude//utils:utils.bzl", "value_or")
load("@fbcode//buck2/platform/execution/util.bzl", "fat_platform_incompatible")

# These interpreters are distributed everywhere to mac and linux and so are
# compatible with mac+linux fat platforms.
# If we add more complex fat platforms, this will need to be updated.
fat_platform_compatible_interpreters = [
    "/usr/local/fbcode/platform009/bin/python3.8",
    "/usr/local/fbcode/platform010/bin/python3.8",
]

# We put some values into an execution dep so that we can have selects
# that are resolved in the exec configuration.
PythonToolchainExecConfigInfo = provider(fields = [
    "host_interpreter",
])

def _python_toolchain_execution_config_impl(ctx):
    return [
        DefaultInfo(),
        PythonToolchainExecConfigInfo(
            host_interpreter = ctx.attrs.host_interpreter,
        ),
    ]

_python_toolchain_execution_config = rule(
    impl = _python_toolchain_execution_config_impl,
    attrs = {
        "host_interpreter": attrs.option(attrs.arg()),
    },
)

# TODO(nmj): When upstream buildifier is landed, add types back
#def config_backed_python_toolchain(flavor: str.type) -> None:
def config_backed_python_toolchain(
        flavor,
        name = None,
        target_compatible_with = [],
        **kwargs):
    sections = ["python#" + flavor, "python"]

    keys = {
        "cache_binaries": read_bool,
        "emit_omnibus_metadata": read_bool,
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

            # Skip if value is missing or explicitly set to empty.
            if val != None and val != "":
                kwargs[key] = val
                break

    # The interpreter is invoked as part of executing binaries produced by this toolchain. And
    # so the interpreter itself needs to be target compatible with the configuration. Now, the
    # interpreter is actually just an opaque absolute path on the machine currently, so we've
    # hardcoded some aspects of its compatibility. It would be good for us to make this compatibility
    # more exhaustive so we don't get strange errors (even better would be to have targets that
    # represent the interpreters and place such compatibility there).
    if kwargs["interpreter"] not in fat_platform_compatible_interpreters:
        kwargs = {k: v for (k, v) in kwargs.items()}
        target_compatible_with = target_compatible_with + fat_platform_incompatible()

    if name == None:
        name = flavor

    # We want to be able to make decisions about the host interpreter based on the exec
    # configuration (because it will be invoked as part of the build), so we push that
    # to a helper target that's an exec dep.
    host_interpreter = kwargs.pop("host_interpreter", None)
    execution_config_name = "{}-exec".format(name)
    _python_toolchain_execution_config(
        name = execution_config_name,
        host_interpreter = host_interpreter,
        target_compatible_with = target_compatible_with,
    )

    _config_backed_python_toolchain_rule(
        name = name,
        platform_name = flavor,
        _execution_config = ":" + execution_config_name,
        target_compatible_with = target_compatible_with,
        **kwargs
    )

# TODO(nmj): When upstream buildifier is landed, add types back
#def _config_backed_python_toolchain_rule_impl(ctx: "context") -> ["provider"]:
def _config_backed_python_toolchain_rule_impl(ctx):
    exec_config = ctx.attrs._execution_config[PythonToolchainExecConfigInfo]
    return [
        DefaultInfo(),
        PythonToolchainInfo(
            compile = ctx.attrs.compile,
            interpreter = ctx.attrs.interpreter,
            host_interpreter = value_or(exec_config.host_interpreter, ctx.attrs.interpreter),
            version = ctx.attrs.version,
            package_style = ctx.attrs.package_style,
            make_source_db = ctx.attrs.make_source_db,
            make_pex_inplace = ctx.attrs.path_to_pex_inplace,
            make_pex_standalone = ctx.attrs.path_to_pex,
            make_pex_modules = ctx.attrs.path_to_pex_modules,
            native_link_strategy = ctx.attrs.native_link_strategy,
            pex_executor = ctx.attrs.path_to_pex_executer,
            pex_extension = ctx.attrs.pex_extension,
            emit_omnibus_metadata = ctx.attrs.emit_omnibus_metadata,
            # In v1, fbcode python platforms set `python.cache_binaries = false`
            # for modes building standalone packages to avoid scaling issues
            # due to size and to stamp them with non-deterministic build info
            # (e.g. T10696178).  We apply the same semantics here based on that
            # config via building these binaries locally.
            build_standalone_binaries_locally = not value_or(ctx.attrs.cache_binaries, True),
        ),
        PythonPlatformInfo(
            name = value_or(ctx.attrs.platform_name, ctx.attrs.name),
        ),
    ]

_config_backed_python_toolchain_rule = rule(
    impl = _config_backed_python_toolchain_rule_impl,
    is_toolchain_rule = True,
    attrs = {
        "cache_binaries": attrs.bool(default = True),
        "compile": attrs.source(default = "prelude//python/tools:compile.py"),
        "emit_omnibus_metadata": attrs.bool(default = False),
        "interpreter": attrs.arg(),
        "make_source_db": attrs.dep(default = "prelude//python/tools:make_source_db"),
        "native_link_strategy": attrs.enum(native.python.NativeLinkStrategy.values()),
        "package_style": attrs.enum(native.python.PackageStyle.values()),
        "path_to_pex": attrs.option(attrs.dep(providers = [RunInfo])),
        "path_to_pex_executer": attrs.option(attrs.dep(providers = [RunInfo])),
        "path_to_pex_inplace": attrs.dep(default = "@prelude//python/tools:make_pex_inplace", providers = [RunInfo]),
        "path_to_pex_modules": attrs.dep(default = "@prelude//python/tools:make_pex_modules", providers = [RunInfo]),
        "pex_extension": attrs.string(default = ".par"),
        "platform_name": attrs.option(attrs.string()),
        "version": attrs.string(),
        "_execution_config": attrs.exec_dep(providers = [PythonToolchainExecConfigInfo]),
    },
)
