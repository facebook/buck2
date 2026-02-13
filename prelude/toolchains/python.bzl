# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:prelude.bzl", "native")
load(
    "@prelude//python:toolchain.bzl",
    "PythonPlatformInfo",
    "PythonToolchainInfo",
)
load(
    "@prelude//python:python_wheel_toolchain.bzl",
    "PythonWheelToolchainInfo",
)
load(
    "@prelude//python_bootstrap:python_bootstrap.bzl",
    "PythonBootstrapToolchainInfo",
)

_INTERPRETER = select({
    "DEFAULT": "python3",
    "config//os:windows": "python.exe",
})

def _python_bootstrap_toolchain_impl(ctx):
    interpreter = ctx.attrs.interpreter if isinstance(ctx.attrs.interpreter, str) else ctx.attrs.interpreter[RunInfo].args
    return [
        DefaultInfo(),
        PythonBootstrapToolchainInfo(interpreter = interpreter),
    ]

# Creates a new bootstrap toolchain using Python that is installed on your system.
# You may use it in your toolchain cell as follows:
#
# ```bzl
# load("@prelude//toolchains:python.bzl", "system_python_bootstrap_toolchain")
#
# system_python_bootstrap_toolchain(
#     name = "python_bootstrap", # the default name rules look for
#     visibility = ["PUBLIC"],
# )
# ```
system_python_bootstrap_toolchain = rule(
    impl = _python_bootstrap_toolchain_impl,
    attrs = {
        "interpreter": attrs.string(default = _INTERPRETER),
    },
    is_toolchain_rule = True,
)

python_bootstrap_toolchain = rule(
    impl = _python_bootstrap_toolchain_impl,
    attrs = {
        "interpreter": attrs.dep(providers = [RunInfo]),
    },
    is_toolchain_rule = True,
    doc = """
        A bootstrap toolchain using the provided interpreter.

        If you want to use whatever's available on $PATH, use `system_python_bootstrap_toolchain`.
    """,
)

def _system_python_toolchain_impl(ctx):
    """
    A very simple toolchain that is hardcoded to the current environment.
    """

    return [
        DefaultInfo(),
        PythonToolchainInfo(
            binary_linker_flags = ctx.attrs.binary_linker_flags,
            linker_flags = ctx.attrs.linker_flags,
            host_interpreter = RunInfo(args = [ctx.attrs.interpreter]),
            interpreter = RunInfo(args = [ctx.attrs.interpreter]),
            compile = RunInfo(args = ["echo", "COMPILEINFO"]),
            package_style = "inplace",
            pex_extension = ctx.attrs.pex_extension,
            native_link_strategy = "separate",
        ),
        PythonPlatformInfo(name = "x86_64"),
    ]

system_python_toolchain = rule(
    impl = _system_python_toolchain_impl,
    attrs = {
        "binary_linker_flags": attrs.default_only(attrs.list(attrs.arg(), default = [])),
        "interpreter": attrs.string(default = _INTERPRETER),
        "linker_flags": attrs.default_only(attrs.list(attrs.arg(), default = [])),
        "pex_extension": attrs.string(default = ".pex"),
    },
    is_toolchain_rule = True,
)

def python_toolchain_impl(ctx) -> list[Provider]:
    return [
        DefaultInfo(),
        PythonToolchainInfo(
            interpreter = ctx.attrs.interpreter[RunInfo],
            host_interpreter = ctx.attrs.interpreter[RunInfo],
            compile = ctx.attrs.compile[DefaultInfo].default_outputs[0],
            package_style = "inplace",
            native_link_strategy = "separate",
            linker_flags = [],
            binary_linker_flags = [],
            extension_linker_flags = ctx.attrs.extension_linker_flags,
        ),
        PythonPlatformInfo(name = "x86_64"),
    ]

python_toolchain = rule(
    impl = python_toolchain_impl,
    attrs = {
        "compile": attrs.default_only(attrs.dep(default = "prelude//python/tools:compile.py")),
        "extension_linker_flags": attrs.list(attrs.arg()),
        "interpreter": attrs.dep(providers = [RunInfo]),
    },
    is_toolchain_rule = True,
    doc = "A Python toolchain that can build Python extensions, given an interpreter and the extra linker flags to use with it. See `remote_python_toolchain` for a toolchain that configures the interpreter and linker flags for you.",
)

def _python_wheel_toolchain_impl(ctx):
    return [
        DefaultInfo(),
        PythonWheelToolchainInfo(
            platform = ctx.attrs.platform,
        ),
    ]

python_wheel_toolchain = rule(
    impl = _python_wheel_toolchain_impl,
    attrs = {
        "platform": attrs.option(attrs.string(), default = None),
    },
    is_toolchain_rule = True,
)

# archives for 3.13
# update this by running `prelude//python/tools:discover_python_archives.sh`
CPYTHON_ARCHIVE = {
    "linux": {
        "arm64": {"sha256": "829d615905b5ae8c50353f2ceb3d6665793442d4cbc64503bc9b27b5b9f6fb8a", "url": "https://github.com/astral-sh/python-build-standalone/releases/download/20250807/cpython-3.13.6+20250807-aarch64-unknown-linux-gnu-install_only_stripped.tar.gz"},
        "x86_64": {"sha256": "e3e280d4b1ead63de6ebc9816de71792fc8c71b7a6a999ea82f937047beba037", "url": "https://github.com/astral-sh/python-build-standalone/releases/download/20250807/cpython-3.13.6+20250807-x86_64-unknown-linux-gnu-install_only_stripped.tar.gz"},
    },
    "macos": {
        "arm64": {"sha256": "c57e48145722a87e34a50a2c31d6b11f0830e62e26d759c11b6606a6a80e6243", "url": "https://github.com/astral-sh/python-build-standalone/releases/download/20250807/cpython-3.13.6+20250807-aarch64-apple-darwin-install_only_stripped.tar.gz"},
        "x86_64": {"sha256": "7caed8ea779dcc5d091955628455823cb0e91587703d8b6f7dd285fb2b44d1e0", "url": "https://github.com/astral-sh/python-build-standalone/releases/download/20250807/cpython-3.13.6+20250807-x86_64-apple-darwin-install_only_stripped.tar.gz"},
    },
    "windows": {
        "arm64": {"sha256": "a24c09048d1c20ed01e58e7e59a5e43b79cb7a9f36c067fc24e17649e4301d54", "url": "https://github.com/astral-sh/python-build-standalone/releases/download/20250807/cpython-3.13.6+20250807-aarch64-pc-windows-msvc-install_only_stripped.tar.gz"},
        "x86_64": {"sha256": "0de6eb2cb66211967874b0fb47cfc0299102e8a805f5319e108121e314083066", "url": "https://github.com/astral-sh/python-build-standalone/releases/download/20250807/cpython-3.13.6+20250807-x86_64-pc-windows-msvc-install_only_stripped.tar.gz"},
    },
}

def remote_python_toolchain(
        name: str,
        visibility: list[str],
        cpython_urls: dict[str, dict[str, dict[str, str]]] = CPYTHON_ARCHIVE,
        bootstrap: bool = True,
        **kwargs) -> None:
    """
    Sets up a Python toolchain by using a pre-built CPython installation, downloaded from the [python-build-standalone project](https://github.com/astral-sh/python-build-standalone).

    If `bootstrap` is set to `True`, this will also set up a bootstrap toolchain using the same interpreter.
    """

    native.http_archive(
        name = "cpython_archive",
        urls = [select({
            "prelude//os:{}".format(os): select({
                "prelude//cpu:{}".format(cpu): archive["url"]
                for cpu, archive in value.items()
            })
            for os, value in cpython_urls.items()
        })],
        sha256 = select({
            "prelude//os:{}".format(os): select({
                "prelude//cpu:{}".format(cpu): archive["sha256"]
                for cpu, archive in value.items()
            })
            for os, value in cpython_urls.items()
        }),
        strip_prefix = "python",
        sub_targets = {
            "include": [
                select({"DEFAULT": "include/python3.13", "prelude//os:windows": "include"}),
            ],
            "lib": [select({"DEFAULT": "lib", "prelude//os:windows": "libs"})],
            "python": [
                select({"DEFAULT": "bin/python", "prelude//os:windows": "python.exe"}),
            ],
        },
    )
    native.command_alias(
        name = "cpython",
        exe = ":cpython_archive[python]",
        visibility = visibility,
        resources = [":cpython_archive"],
    )

    if bootstrap:
        python_bootstrap_toolchain(
            name = "{}_bootstrap".format(name),
            visibility = visibility,
            interpreter = ":cpython",
        )

    native.genrule(
        name = "libpython_symbols",
        out = "linker_args",
        # TODO: is this necessary on Windows?
        cmd = '$(exe_target prelude//python/tools:gather_libpython_symbols) "$OUT"',
    )

    python_toolchain(
        name = name,
        visibility = visibility,
        interpreter = ":cpython",
        extension_linker_flags = select({
            "DEFAULT": ["-L$(location :cpython_archive[lib])", "@$(location :libpython_symbols)"],
            "prelude//os:windows": ["/LIBPATH:$(location :cpython_archive[lib])"],
        }),
        **kwargs
    )
