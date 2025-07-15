# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxPlatformInfo")
load("@prelude//utils:arglike.bzl", "ArgLike")
load("@prelude//utils:platform_flavors_util.bzl", "by_platform")
load(":manifest.bzl", "ManifestInfo")

# The ways that Python executables handle native linkable dependencies.
NativeLinkStrategy = enum(
    # Statically links extensions into an embedded python binary
    "native",
    # Pull transitive native deps in as fully linked standalone shared libraries.
    # This is typically the fastest build-time link strategy, as it requires no
    # top-level context and therefore can shared build artifacts with all other
    # binaries using this strategy.
    "separate",
    # Statically link all transitive native deps, which don't have an explicit
    # dep from non-C/C++ code (e.g. Python), into a monolithic shared library.
    # Native dep roots, which have an explicit dep from non-C/C++ code, remain
    # as fully linked standalone shared libraries so that, typically, application
    # code doesn't need to change to work with this strategy. This strategy
    # incurs a relatively big build-time cost, but can significantly reduce the
    # size of native code and number of shared libraries pulled into the binary.
    "merged",
)

PackageStyle = enum(
    "inplace",
    "standalone",
    "inplace_lite",
)

StripLibparStrategy = enum(
    # Strip all binaries and libraries
    "full",
    # Extract debug symbols into separate files
    "extract",
    # Leave debug symbols intact
    "none",
)

PythonToolchainInfo = provider(
    # @unsorted-dict-items
    fields = {
        "build_standalone_binaries_locally": provider_field(bool | None, default = None),
        "compile": provider_field(ArgLike | None, default = None),
        # The interpreter to use to compile bytecode.
        "host_interpreter": provider_field(ArgLike | None, default = None),
        "interpreter": provider_field(ArgLike | None, default = None),
        "version": provider_field(str | None, default = None),
        "native_link_strategy": provider_field(str | None, default = None),  # Should be `NativeLinkStrategy`.
        "linker_flags": provider_field(ArgLike, default = []),
        "binary_linker_flags": provider_field(ArgLike, default = []),
        "extension_linker_flags": provider_field(ArgLike, default = []),
        "wheel_linker_flags": provider_field(ArgLike, default = []),
        # site-packages-relative rpaths to emebed into libs/bins in the wheel
        "wheel_rpaths": provider_field(ArgLike, default = []),
        "gen_lpar_bootstrap": provider_field(Dependency | None, default = None),
        "package_style": provider_field(str | None, default = None),  # Should be `PackageStyle`.
        "strip_libpar": provider_field(str | None, default = None),  # Should be `StripLibparStrategy`.
        "native_library_runtime_paths": provider_field(ArgLike, default = []),
        "native_library_env_var": provider_field(ArgLike | None, default = None),
        "make_py_package_live": provider_field(Dependency | None, default = None),
        "make_py_package_standalone": provider_field(ArgLike | None, default = None),
        "pex_extension": provider_field(str | None, default = None),
        "type_checker": provider_field(RunInfo | None, default = None),
        "typeshed_stubs": provider_field(ManifestInfo | None, default = None),
        "emit_omnibus_metadata": provider_field(bool | None, default = None),
        # The fully qualified name of a function that handles invoking the
        # executable's entry point
        "main_runner": provider_field(str, default = "__par__.bootstrap.run_as_main"),
        # Prefix to use when running a Python test/executable.
        "run_prefix": provider_field(ArgLike, default = []),
        "python_error_handler": provider_field(typing.Callable | None, default = None),
        "manifest_module_entries": provider_field(dict[str, list[str] | dict[str, typing.Any]] | None, default = None),
    },
)

# Stores "platform"/flavor name used to resolve *platform_* arguments
PythonPlatformInfo = provider(fields = {
    "name": provider_field(typing.Any, default = None),
})

def get_package_style(ctx: AnalysisContext) -> PackageStyle:
    if ctx.attrs.package_style != None:
        return PackageStyle(ctx.attrs.package_style.lower())
    return PackageStyle(ctx.attrs._python_toolchain[PythonToolchainInfo].package_style)

def get_platform_attr(
        python_platform_info: PythonPlatformInfo,
        cxx_toolchain: Dependency,
        xs: list[(str, typing.Any)]) -> list[typing.Any]:
    """
    Take a platform_* value, and the non-platform version, and concat into a list
    of values based on the cxx/python platform
    """
    if len(xs) == 0:
        return []
    cxx_info = cxx_toolchain.get(CxxPlatformInfo)
    if cxx_info == None:
        fail("Cannot use platform attrs in a fat platform configuration")
    python_platform = python_platform_info.name
    cxx_platform = cxx_info.name
    return by_platform([python_platform, cxx_platform], xs)

python = struct(
    PythonToolchainInfo = PythonToolchainInfo,
    PythonPlatformInfo = PythonPlatformInfo,
    PackageStyle = PackageStyle,
    NativeLinkStrategy = NativeLinkStrategy,
)
