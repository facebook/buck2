# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load(
    "@prelude//linking:link_info.bzl",
    "LinkArgs",
    "LinkedObject",  # @unused Used as a type
)
load("@prelude//linking:strip.bzl", "strip_object")

SharedLibrary = record(
    lib = field(LinkedObject),
    # The LinkArgs used to produce this SharedLibrary. This can be useful for debugging or
    # for downstream rules to reproduce the shared library with some modifications (for example
    # android relinker will link again with an added version script argument).
    # TODO(cjhopman): This is currently always available.
    link_args = field(list[LinkArgs] | None, None),
    # The sonames of the shared libraries that this links against.
    # TODO(cjhopman): This is currently always available.
    shlib_deps = field(list[str] | None, None),
    stripped_lib = field(Artifact | None, None),
    can_be_asset = field(bool, False),
    for_primary_apk = field(bool, False),
    soname = field(str),
    label = field(Label),
)

SharedLibraries = record(
    # A mapping of shared library SONAME (e.g. `libfoo.so.2`) to the artifact.
    # Since the SONAME is what the dynamic loader uses to uniquely identify
    # libraries, using this as the key allows easily detecting conflicts from
    # dependencies.
    libraries = field(list[SharedLibrary]),
)

# T-set of SharedLibraries
SharedLibrariesTSet = transitive_set()

# Shared libraries required by top-level packaging rules (e.g. shared libs
# for Python binary, symlink trees of shared libs for C++ binaries)
SharedLibraryInfo = provider(fields = {
    "set": provider_field(SharedLibrariesTSet | None, default = None),
})

def get_strip_non_global_flags(cxx_toolchain: CxxToolchainInfo) -> list:
    if cxx_toolchain.strip_flags_info and cxx_toolchain.strip_flags_info.strip_non_global_flags:
        return cxx_toolchain.strip_flags_info.strip_non_global_flags

    return ["--strip-unneeded"]

def create_shared_libraries(
        ctx: AnalysisContext,
        libraries: dict[str, LinkedObject]) -> SharedLibraries:
    """
    Take a mapping of dest -> src and turn it into a mapping that will be
    passed around in providers. Used for both srcs, and resources.
    """
    cxx_toolchain = getattr(ctx.attrs, "_cxx_toolchain", None)
    return SharedLibraries(
        libraries = [SharedLibrary(
            lib = shlib,
            stripped_lib = strip_object(
                ctx,
                cxx_toolchain[CxxToolchainInfo],
                shlib.output,
                cmd_args(get_strip_non_global_flags(cxx_toolchain[CxxToolchainInfo])),
            ) if cxx_toolchain != None else None,
            link_args = shlib.link_args,
            shlib_deps = None,  # TODO(cjhopman): we need this figured out.
            can_be_asset = getattr(ctx.attrs, "can_be_asset", False) or False,
            for_primary_apk = getattr(ctx.attrs, "used_by_wrap_script", False),
            label = ctx.label,
            soname = name,
        ) for (name, shlib) in libraries.items()],
    )

# Merge multiple SharedLibraryInfo. The value in `node` represents a set of
# SharedLibraries that is provided by the target being analyzed. It's optional
# because that might not always exist, e.g. a Python library can pass through
# SharedLibraryInfo but it cannot produce any. The value in `deps` represents
# all the inherited shared libraries for this target.
def merge_shared_libraries(
        actions: AnalysisActions,
        node: [SharedLibraries, None] = None,
        deps: list[SharedLibraryInfo] = []) -> SharedLibraryInfo:
    kwargs = {}

    children = filter(None, [dep.set for dep in deps])
    if children:
        kwargs["children"] = children
    if node:
        kwargs["value"] = node

    set = actions.tset(SharedLibrariesTSet, **kwargs) if kwargs else None
    return SharedLibraryInfo(set = set)

def traverse_shared_library_info(info: SharedLibraryInfo):  # -> list[SharedLibrary]:
    libraries = []
    if info.set:
        for libs in info.set.traverse():
            libraries.extend(libs.libraries)
    return libraries

# Helper to merge shlibs, throwing an error if more than one have the same SONAME.
def _merge_shlibs(
        shared_libs: list[SharedLibrary],
        resolve_soname: typing.Callable) -> dict[str, SharedLibrary]:
    merged = {}
    for shlib in shared_libs:
        soname = resolve_soname(shlib.soname)
        existing = merged.get(soname)
        if existing != None and existing.lib != shlib.lib:
            error = (
                "Duplicate library {}! Conflicting mappings:\n" +
                "{} from {}\n" +
                "{} from {}"
            )
            fail(
                error.format(
                    shlib.soname,
                    existing.lib,
                    existing.label,
                    shlib.lib,
                    shlib.label,
                ),
            )
        merged[soname] = shlib
    return merged

def with_unique_sonames(shared_libs: list[SharedLibrary]) -> dict[str, SharedLibrary]:
    """
    Convert a list of `SharedLibrary`s to a map of unique SONAMEs to the
    corresponding `SharedLibrary`.

    Will fail if the same SONAME maps to multiple `SharedLibrary`s.
    """
    return _merge_shlibs(
        shared_libs = shared_libs,
        resolve_soname = lambda s: s,
    )

def create_shlib_symlink_tree(actions: AnalysisActions, out: str, shared_libs: list[SharedLibrary]):
    """
    Merged shared libs into a symlink tree mapping the library's SONAME to
    it's artifact.
    """
    merged = with_unique_sonames(shared_libs = shared_libs)
    return actions.symlinked_dir(
        out,
        {name: shlib.lib.output for name, shlib in merged.items()},
    )
