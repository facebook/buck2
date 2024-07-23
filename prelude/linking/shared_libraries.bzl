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
load("@prelude//utils:expect.bzl", "expect")

Soname = record(
    # Return the SONAME if it's a string, otherwise None.
    as_str = field(typing.Callable),
    # Return the SONAME as a string, throwing an error if it is actually an
    # artifact.
    ensure_str = field(typing.Callable),
    # Return `True` if the SONAME is respresented as a string.
    is_str = field(typing.Callable),
    # The the actual SONAME can be rerepsented by a static string, or the
    # contents of a file genrated at build time.
    _soname = field(str | Artifact),
)

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
    soname = field(Soname),
    label = field(Label),
)

def _ensure_str(soname: str | Artifact) -> str:
    expect(type(soname) == type(""), "SONAME is not a `str`: {}", soname)
    return soname

def to_soname(soname: str | Artifact | Soname) -> Soname:
    if isinstance(soname, Soname):
        return soname
    return Soname(
        as_str = lambda: soname if type(soname) == type("") else None,
        ensure_str = lambda: _ensure_str(soname),
        is_str = lambda: type(soname) == type(""),
        _soname = soname,
    )

def create_shlib(
        # The soname can either be a string or an artifact with the soname in
        # text form.
        soname: str | Artifact | Soname,
        **kwargs):
    return SharedLibrary(
        soname = to_soname(soname),
        **kwargs
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

def create_shlib_from_ctx(
        ctx: AnalysisContext,
        soname: str | Artifact | Soname,
        lib: LinkedObject):
    cxx_toolchain = getattr(ctx.attrs, "_cxx_toolchain", None)
    return create_shlib(
        lib = lib,
        stripped_lib = strip_object(
            ctx,
            cxx_toolchain[CxxToolchainInfo],
            lib.output,
            cmd_args(get_strip_non_global_flags(cxx_toolchain[CxxToolchainInfo])),
        ) if cxx_toolchain != None else None,
        link_args = lib.link_args,
        shlib_deps = None,  # TODO(cjhopman): we need this figured out.
        can_be_asset = getattr(ctx.attrs, "can_be_asset", False) or False,
        for_primary_apk = getattr(ctx.attrs, "used_by_wrap_script", False),
        label = ctx.label,
        soname = soname,
    )

def create_shared_libraries(
        ctx: AnalysisContext,
        libraries: dict[str, LinkedObject]) -> SharedLibraries:
    """
    Take a mapping of dest -> src and turn it into a mapping that will be
    passed around in providers. Used for both srcs, and resources.
    """
    return SharedLibraries(
        libraries = [
            create_shlib_from_ctx(ctx = ctx, soname = name, lib = shlib)
            for (name, shlib) in libraries.items()
        ],
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

def with_unique_str_sonames(
        shared_libs: list[SharedLibrary],
        skip_dynamic: bool = False) -> dict[str, SharedLibrary]:
    """
    Convert a list of `SharedLibrary`s to a map of unique SONAMEs to the
    corresponding `SharedLibrary`.

    Will fail if the same SONAME maps to multiple `SharedLibrary`s.
    """
    return _merge_shlibs(
        shared_libs = [
            shlib
            for shlib in shared_libs
            if shlib.soname.is_str() or not skip_dynamic
        ],
        resolve_soname = lambda s: s.ensure_str(),
    )

def gen_shared_libs_action(
        actions: AnalysisActions,
        out: str,
        shared_libs: list[SharedLibrary],
        gen_action: typing.Callable,
        dir = False):
    """
    Produce an action by first resolving all SONAME of the given shlibs and
    enforcing that each SONAME is unique.

    The provided `gen_action` callable is called with a map of unique SONAMEs
    to the corresponding shlibs.
    """

    output = actions.declare_output(out, dir = dir)

    def func(actions, artifacts, output):
        def resolve_soname(soname):
            if soname.is_str():
                return soname._soname
            else:
                return artifacts[soname._soname].read_string().strip()

        gen_action(
            actions,
            output,
            _merge_shlibs(
                shared_libs = shared_libs,
                resolve_soname = resolve_soname,
            ),
        )

    dynamic_sonames = [shlib.soname._soname for shlib in shared_libs if not shlib.soname.is_str()]
    if dynamic_sonames:
        actions.dynamic_output(
            dynamic = [shlib.soname._soname for shlib in shared_libs if not shlib.soname.is_str()],
            inputs = [],
            outputs = [output.as_output()],
            f = lambda ctx, artifacts, outputs: func(ctx.actions, artifacts, outputs[output]),
        )
    else:
        func(actions, {}, output)

    return output

def zip_shlibs(
        merged: dict[str, SharedLibrary],
        vals: list[(SharedLibrary, typing.Any)]) -> list[(str, SharedLibrary, typing.Any)]:
    """
    Helper to "zip" together the soname->shlib map to a list with associated
    shared lib values.

    This is useful for callers of `gen_shared_libs_action` to combine the merged
    shared libs, in dedup'd dict form, with some additional data.
    """

    zipped = []

    # Walk through the shlib and val tuples
    idx = 0
    for soname, shlib in merged.items():
        for idx in range(idx, len(vals)):
            if vals[idx][0] == shlib:
                break
        zipped.append((soname, shlib, vals[idx][1]))

    return zipped

def create_shlib_symlink_tree(actions: AnalysisActions, out: str, shared_libs: list[SharedLibrary]):
    """
    Merged shared libs into a symlink tree mapping the library's SONAME to
    it's artifact.
    """
    return gen_shared_libs_action(
        actions = actions,
        out = out,
        shared_libs = shared_libs,
        gen_action = lambda actions, output, shared_libs: actions.symlinked_dir(
            output,
            {name: shlib.lib.output for name, shlib in shared_libs.items()},
        ),
        dir = True,
    )

def extract_soname_from_shlib(
        actions: AnalysisActions,
        name: str,
        shared_lib: Artifact) -> Artifact:
    """
    Extract the SONAME from a shared library into a file.
    """
    soname = actions.declare_output(name)
    cmd = cmd_args(
        "sh",
        "-c",
        '''set -euo pipefail; objdump -p "$1" | grep SONAME | awk '{print $2}' > "$2"''',
        "",
        shared_lib,
        soname.as_output(),
    )
    actions.run(cmd, category = "extract_soname", identifier = shared_lib.short_path)
    return soname
