# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//:paths.bzl", "paths")
load("@prelude//cxx:cxx_toolchain_types.bzl", "CxxToolchainInfo")
load(
    "@prelude//linking:link_info.bzl",
    "LibOutputStyle",
    "LinkInfo",
    "LinkInfos",
    "MergedLinkInfo",
    "ObjectsLinkable",
)
load(
    "@prelude//linking:linkable_graph.bzl",
    "DlopenableLibraryInfo",
    "LinkableRootInfo",
)
load(
    "@prelude//linking:linkables.bzl",
    "LinkableProviders",  # @unused Used as type
    "linkable",
)
load("@prelude//linking:strip.bzl", "strip_debug_info")

# Info required to link cxx_python_extensions into native python binaries
CxxExtensionLinkInfo = provider(
    fields = {
        "set": provider_field(typing.Any, default = None),  # "CxxExtensionTSet"
    },
)

CxxExtensionLinkInfoMember = record(
    linkable_providers = field([LinkableProviders, None], None),
    artifacts = field([dict[str, Artifact], None], None),
    python_module_names = field([dict[str, str], None], None),
    unembeddable_extensions = field([dict[str, LinkableProviders], None], None),
    dlopen_deps = field([dict[Label, LinkableProviders], None], None),
    shared_only_libs = field([dict[Label, LinkableProviders], None], None),
)
CxxExtensionLinkInfoReduced = record(
    linkable_providers = field(list[LinkableProviders], []),
    artifacts = field(dict[str, Artifact], {}),
    python_module_names = field(dict[str, str], {}),
    unembeddable_extensions = field(dict[str, LinkableProviders], {}),
    dlopen_deps = field(list[LinkableProviders], []),
    shared_only_libs = field(list[LinkableProviders], []),
)

def _cxx_extension_info_python_module_names(info: CxxExtensionLinkInfoMember):
    return cmd_args(
        [
            "{}:{}".format(k, v)
            for k, v in (info.python_module_names or {}).items()
        ],
        format = "--extension={}",
    )

CxxExtensionTSet = transitive_set(
    args_projections = {
        "python_module_names": _cxx_extension_info_python_module_names,
    },
)

def merge_cxx_extension_info(
        actions: AnalysisActions,
        deps: list[Dependency],
        linkable_providers: [LinkableProviders, None] = None,
        artifacts: dict[str, typing.Any] = {},
        python_module_names: dict[str, str] = {},
        unembeddable_extensions: dict[str, LinkableProviders] = {},
        shared_deps: list[Dependency] = []) -> CxxExtensionLinkInfo:
    dlopen_deps = {}
    shared_only_libs = {}
    for dep in shared_deps:
        # Libs that should be linked into their own, standalone link groups
        if DlopenableLibraryInfo in dep:
            dlopen_deps[dep.label] = linkable(dep)
            continue

        # Try to detect prebuilt, shared-only libraries.
        # TODO(agallagher): We need a more general way to support this, which
        # should *just* use `preferred_linkage` (and so it supports non-prebuilt
        # libs too), but this will require hoisting the rules first-order deps
        # up the tree as `dlopen_deps` so that we link them properly.
        if MergedLinkInfo in dep and LinkableRootInfo not in dep:
            shared_only_libs[dep.label] = linkable(dep)

    children = []
    for dep in deps:
        info = dep.get(CxxExtensionLinkInfo)
        if info != None:
            children.append(info.set)

    return CxxExtensionLinkInfo(
        set = actions.tset(
            CxxExtensionTSet,
            value = CxxExtensionLinkInfoMember(
                linkable_providers = linkable_providers or None,
                artifacts = artifacts or None,
                python_module_names = python_module_names or None,
                unembeddable_extensions = unembeddable_extensions or None,
                dlopen_deps = dlopen_deps or None,
                shared_only_libs = shared_only_libs or None,
            ),
            children = children,
        ),
    )

def reduce_cxx_extension_info(link_info: CxxExtensionLinkInfo) -> CxxExtensionLinkInfoReduced:
    linkable_providers = []
    artifacts = {}
    python_module_names = {}
    unembeddable_extensions = {}
    dlopen_deps = {}
    shared_only_libs = {}

    for info in link_info.set.traverse():
        if info.linkable_providers:
            linkable_providers.append(info.linkable_providers)
        if info.artifacts:
            artifacts.update(info.artifacts)
        if info.python_module_names:
            python_module_names.update(info.python_module_names)
        if info.unembeddable_extensions:
            unembeddable_extensions.update(info.unembeddable_extensions)
        if info.dlopen_deps:
            dlopen_deps.update(info.dlopen_deps)
        if info.shared_only_libs:
            shared_only_libs.update(info.shared_only_libs)

    return CxxExtensionLinkInfoReduced(
        linkable_providers = linkable_providers,
        artifacts = artifacts,
        python_module_names = python_module_names,
        unembeddable_extensions = unembeddable_extensions,
        dlopen_deps = dlopen_deps.values(),
        shared_only_libs = shared_only_libs.values(),
    )

def rewrite_static_symbols(
        ctx: AnalysisContext,
        suffix: str,
        pic_objects: list[Artifact],
        non_pic_objects: list[Artifact],
        libraries: dict[LibOutputStyle, LinkInfos],
        cxx_toolchain: CxxToolchainInfo,
        suffix_all: bool = False) -> dict[LibOutputStyle, LinkInfos]:
    symbols_file = _write_syms_file(
        ctx = ctx,
        name = ctx.label.name + "_rename_syms",
        objects = non_pic_objects,
        suffix = suffix,
        cxx_toolchain = cxx_toolchain,
        suffix_all = suffix_all,
    )
    static_objects, stripped_static_objects = suffix_symbols(ctx, suffix, non_pic_objects, symbols_file, cxx_toolchain)

    symbols_file_pic = _write_syms_file(
        ctx = ctx,
        name = ctx.label.name + "_rename_syms_pic",
        objects = pic_objects,
        suffix = suffix,
        cxx_toolchain = cxx_toolchain,
        suffix_all = suffix_all,
    )
    static_pic_objects, stripped_static_pic_objects = suffix_symbols(ctx, suffix, pic_objects, symbols_file_pic, cxx_toolchain)

    static_info = libraries[LibOutputStyle("archive")].default
    updated_static_info = LinkInfo(
        name = static_info.name,
        pre_flags = static_info.pre_flags,
        post_flags = static_info.post_flags,
        linkables = [static_objects],
        external_debug_info = static_info.external_debug_info,
        metadata = static_info.metadata,
    )
    updated_stripped_static_info = None
    static_info = libraries[LibOutputStyle("archive")].stripped
    if static_info != None:
        updated_stripped_static_info = LinkInfo(
            name = static_info.name,
            pre_flags = static_info.pre_flags,
            post_flags = static_info.post_flags,
            linkables = [stripped_static_objects],
            metadata = static_info.metadata,
        )

    static_pic_info = libraries[LibOutputStyle("pic_archive")].default
    updated_static_pic_info = LinkInfo(
        name = static_pic_info.name,
        pre_flags = static_pic_info.pre_flags,
        post_flags = static_pic_info.post_flags,
        linkables = [static_pic_objects],
        external_debug_info = static_pic_info.external_debug_info,
        metadata = static_pic_info.metadata,
    )
    updated_stripped_static_pic_info = None
    static_pic_info = libraries[LibOutputStyle("pic_archive")].stripped
    if static_pic_info != None:
        updated_stripped_static_pic_info = LinkInfo(
            name = static_pic_info.name,
            pre_flags = static_pic_info.pre_flags,
            post_flags = static_pic_info.post_flags,
            linkables = [stripped_static_pic_objects],
            metadata = static_pic_info.metadata,
        )
    updated_libraries = {
        LibOutputStyle("archive"): LinkInfos(default = updated_static_info, stripped = updated_stripped_static_info),
        LibOutputStyle("pic_archive"): LinkInfos(default = updated_static_pic_info, stripped = updated_stripped_static_pic_info),
    }
    return updated_libraries

def _write_syms_file(
        ctx: AnalysisContext,
        name: str,
        objects: list[Artifact],
        suffix: str,
        cxx_toolchain: CxxToolchainInfo,
        suffix_all: bool = False) -> Artifact:
    """
    Take a list of objects and append a suffix to all  defined symbols.
    """
    nm = cxx_toolchain.binary_utilities_info.nm
    symbols_file = ctx.actions.declare_output(name)

    objects_argsfile = ctx.actions.write(name + ".py_objects_argsfile", objects)
    objects_args = cmd_args(objects_argsfile, hidden = objects)

    script_env = {
        "NM": nm,
        "OBJECTS": objects_args,
        "SYMSFILE": symbols_file.as_output(),
    }

    # Compile symbols defined by all object files into a de-duplicated list of symbols to rename
    # --no-sort tells nm not to sort the output because we are sorting it to dedupe anyway
    # --defined-only prints only the symbols defined by this extension this way we won't rename symbols defined externally e.g. PyList_GetItem, etc...
    # --extern-only if suffix_all is not set, we are only interested in the externally visible PyInit symbols
    # -j print only the symbol name
    # sed removes filenames generated from objcopy (lines ending with ":") and empty lines
    # sort -u sorts the combined list of symbols and removes any duplicate entries
    # using awk we format the symbol names 'PyInit_hello' followed by the symbol name with the suffix appended to create the input file for objcopy
    # objcopy uses a list of symbol name followed by updated name e.g. 'PyInit_hello PyInit_hello_package_module'
    script = (
        "set -euo pipefail; " +  # fail if any command in the script fails
        '"$NM" --no-sort --defined-only -j {}@"$OBJECTS" | sed "/:$/d;/^$/d"'
    ).format("--extern-only " if not suffix_all else "")

    if not suffix_all:
        script += ' | grep "^PyInit_"'

    # Don't suffix asan symbols, as they shouldn't conflict, and suffixing
    # prevents deduplicating all the module constructors, which can be really
    # expensive to run.  We need to keep ODR guards but remove all sanitizer
    # globals. This removes:
    # __asan_*, ___asan_*, __tsan_*, ___tsan_*, __sanitizer_*, ___sanitizer_*,
    # asan.module_ctor, asan.module_dtor, tsan.module_ctor, tsan.module_dtor
    script += " | grep -v \"\\(\\(^_\\?__\\(\\(a\\|t\\)san\\|\\(sanitizer\\)\\)_\\)\\|\\(^\\(a\\|t\\)san.module_\\(c\\|d\\)tor\\)\\)\""

    script += (
        ' | awk \'{{print $1" "$1"_{suffix}"}}\' | sort -u > '.format(suffix = suffix) +
        '"$SYMSFILE";'
    )

    ctx.actions.run(
        [
            "/usr/bin/env",
            "bash",
            "-c",
            script,
        ],
        env = script_env,
        category = "write_syms_file",
        identifier = "{}_write_syms_file".format(symbols_file.basename),
    )

    return symbols_file

def suffix_symbols(
        ctx: AnalysisContext,
        suffix: str,
        objects: list[Artifact],
        symbols_file: Artifact,
        cxx_toolchain: CxxToolchainInfo) -> (ObjectsLinkable, ObjectsLinkable):
    """
    Take a list of objects and append a suffix to all  defined symbols.
    """
    objcopy = cxx_toolchain.binary_utilities_info.objcopy

    artifacts = []
    stripped_artifacts = []
    for obj in objects:
        base, name = paths.split_extension(obj.short_path)
        updated_name = "_".join([base, suffix, name])
        artifact = ctx.actions.declare_output(updated_name)

        script_env = {
            "OBJCOPY": objcopy,
            "ORIGINAL": obj,
            "OUT": artifact.as_output(),
            "SYMSFILE": symbols_file,
        }

        script = (
            "set -euo pipefail; " +  # fail if any command in the script fails
            '"$OBJCOPY" --redefine-syms="$SYMSFILE" "$ORIGINAL" "$OUT"'  # using objcopy we pass in the symbols file to re-write the original symbol name to the now suffixed version
        )

        # Usage: objcopy [option(s)] in-file [out-file]
        ctx.actions.run(
            [
                "/usr/bin/env",
                "bash",
                "-c",
                script,
            ],
            env = script_env,
            category = "suffix_symbols",
            identifier = updated_name,
        )

        artifacts.append(artifact)
        updated_base, _ = paths.split_extension(artifact.short_path)
        stripped_artifacts.append(strip_debug_info(ctx, updated_base + ".stripped.o", artifact))

    default = ObjectsLinkable(
        objects = artifacts,
        linker_type = cxx_toolchain.linker_info.type,
    )
    stripped = ObjectsLinkable(
        objects = stripped_artifacts,
        linker_type = cxx_toolchain.linker_info.type,
    )
    return default, stripped
