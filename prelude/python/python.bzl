# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//linking:link_info.bzl", "LinkedObject")
load("@prelude//linking:shared_libraries.bzl", "SharedLibraryInfo")
load("@prelude//utils:arglike.bzl", "ArgLike")
load(":compile.bzl", "PycInvalidationMode")
load(":interface.bzl", "PythonLibraryManifestsInterface")
load(":manifest.bzl", "ManifestInfo")

NativeDepsInfoTSet = transitive_set()

NativeDepsInfo = record(
    native_deps = field(dict[Label, Dependency], dict()),
)

PythonLibraryManifests = record(
    label = field(Label),
    srcs = field([ManifestInfo, None]),
    src_types = field([ManifestInfo, None], None),
    default_resources = field([(ManifestInfo, list[ArgLike]), None]),
    standalone_resources = field([(ManifestInfo, list[ArgLike]), None]),
    bytecode = field([dict[PycInvalidationMode, ManifestInfo], None]),
    extensions = field([dict[str, LinkedObject], None]),
)

def _bytecode_artifacts(invalidation_mode: PycInvalidationMode) -> typing.Callable[[PycInvalidationMode], list[ArgLike]]:
    return lambda value: [] if value.bytecode == None else (
        [a for a, _ in value.bytecode[invalidation_mode].artifacts]
    )

def _bytecode_manifests(invalidation_mode: PycInvalidationMode) -> typing.Callable[[PycInvalidationMode], list[None] | Artifact]:
    return lambda value: [] if value.bytecode == None else (
        value.bytecode[invalidation_mode].manifest
    )

def _hidden_resources(value: PythonLibraryManifests) -> list[ArgLike]:
    if value.default_resources == None:
        return []
    return value.default_resources[1]

def _has_hidden_resources(children: list[bool], value: [PythonLibraryManifests, None]) -> bool:
    if value:
        if value.default_resources and len(value.default_resources[1]) > 0:
            return True
    return any(children)

def _resource_manifests(value: PythonLibraryManifests) -> list[None] | Artifact:
    if value.default_resources == None:
        return []
    return value.default_resources[0].manifest

def _resource_artifacts(value: PythonLibraryManifests) -> list[ArgLike]:
    if value.default_resources == None:
        return []
    return [a for a, _ in value.default_resources[0].artifacts]

def _resource_artifacts_simple(value: PythonLibraryManifests) -> list[ArgLike]:
    if value.default_resources == None:
        return []
    return [cmd_args(a, p, delimiter = "::") for a, p in value.default_resources[0].artifacts]

def _standalone_hidden_resources(value: PythonLibraryManifests) -> list[ArgLike]:
    if value.standalone_resources == None:
        return []
    return value.standalone_resources[1]

def _standalone_has_hidden_resources(children: list[bool], value: [PythonLibraryManifests, None]) -> bool:
    if value:
        if value.standalone_resources and len(value.standalone_resources[1]) > 0:
            return True
    return any(children)

def _standalone_resource_manifests(value: PythonLibraryManifests) -> list[None] | Artifact:
    if value.standalone_resources == None:
        return []
    return value.standalone_resources[0].manifest

def _standalone_resource_artifacts(value: PythonLibraryManifests) -> list[ArgLike]:
    if value.standalone_resources == None:
        return []
    return [a for a, _ in value.standalone_resources[0].artifacts]

def _source_manifests(value: PythonLibraryManifests) -> list[None] | Artifact:
    if value.srcs == None:
        return []
    return value.srcs.manifest

def _source_artifacts(value: PythonLibraryManifests) -> list[ArgLike]:
    if value.srcs == None:
        return []
    return [a for a, _ in value.srcs.artifacts]

def _sources_simple(value: PythonLibraryManifests) -> list[ArgLike]:
    if value.srcs == None:
        return []
    return [cmd_args(a, p, delimiter = "::") for a, p in value.srcs.artifacts]

def _source_type_manifests(value: PythonLibraryManifests) -> list[None] | Artifact:
    if value.src_types == None:
        return []
    return value.src_types.manifest

def _source_type_manifest_jsons(value: PythonLibraryManifests) -> (TargetLabel, Artifact) | None:
    if value.src_types == None:
        return None
    return (value.label.raw_target(), value.src_types.manifest)

def _source_type_artifacts(value: PythonLibraryManifests) -> list[ArgLike]:
    if value.src_types == None:
        return []
    return [a for a, _ in value.src_types.artifacts]

_BYTECODE_PROJ_PREFIX = {
    PycInvalidationMode("checked_hash"): "checked_bytecode",
    PycInvalidationMode("unchecked_hash"): "bytecode",
}

args_projections = {
    "hidden_resources": _hidden_resources,
    "resource_artifacts": _resource_artifacts,
    "resource_artifacts_simple": _resource_artifacts_simple,
    "resource_manifests": _resource_manifests,
    "source_artifacts": _source_artifacts,
    "source_manifests": _source_manifests,
    "source_type_artifacts": _source_type_artifacts,
    "source_type_manifests": _source_type_manifests,
    "sources_simple": _sources_simple,
    "standalone_hidden_resources": _standalone_hidden_resources,
    "standalone_resource_artifacts": _standalone_resource_artifacts,
    "standalone_resource_manifests": _standalone_resource_manifests,
}
args_projections.update({
    "{}_artifacts".format(prefix): _bytecode_artifacts(mode)
    for mode, prefix in _BYTECODE_PROJ_PREFIX.items()
})
args_projections.update({
    "{}_manifests".format(prefix): _bytecode_manifests(mode)
    for mode, prefix in _BYTECODE_PROJ_PREFIX.items()
})

PythonLibraryManifestsTSet = transitive_set(
    args_projections = args_projections,
    json_projections = {
        "source_type_manifests_json": _source_type_manifest_jsons,
    },
    reductions = {
        "has_hidden_resources": _has_hidden_resources,
        "standalone_has_hidden_resources": _standalone_has_hidden_resources,
    },
)

# Information about a python library and its dependencies.
PythonLibraryInfo = provider(fields = {
    # Shared libraries coming from cxx_python_extension targets
    "extension_shared_libraries": provider_field(SharedLibraryInfo),
    "is_native_dep": provider_field(bool),
    # See the docs for PythonLibraryManifestsInterface
    "manifests": provider_field(PythonLibraryManifestsTSet),
    # Native deps
    "native_deps": provider_field(NativeDepsInfoTSet),
    # Shared libraries coming from python_library and others
    "shared_libraries": provider_field(SharedLibraryInfo),
})

def _get_resource_manifests(standalone: bool, manifests: PythonLibraryManifestsTSet) -> list[ArgLike]:
    if standalone:
        return [manifests.project_as_args("standalone_resource_manifests")]
    else:
        return [manifests.project_as_args("resource_manifests")]

def _get_resource_artifacts(standalone: bool, manifests: PythonLibraryManifestsTSet) -> list[ArgLike]:
    if standalone:
        return [manifests.project_as_args("standalone_resource_artifacts")]
    else:
        return [manifests.project_as_args("resource_artifacts")]

def _get_hidden_resources(standalone: bool, manifests: PythonLibraryManifestsTSet) -> list[ArgLike]:
    if standalone:
        return [manifests.project_as_args("standalone_hidden_resources")]
    else:
        return [manifests.project_as_args("hidden_resources")]

def _get_resource_artifacts_with_path(standalone: bool, manifests: PythonLibraryManifestsTSet) -> list[(ArgLike, ArgLike)]:
    if standalone:
        return [(a, p) for m in manifests.traverse() if m != None and m.standalone_resources != None for a, p in m.standalone_resources[0].artifacts]
    else:
        return [(a, p) for m in manifests.traverse() if m != None and m.default_resources != None for a, p in m.default_resources[0].artifacts]

def _get_has_hidden_resources(standalone: bool, manifests: PythonLibraryManifestsTSet) -> bool:
    if standalone:
        return manifests.reduce("standalone_has_hidden_resources")
    else:
        return manifests.reduce("has_hidden_resources")

def manifests_to_interface(manifests: PythonLibraryManifestsTSet) -> PythonLibraryManifestsInterface:
    return PythonLibraryManifestsInterface(
        sources_simple = lambda: manifests.project_as_args("sources_simple"),
        src_manifests = lambda: [manifests.project_as_args("source_manifests")],
        src_artifacts = lambda: [manifests.project_as_args("source_artifacts")],
        src_artifacts_with_paths = lambda: [(a, p) for m in manifests.traverse() if m != None and m.srcs != None for a, p in m.srcs.artifacts],
        src_type_manifests = lambda: [manifests.project_as_args("source_manifests")],
        src_type_artifacts = lambda: [manifests.project_as_args("source_artifacts")],
        src_type_artifacts_with_path = lambda: [(a, p) for m in manifests.traverse() if m != None and m.src_types != None for a, p in m.src_types.artifacts],
        bytecode_manifests = lambda mode: [manifests.project_as_args("{}_manifests".format(_BYTECODE_PROJ_PREFIX[mode]))],
        bytecode_artifacts = lambda mode: [manifests.project_as_args("{}_artifacts".format(_BYTECODE_PROJ_PREFIX[mode]))],
        bytecode_artifacts_with_paths = lambda mode: [(a, p) for m in manifests.traverse() if m != None and m.bytecode != None for a, p in m.bytecode[mode].artifacts],
        resource_manifests = lambda standalone = False: _get_resource_manifests(standalone, manifests),
        resource_artifacts = lambda standalone = False: _get_resource_artifacts(standalone, manifests),
        resource_artifacts_with_paths = lambda standalone = False: _get_resource_artifacts_with_path(standalone, manifests),
        resource_artifacts_simple = lambda: manifests.project_as_args("resource_artifacts_simple"),
        has_hidden_resources = lambda standalone = False: _get_has_hidden_resources(standalone, manifests),
        hidden_resources = lambda standalone = False: _get_hidden_resources(standalone, manifests),
    )
