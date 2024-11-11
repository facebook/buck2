# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//linking:shared_libraries.bzl", "traverse_shared_library_info")
load("@prelude//utils:arglike.bzl", "ArgLike")
load(":compile.bzl", "PycInvalidationMode")
load(":interface.bzl", "PythonLibraryInterface", "PythonLibraryManifestsInterface")
load(":manifest.bzl", "ManifestInfo")

PythonLibraryManifests = record(
    label = field(Label),
    srcs = field([ManifestInfo, None]),
    src_types = field([ManifestInfo, None], None),
    default_resources = field([(ManifestInfo, list[ArgLike]), None]),
    standalone_resources = field([(ManifestInfo, list[ArgLike]), None]),
    bytecode = field([dict[PycInvalidationMode, ManifestInfo], None]),
    dep_manifest = field([ManifestInfo, None]),
    extensions = field([dict[str, typing.Any], None]),
)

def _bytecode_artifacts(invalidation_mode: PycInvalidationMode):
    return lambda value: [] if value.bytecode == None else (
        [a for a, _ in value.bytecode[invalidation_mode].artifacts]
    )

def _bytecode_manifests(invalidation_mode: PycInvalidationMode):
    return lambda value: [] if value.bytecode == None else (
        value.bytecode[invalidation_mode].manifest
    )

def _dep_manifests(value: PythonLibraryManifests):
    if value.dep_manifest == None:
        return []
    return cmd_args(value.dep_manifest.manifest, format = "--manifest={}")

def _dep_artifacts(value: PythonLibraryManifests):
    if value.dep_manifest == None:
        return []
    return value.dep_manifest.artifacts

def _hidden_resources(value: PythonLibraryManifests):
    if value.default_resources == None:
        return []
    return value.default_resources[1]

def _has_hidden_resources(children: list[bool], value: [PythonLibraryManifests, None]):
    if value:
        if value.default_resources and len(value.default_resources[1]) > 0:
            return True
    return any(children)

def _resource_manifests(value: PythonLibraryManifests):
    if value.default_resources == None:
        return []
    return value.default_resources[0].manifest

def _resource_artifacts(value: PythonLibraryManifests):
    if value.default_resources == None:
        return []
    return [a for a, _ in value.default_resources[0].artifacts]

def _standalone_hidden_resources(value: PythonLibraryManifests):
    if value.standalone_resources == None:
        return []
    return value.standalone_resources[1]

def _standalone_has_hidden_resources(children: list[bool], value: [PythonLibraryManifests, None]):
    if value:
        if value.standalone_resources and len(value.standalone_resources[1]) > 0:
            return True
    return any(children)

def _standalone_resource_manifests(value: PythonLibraryManifests):
    if value.standalone_resources == None:
        return []
    return value.standalone_resources[0].manifest

def _standalone_resource_artifacts(value: PythonLibraryManifests):
    if value.standalone_resources == None:
        return []
    return [a for a, _ in value.standalone_resources[0].artifacts]

def _source_manifests(value: PythonLibraryManifests):
    if value.srcs == None:
        return []
    return value.srcs.manifest

def _source_artifacts(value: PythonLibraryManifests):
    if value.srcs == None:
        return []
    return [a for a, _ in value.srcs.artifacts]

def _source_type_manifests(value: PythonLibraryManifests):
    if value.src_types == None:
        return []
    return value.src_types.manifest

def _source_type_manifest_jsons(value: PythonLibraryManifests):
    if value.src_types == None:
        return None
    return (value.label.raw_target(), value.src_types.manifest)

def _source_type_artifacts(value: PythonLibraryManifests):
    if value.src_types == None:
        return []
    return [a for a, _ in value.src_types.artifacts]

_BYTECODE_PROJ_PREFIX = {
    PycInvalidationMode("checked_hash"): "checked_bytecode",
    PycInvalidationMode("unchecked_hash"): "bytecode",
}

args_projections = {
    "dep_artifacts": _dep_artifacts,
    "dep_manifests": _dep_manifests,
    "hidden_resources": _hidden_resources,
    "resource_artifacts": _resource_artifacts,
    "resource_manifests": _resource_manifests,
    "source_artifacts": _source_artifacts,
    "source_manifests": _source_manifests,
    "source_type_artifacts": _source_type_artifacts,
    "source_type_manifests": _source_type_manifests,
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
# TODO(nmj): Resources in general, and mapping of resources to new paths too.
PythonLibraryInfo = provider(fields = {
    "extension_shared_libraries": provider_field(typing.Any, default = None),  # "SharedLibraryInfo"
    "manifests": provider_field(typing.Any, default = None),  # PythonLibraryManifestsTSet
    "shared_libraries": provider_field(typing.Any, default = None),  # "SharedLibraryInfo"
})

def info_to_interface(info: PythonLibraryInfo) -> PythonLibraryInterface:
    return PythonLibraryInterface(
        shared_libraries = lambda: traverse_shared_library_info(info.shared_libraries),
        extension_shared_libraries = lambda: traverse_shared_library_info(info.extension_shared_libraries),
        iter_manifests = lambda: info.manifests.traverse(),
        manifests = lambda: manifests_to_interface(info.manifests),
    )

def _get_resource_manifests(standalone: typing.Any, manifests: typing.Any) -> typing.Any:
    if standalone:
        return [manifests.project_as_args("standalone_resource_manifests")]
    else:
        return [manifests.project_as_args("resource_manifests")]

def _get_resource_artifacts(standalone: typing.Any, manifests: typing.Any) -> typing.Any:
    if standalone:
        return [manifests.project_as_args("standalone_resource_artifacts")]
    else:
        return [manifests.project_as_args("resource_artifacts")]

def _get_hidden_resources(standalone: typing.Any, manifests: typing.Any) -> typing.Any:
    if standalone:
        return [manifests.project_as_args("standalone_hidden_resources")]
    else:
        return [manifests.project_as_args("hidden_resources")]

def _get_resource_artifacts_with_path(standalone: typing.Any, manifests: typing.Any) -> typing.Any:
    if standalone:
        return [(a, p) for m in manifests.traverse() if m != None and m.standalone_resources != None for a, p in m.standalone_resources[0].artifacts]
    else:
        return [(a, p) for m in manifests.traverse() if m != None and m.default_resources != None for a, p in m.default_resources[0].artifacts]

def _get_has_hidden_resources(standalone: typing.Any, manifests: typing.Any) -> typing.Any:
    if standalone:
        return manifests.reduce("standalone_has_hidden_resources")
    else:
        return manifests.reduce("has_hidden_resources")

def manifests_to_interface(manifests: PythonLibraryManifestsTSet) -> PythonLibraryManifestsInterface:
    return PythonLibraryManifestsInterface(
        src_manifests = lambda: [manifests.project_as_args("source_manifests")],
        src_artifacts = lambda: [manifests.project_as_args("source_artifacts")],
        src_artifacts_with_paths = lambda: [(a, p) for m in manifests.traverse() if m != None and m.srcs != None for a, p in m.srcs.artifacts],
        src_type_manifests = lambda: [manifests.project_as_args("source_manifests")],
        src_type_artifacts = lambda: [manifests.project_as_args("source_artifacts")],
        src_type_artifacts_with_path = lambda: [(a, p) for m in manifests.traverse() if m != None and m.src_types != None for a, p in m.src_types.artifacts],
        bytecode_manifests = lambda mode: [manifests.project_as_args("{}_manifests".format(_BYTECODE_PROJ_PREFIX[mode]))],
        bytecode_artifacts = lambda mode: [manifests.project_as_args("{}_artifacts".format(_BYTECODE_PROJ_PREFIX[mode]))],
        bytecode_artifacts_with_paths = lambda mode: [(a, p) for m in manifests.traverse() if m != None and m.bytecode != None for a, p in m.bytecode[mode].artifacts],
        resource_manifests = lambda standalone: _get_resource_manifests(standalone, manifests),
        resource_artifacts = lambda standalone: _get_resource_artifacts(standalone, manifests),
        resource_artifacts_with_paths = lambda standalone: _get_resource_artifacts_with_path(standalone, manifests),
        has_hidden_resources = lambda standalone: _get_has_hidden_resources(standalone, manifests),
        hidden_resources = lambda standalone: _get_hidden_resources(standalone, manifests),
    )
