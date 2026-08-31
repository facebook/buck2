# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    "@prelude//:artifact_tset.bzl",
    "ArtifactInfo",
    "make_artifact_tset",
)
load(
    "@prelude//cxx:cxx_toolchain_types.bzl",
    "CxxToolchainInfo",
    "LinkerType",
)
load("@prelude//linking:execution_preference.bzl", "LinkExecutionPreference")
load(
    "@prelude//linking:link_info.bzl",
    "Archive",
    "ArchiveContentsType",
    "ArchiveLinkable",
    "DepMetadata",
    "LinkArgs",
    "LinkInfo",  # @unused Used as a type
    "ObjectsLinkable",
    "SharedLibLinkable",
)
load(
    ":link_types.bzl",
    "CxxLinkResultType",
    "LinkOptions",
    "link_options",
)

# The link recipe is encoded as one JSON string of pure scalars/counts plus two
# flat streams (flags and artifacts) that both sides walk in the same traversal
# order. Encoding each linkable as nested attr tuples instead costs the daemon
# ~1KiB of coerced-attr nodes per linkable, which dominates buck2 memory on
# large link-group graphs (measured ~1GiB retained on a 14-target fbcode dev
# cohort); the artifacts must remain real `attrs.source()` values for input
# tracking, but everything else can live in the string.
#
# The anon target identity therefore hashes (recipe, flags, artifacts), which
# discriminates exactly like the nested encoding, except that external debug
# info labels are no longer part of the identity: deserialization always
# discarded them, so they only served to split otherwise-identical identities.

_LINK_ARGS_FLAGS_TAG = 0
_LINK_ARGS_INFOS_TAG = 1

_LINKABLE_ARCHIVE_TAG = 0
_LINKABLE_OBJECTS_TAG = 1
_LINKABLE_SHARED_TAG = 2

def _encode_linkable(linkable, artifacts: list[Artifact]):
    if isinstance(linkable, ArchiveLinkable):
        artifacts.append(linkable.archive.artifact)
        artifacts.extend(linkable.archive.external_objects)
        return [
            _LINKABLE_ARCHIVE_TAG,
            len(linkable.archive.external_objects),
            linkable.archive.archive_contents_type.value,
            linkable.link_whole,
            linkable.linker_type.value,
            linkable.supports_lto,
        ]

    if isinstance(linkable, ObjectsLinkable):
        artifacts.extend(linkable.objects)
        return [
            _LINKABLE_OBJECTS_TAG,
            len(linkable.objects),
            linkable.link_whole,
            linkable.linker_type.value,
        ]

    if isinstance(linkable, SharedLibLinkable):
        artifacts.append(linkable.lib)
        return [
            _LINKABLE_SHARED_TAG,
            linkable.link_without_soname,
        ]

    fail('cannot serialize linkable "{}"'.format(str(linkable)))

def _encode_link_info(info: LinkInfo, flags: list, artifacts: list[Artifact]):
    external_debug_artifacts = []
    if info.external_debug_info._tset != None:
        external_debug_infos = []
        for tset_infos in info.external_debug_info._tset.traverse():
            external_debug_infos.extend(tset_infos)
        for artifact_info in dedupe(external_debug_infos):
            external_debug_artifacts.extend(artifact_info.artifacts)

    flags.extend(info.pre_flags)
    flags.extend(info.post_flags)
    linkables = [_encode_linkable(linkable, artifacts) for linkable in info.linkables]
    artifacts.extend(external_debug_artifacts)

    return [
        info.name,
        len(info.pre_flags),
        len(info.post_flags),
        linkables,
        len(external_debug_artifacts),
        [m.version for m in info.metadata],
    ]

def serialize_anon_attrs(output: str, result_type: CxxLinkResultType, opts: LinkOptions) -> dict[str, typing.Any]:
    recipe = []
    flags = []
    artifacts = []
    for link in opts.links:
        if link.flags != None:
            recipe.append([_LINK_ARGS_FLAGS_TAG, len(link.flags)])
            flags.extend(link.flags)
        elif link.infos != None:
            recipe.append([_LINK_ARGS_INFOS_TAG, [_encode_link_info(info, flags, artifacts) for info in link.infos]])
        else:
            fail("cannot serialize link args")

    return dict(
        links_recipe = json.encode(recipe),
        links_flags = flags,
        links_artifacts = artifacts,
        output = output,
        import_library = opts.import_library,
        link_execution_preference = opts.link_execution_preference.value,
        dwp_execution_preference = opts.dwp_execution_preference.value if opts.dwp_execution_preference != None else None,
        enable_distributed_thinlto = opts.enable_distributed_thinlto,
        identifier = opts.identifier,
        category_suffix = opts.category_suffix,
        result_type = result_type.value,
        allow_cache_upload = opts.allow_cache_upload,
    )

def _decode_linkable(spec, artifacts, artifact_cursor: list[int]) -> typing.Any:
    kind = spec[0]

    if kind == _LINKABLE_ARCHIVE_TAG:
        _, num_external_objects, archive_contents_type, link_whole, linker_type, supports_lto = spec
        archive_artifact = artifacts[artifact_cursor[0]]
        external_objects = artifacts[artifact_cursor[0] + 1 : artifact_cursor[0] + 1 + num_external_objects]
        artifact_cursor[0] += 1 + num_external_objects
        return ArchiveLinkable(
            archive = Archive(
                artifact = archive_artifact,
                external_objects = external_objects,
                archive_contents_type = ArchiveContentsType(archive_contents_type),
            ),
            link_whole = link_whole,
            linker_type = LinkerType(linker_type),
            supports_lto = supports_lto,
        )

    if kind == _LINKABLE_OBJECTS_TAG:
        _, num_objects, link_whole, linker_type = spec
        objects = artifacts[artifact_cursor[0] : artifact_cursor[0] + num_objects]
        artifact_cursor[0] += num_objects
        return ObjectsLinkable(
            objects = objects,
            link_whole = link_whole,
            linker_type = LinkerType(linker_type),
        )

    if kind == _LINKABLE_SHARED_TAG:
        _, link_without_soname = spec
        lib = artifacts[artifact_cursor[0]]
        artifact_cursor[0] += 1
        return SharedLibLinkable(
            lib = lib,
            link_without_soname = link_without_soname,
        )

    fail("Invalid linkable kind: {}".format(kind))

def _decode_link_info(actions: AnalysisActions, label: Label, spec, flags, flag_cursor: list[int], artifacts, artifact_cursor: list[int]) -> LinkInfo:
    name, num_pre_flags, num_post_flags, linkable_specs, num_external_debug_artifacts, metadata = spec

    pre_flags = flags[flag_cursor[0] : flag_cursor[0] + num_pre_flags]
    post_flags = flags[flag_cursor[0] + num_pre_flags : flag_cursor[0] + num_pre_flags + num_post_flags]
    flag_cursor[0] += num_pre_flags + num_post_flags

    linkables = [_decode_linkable(linkable_spec, artifacts, artifact_cursor) for linkable_spec in linkable_specs]

    external_debug_artifacts = artifacts[artifact_cursor[0] : artifact_cursor[0] + num_external_debug_artifacts]
    artifact_cursor[0] += num_external_debug_artifacts

    return LinkInfo(
        name = name,
        pre_flags = pre_flags,
        post_flags = post_flags,
        linkables = linkables,
        external_debug_info = make_artifact_tset(
            actions = actions,
            infos = [ArtifactInfo(label = label, artifacts = external_debug_artifacts, tags = [])] if external_debug_artifacts else [],
        ),
        metadata = [DepMetadata(version = v) for v in metadata],
    )

def deserialize_anon_attrs(actions: AnalysisActions, label: Label, attrs: struct) -> (str, CxxLinkResultType, LinkOptions):
    flag_cursor = [0]
    artifact_cursor = [0]
    links = []
    for entry in json.decode(attrs.links_recipe):
        if entry[0] == _LINK_ARGS_FLAGS_TAG:
            num_flags = entry[1]
            links.append(LinkArgs(flags = attrs.links_flags[flag_cursor[0] : flag_cursor[0] + num_flags]))
            flag_cursor[0] += num_flags
        elif entry[0] == _LINK_ARGS_INFOS_TAG:
            links.append(
                LinkArgs(
                    infos = [_decode_link_info(actions, label, spec, attrs.links_flags, flag_cursor, attrs.links_artifacts, artifact_cursor) for spec in entry[1]]
                )
            )
        else:
            fail("Invalid link args kind: {}".format(entry[0]))

    if flag_cursor[0] != len(attrs.links_flags) or artifact_cursor[0] != len(attrs.links_artifacts):
        fail(
            "anon link recipe did not consume its flag/artifact streams exactly (flags {}/{}, artifacts {}/{})".format(
                flag_cursor[0],
                len(attrs.links_flags),
                artifact_cursor[0],
                len(attrs.links_artifacts),
            )
        )

    opts = link_options(
        links = links,
        import_library = attrs.import_library,
        link_execution_preference = LinkExecutionPreference(attrs.link_execution_preference),
        dwp_execution_preference = LinkExecutionPreference(attrs.dwp_execution_preference) if attrs.dwp_execution_preference != None else None,
        category_suffix = attrs.category_suffix,
        identifier = attrs.identifier,
        enable_distributed_thinlto = attrs.enable_distributed_thinlto,
        allow_cache_upload = attrs.allow_cache_upload,
    )

    result_type = CxxLinkResultType(attrs.result_type)

    return (attrs.output, result_type, opts)

# The attributes -- and their serialized types -- that can be passed to an
# anonymous link. See the module docblock for the stream encoding contract.
ANON_ATTRS = {
    "allow_cache_upload": attrs.bool(),
    "category_suffix": attrs.string(),
    "dwp_execution_preference": attrs.option(attrs.enum(LinkExecutionPreference.values()), default = None),
    "enable_distributed_thinlto": attrs.bool(),
    "identifier": attrs.option(attrs.string(), default = None),
    "import_library": attrs.option(attrs.source(), default = None),
    "link_execution_preference": attrs.enum(LinkExecutionPreference.values()),
    "links_artifacts": attrs.list(attrs.source(), default = []),
    "links_flags": attrs.list(attrs.arg(), default = []),
    "links_recipe": attrs.string(default = "[]"),
    "output": attrs.string(),
    "result_type": attrs.enum(CxxLinkResultType.values()),
    "separate_debug_info": attrs.bool(default = False),
    "_cxx_toolchain": attrs.dep(providers = [CxxToolchainInfo]),
}
