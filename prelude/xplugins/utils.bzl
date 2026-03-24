# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:link_groups_types.bzl", "LinkGroupInfo")
load("@prelude//utils:arglike.bzl", "ArgLike")
load(
    ":types.bzl",
    "XPluginsPluginUsageInfo",
    "XPluginsSocketUsageInfo",
    "XPluginsUsageInfo",
    "XPluginsUsageInfoSet",
)

def get_xplugins_usage_info(actions: AnalysisActions, all_deps: list[Dependency]) -> XPluginsUsageInfo | None:
    plugin_manifests = [d[XPluginsPluginUsageInfo] for d in all_deps if XPluginsPluginUsageInfo in d]
    socket_manifests = [d[XPluginsSocketUsageInfo] for d in all_deps if XPluginsSocketUsageInfo in d]
    usage_infos = [d[XPluginsUsageInfo] for d in all_deps if XPluginsUsageInfo in d]

    if plugin_manifests or socket_manifests or usage_infos:
        plugin_info_tset = actions.tset(
            XPluginsUsageInfoSet,
            value = plugin_manifests,
            children = [u.plugin_info_tset for u in usage_infos],
        )
        socket_info_tset = actions.tset(
            XPluginsUsageInfoSet,
            value = socket_manifests,
            children = [u.socket_info_tset for u in usage_infos],
        )
        return XPluginsUsageInfo(
            plugin_info_tset = plugin_info_tset,
            socket_info_tset = socket_info_tset,
        )
    else:
        return None

def _filter_plugins_artifacts(
        actions: AnalysisActions,
        link_group: str,
        link_group_info: LinkGroupInfo,
        out: OutputArtifact,
        usage_info_tset: XPluginsUsageInfoSet) -> list[Provider]:
    mappings = link_group_info.mappings
    usage_infos = []
    for usage_info_list in usage_info_tset.traverse():
        for usage_info in usage_info_list:
            # We use empty string to represent not in a link group, which
            # generally means part of the main binary.
            dep_link_group = mappings.get(usage_info.target, "")
            if link_group == dep_link_group:
                usage_infos.append(usage_info.usage_info)

    filelist = actions.write(
        out,
        usage_infos,
        with_inputs = True,
    )

    return [DefaultInfo(filelist)]

_xplugins_manifest_rule = dynamic_actions(
    impl = _filter_plugins_artifacts,
    attrs = {
        "link_group": dynattrs.value(str),
        "link_group_info": dynattrs.value(LinkGroupInfo),
        "out": dynattrs.output(),
        "usage_info_tset": dynattrs.value(XPluginsUsageInfoSet),
    },
)

_ManifestInfo = record(
    argsfile = field(Artifact),
    unfiltered_argsfile = field(Artifact),
    manifests = field(ArgLike),
)

def _process_manifest(ctx, kind, info_tset, link_group_info):
    """Process a single manifest kind (plugin or socket), returning a _ManifestInfo record."""
    argsfile = ctx.actions.declare_output("xplugins/{}_{}_usage_info.argsfile".format(ctx.attrs.name, kind), has_content_based_path = False)
    manifests = info_tset.project_as_args("artifacts")

    if link_group_info:
        ctx.actions.dynamic_output_new(
            _xplugins_manifest_rule(
                link_group = ctx.attrs.link_group or "",
                link_group_info = link_group_info,
                out = argsfile.as_output(),
                usage_info_tset = info_tset,
            ),
        )
        unfiltered_argsfile = ctx.actions.write(
            "unfiltered_{}_usage_info.argsfile".format(kind),
            manifests,
        )
    else:
        unfiltered_argsfile = argsfile
        ctx.actions.write(
            argsfile.as_output(),
            manifests,
        )

    return _ManifestInfo(
        argsfile = argsfile,
        unfiltered_argsfile = unfiltered_argsfile,
        manifests = manifests,
    )

def _make_manifest_providers(info):
    """Build the provider list for a manifest subtarget."""
    return [
        DefaultInfo(
            default_output = info.argsfile,
            other_outputs = [info.manifests],
            sub_targets = {
                "unfiltered": [DefaultInfo(
                    default_output = info.unfiltered_argsfile,
                    other_outputs = [info.manifests],
                )],
            },
        ),
    ]

def get_xplugins_usage_subtargets(
        ctx: AnalysisContext,
        usage_info: XPluginsUsageInfo | None,
        link_group_info: LinkGroupInfo | None) -> dict[str, list[Provider]]:
    if not usage_info:
        return {}

    plugin = _process_manifest(ctx, "plugin", usage_info.plugin_info_tset, link_group_info)
    socket = _process_manifest(ctx, "socket", usage_info.socket_info_tset, link_group_info)

    return {
        "xplugins-plugin-manifests": _make_manifest_providers(plugin),
        "xplugins-socket-manifests": _make_manifest_providers(socket),
    }
