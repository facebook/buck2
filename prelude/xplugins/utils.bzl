# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//cxx:cxx_library_utility.bzl", "cxx_attr_deps", "cxx_attr_exported_deps")
load("@prelude//cxx:link_groups_types.bzl", "LinkGroupInfo")
load(
    ":types.bzl",
    "XPluginsPluginUsageInfo",
    "XPluginsSocketUsageInfo",
    "XPluginsUsageInfo",
    "XPluginsUsageInfoSet",
)

def get_xplugins_usage_info(ctx: AnalysisContext) -> XPluginsUsageInfo | None:
    plugin_manifests = []
    socket_manifests = []
    usage_infos = []
    for d in cxx_attr_deps(ctx):
        if XPluginsPluginUsageInfo in d:
            plugin_manifests.append(d[XPluginsPluginUsageInfo])
        if XPluginsSocketUsageInfo in d:
            socket_manifests.append(d[XPluginsSocketUsageInfo])
        if XPluginsUsageInfo in d:
            usage_infos.append(d[XPluginsUsageInfo])

    # Collect the XPluginsUsageInfo from exported_deps too
    for d in cxx_attr_exported_deps(ctx):
        if XPluginsUsageInfo in d:
            usage_infos.append(d[XPluginsUsageInfo])

    if plugin_manifests or socket_manifests or usage_infos:
        plugin_info_tset = ctx.actions.tset(
            XPluginsUsageInfoSet,
            value = plugin_manifests,
            children = [u.plugin_info_tset for u in usage_infos],
        )
        socket_info_tset = ctx.actions.tset(
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

def get_xplugins_usage_subtargets(
        ctx: AnalysisContext,
        usage_info: XPluginsUsageInfo | None,
        link_group_info: LinkGroupInfo | None) -> dict[str, list[Provider]]:
    if not usage_info:
        return {}

    plugin_usage_argsfile = ctx.actions.declare_output("xplugins/{}_plugin_usage_info.argsfile".format(ctx.attrs.name))
    socket_usage_argsfile = ctx.actions.declare_output("xplugins/{}_socket_usage_info.argsfile".format(ctx.attrs.name))
    plugin_usage_manifests = usage_info.plugin_info_tset.project_as_args("artifacts")
    socket_usage_manifests = usage_info.socket_info_tset.project_as_args("artifacts")

    if link_group_info:
        ctx.actions.dynamic_output_new(
            _xplugins_manifest_rule(
                link_group = ctx.attrs.link_group or "",
                link_group_info = link_group_info,
                out = plugin_usage_argsfile.as_output(),
                usage_info_tset = usage_info.plugin_info_tset,
            ),
        )
        ctx.actions.dynamic_output_new(
            _xplugins_manifest_rule(
                link_group = ctx.attrs.link_group or "",
                link_group_info = link_group_info,
                out = socket_usage_argsfile.as_output(),
                usage_info_tset = usage_info.socket_info_tset,
            ),
        )

        unfiltered_socket_usage_argsfile = ctx.actions.write(
            "unfiltered_socket_usage_info.argsfile",
            socket_usage_manifests,
        )
        unfiltered_plugin_usage_argsfile = ctx.actions.write(
            "unfiltered_plugin_usage_info.argsfile",
            plugin_usage_manifests,
        )

    else:
        unfiltered_socket_usage_argsfile = socket_usage_argsfile
        unfiltered_plugin_usage_argsfile = plugin_usage_argsfile

        # Without link groups we can just project the artifacts to a file
        ctx.actions.write(
            plugin_usage_argsfile.as_output(),
            plugin_usage_manifests,
        )
        ctx.actions.write(
            socket_usage_argsfile.as_output(),
            socket_usage_manifests,
        )

    return {
        "xplugins-plugin-manifests": [
            DefaultInfo(default_output = plugin_usage_argsfile, other_outputs = [plugin_usage_manifests], sub_targets = {
                "unfiltered": [DefaultInfo(
                    default_output = unfiltered_plugin_usage_argsfile,
                    other_outputs = [plugin_usage_manifests],
                )],
            }),
        ],
        "xplugins-socket-manifests": [
            DefaultInfo(
                default_output = socket_usage_argsfile,
                other_outputs = [socket_usage_manifests],
                sub_targets = {
                    "unfiltered": [DefaultInfo(
                        default_output = unfiltered_socket_usage_argsfile,
                        other_outputs = [socket_usage_manifests],
                    )],
                },
            ),
        ],
    }
