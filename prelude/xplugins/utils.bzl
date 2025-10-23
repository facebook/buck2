# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:artifact_tset.bzl", "make_artifact_tset")
load("@prelude//cxx:cxx_library_utility.bzl", "cxx_attr_deps", "cxx_attr_exported_deps")
load(":types.bzl", "XPluginsSocketUsageInfo", "XPluginsUsageInfo")

def get_xplugins_usage_info(ctx: AnalysisContext) -> XPluginsUsageInfo | None:
    socket_manifests = []
    usage_infos = []
    for d in cxx_attr_deps(ctx):
        if XPluginsSocketUsageInfo in d:
            socket_manifests.append(d[XPluginsSocketUsageInfo].usage_info)
        if XPluginsUsageInfo in d:
            usage_infos.append(d[XPluginsUsageInfo])

    # Collect the XPluginsUsageInfo from exported_deps too
    for d in cxx_attr_exported_deps(ctx):
        if XPluginsUsageInfo in d:
            usage_infos.append(d[XPluginsUsageInfo])

    if socket_manifests or usage_infos:
        # Combine into a new tset
        tset = make_artifact_tset(
            actions = ctx.actions,
            label = ctx.label,
            artifacts = socket_manifests,
            children = [u.socket_info_tset for u in usage_infos],
        )
        return XPluginsUsageInfo(socket_info_tset = tset)
    else:
        return None
