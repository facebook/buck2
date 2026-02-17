# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(
    ":types.bzl",
    "XPluginsDebugArtifactsEntry",
    "XPluginsDebugArtifactsInfo",
    "XPluginsDebugArtifactsTSet",
    "XPluginsManifestInfo",
)

def xplugins_get_debug_artifacts_info(
        ctx: AnalysisContext,
        deps: list[Dependency]) -> XPluginsDebugArtifactsInfo | None:
    children = []
    for dep in deps:
        info = dep.get(XPluginsDebugArtifactsInfo)
        if info:
            children.append(info.tset)
    value = None

    # We don't expose this on apple_bundle because it should be propagated from the binary.
    manifest_dep = getattr(ctx.attrs, "xplugins_debug_artifacts", None)
    manifest_info = manifest_dep.get(XPluginsManifestInfo) if manifest_dep else None
    if manifest_info:
        value = XPluginsDebugArtifactsEntry(
            target = ctx.label,
            manifest_info = manifest_info,
        )
    if value or children:
        tset = ctx.actions.tset(
            XPluginsDebugArtifactsTSet,
            value = value,
            children = children,
        )
        return XPluginsDebugArtifactsInfo(tset = tset)
    return None

def xplugins_get_debug_artifacts_subtargets(actions: AnalysisActions, info: XPluginsDebugArtifactsInfo | None) -> list[Provider]:
    if not info:
        info = XPluginsDebugArtifactsInfo(tset = actions.tset(XPluginsDebugArtifactsTSet))

    dir_contents = {}
    manifest = {}

    name_counts = {}
    for entry in info.tset.traverse():
        if entry:
            raw_target = str(entry.target.raw_target())
            name = entry.target.name
            count = name_counts.get(name, 0)
            name_counts[name] = count + 1
            filename = "{}.json".format(name) if count == 0 else "{}{}.json".format(name, count)
            dir_contents[filename] = entry.manifest_info.function_mapping
            if raw_target not in manifest:
                manifest[raw_target] = []
            manifest[raw_target].append({
                "configured_target": entry.target,
                "path": filename,
            })

    manifest_file = actions.write_json("manifest.json", manifest, pretty = True)
    dir_contents["MANIFEST.json"] = manifest_file

    directory = actions.copied_dir(
        "XPluginsFunctionMappings",
        dir_contents,
    )

    return [
        DefaultInfo(
            sub_targets = {
                "function_mappings": [DefaultInfo(default_output = directory)],
            },
        ),
    ]
