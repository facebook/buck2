load("@fbcode//buck2/prelude/android:android_providers.bzl", "AndroidManifestInfo", "merge_android_packageable_info")
load("@fbcode//buck2/prelude/android:android_toolchain.bzl", "AndroidToolchainInfo")
load("@fbcode//buck2/prelude/android:configuration.bzl", "get_deps_by_platform")

ROOT_APKMODULE_NAME = "dex"

def android_manifest_impl(ctx: "context") -> ["provider"]:
    output, merge_report = generate_android_manifest(
        ctx,
        ctx.attr._android_toolchain[AndroidToolchainInfo].generate_manifest[RunInfo],
        ctx.attr.skeleton,
        ROOT_APKMODULE_NAME,
        _get_manifests_from_deps(ctx),
        {},
    )

    return [
        AndroidManifestInfo(manifest = output, merge_report = merge_report),
        DefaultInfo(default_outputs = [output], other_outputs = [merge_report]),
    ]

def generate_android_manifest(
        ctx: "context",
        generate_manifest: RunInfo.type,
        manifest_skeleton: "artifact",
        module_name: str.type,
        manifests_tset: ["ManifestTSet", None],
        placeholder_entries: "dict") -> ("artifact", "artifact"):
    generate_manifest_cmd = cmd_args(generate_manifest)
    generate_manifest_cmd.add([
        "--skeleton-manifest",
        manifest_skeleton,
        "--module-name",
        module_name,
    ])

    manifests = manifests_tset.project_as_args("artifacts") if manifests_tset else []
    library_manifest_paths_file = ctx.actions.write("library_manifest_paths_file", manifests)

    generate_manifest_cmd.add(["--library-manifests-list", library_manifest_paths_file])
    generate_manifest_cmd.hidden(manifests)

    placeholder_entries_args = cmd_args()
    for key, val in placeholder_entries:
        placeholder_entries_args.add(cmd_args(key, val, delimiter = " "))
    placeholder_entries_file = ctx.actions.write("placeholder_entries_file", placeholder_entries_args)

    generate_manifest_cmd.add(["--placeholder-entries-list", placeholder_entries_file])

    output = ctx.actions.declare_output("AndroidManifest.xml")
    merge_report = ctx.actions.declare_output("merge-report.txt")
    generate_manifest_cmd.add([
        "--output",
        output.as_output(),
        "--merge-report",
        merge_report.as_output(),
    ])

    ctx.actions.run(generate_manifest_cmd, category = "generate_manifest")

    return (output, merge_report)

def _get_manifests_from_deps(ctx: "context") -> ["ManifestTSet", None]:
    if len(ctx.attr.deps) == 0:
        return None

    deps_by_platform = get_deps_by_platform(ctx)
    primary_platform = deps_by_platform.keys()[0]
    deps = deps_by_platform[primary_platform]

    android_packageable_info = merge_android_packageable_info(ctx.actions, deps)
    return android_packageable_info.manifests
