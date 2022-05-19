load(
    ":manifest.bzl",
    "ManifestInfo",  # @unused Used as a type
)
load(":python.bzl", "PythonLibraryInfo", "PythonLibraryManifestsTSet")
load(":toolchain.bzl", "PythonToolchainInfo")

def create_source_db_deps(
        ctx: "context",
        srcs: [ManifestInfo.type, None],
        python_deps: ["PythonLibraryInfo"]) -> DefaultInfo.type:
    output = ctx.actions.declare_output("db.json")
    artifacts = []

    python_toolchain = ctx.attr._python_toolchain[PythonToolchainInfo]
    cmd = cmd_args(python_toolchain.make_source_db[RunInfo])
    cmd.add(cmd_args(output.as_output(), format = "--output={}"))

    # Pass manifests for rule's sources.
    if srcs != None:
        cmd.add(cmd_args(srcs.manifest, format = "--sources={}"))
        artifacts.extend(srcs.artifacts)

    # Pass manifests for transitive deps.
    dep_manifests = ctx.actions.tset(PythonLibraryManifestsTSet, children = [d.manifests for d in python_deps])

    cmd.add(cmd_args(dep_manifests.project_as_args("source_type_manifests"), format = "--dependency={}"))
    artifacts.append(dep_manifests.project_as_args("source_type_artifacts"))

    ctx.actions.run(cmd, category = "py_source_db")

    return DefaultInfo(default_outputs = [output], other_outputs = artifacts)

def create_source_db(
        ctx: "context",
        srcs: [ManifestInfo.type, None],
        deps: ["dependency"]) -> DefaultInfo.type:
    python_deps = filter(None, [d[PythonLibraryInfo] for d in deps])
    return create_source_db_deps(ctx, srcs, python_deps)
