load(
    ":manifest.bzl",
    "ManifestInfo",  # @unused Used as a type
)
load(":toolchain.bzl", "PythonToolchainInfo")

def compile_manifests(
        ctx: "context",
        manifests: [ManifestInfo.type],
        ignore_errors: bool.type = False) -> "artifact":
    output = ctx.actions.declare_output("bytecode")
    cmd = cmd_args(ctx.attrs._python_toolchain[PythonToolchainInfo].host_interpreter)
    cmd.add(ctx.attrs._python_toolchain[PythonToolchainInfo].compile)
    cmd.add(cmd_args(output.as_output(), format = "--output={}"))
    if ignore_errors:
        cmd.add("--ignore-errors")
    for manifest in manifests:
        cmd.add(manifest.manifest)
        cmd.hidden(manifest.artifacts)
    ctx.actions.run(
        cmd,
        # On some platforms (e.g. linux), python hash code randomness can cause
        # the bytecode to be non-deterministic, so pin via the `PYTHONHASHSEED`
        # env var.
        env = {"PYTHONHASHSEED": "7"},
        category = "py_compile",
    )
    return output
