load("@prelude//apple:apple_toolchain_types.bzl", "AppleToolchainInfo")
load(":apple_bundle_utility.bzl", "get_bundle_min_target_version", "get_bundle_resource_processing_options")
load(":apple_core_data_types.bzl", "AppleCoreDataSpec")
load(":apple_sdk.bzl", "get_apple_sdk_name")
load(":resource_groups.bzl", "create_resource_graph")

def apple_core_data_impl(ctx: "context") -> ["provider"]:
    spec = AppleCoreDataSpec(
        path = ctx.attrs.path,
    )
    graph = create_resource_graph(
        ctx = ctx,
        labels = ctx.attrs.labels,
        deps = [],
        exported_deps = [],
        core_data_spec = spec,
    )
    return [DefaultInfo(), graph]

def compile_apple_core_data(ctx: "context", specs: [AppleCoreDataSpec.type], product_name: str.type) -> ["artifact", None]:
    if len(specs) == 0:
        return None

    output = ctx.actions.declare_output("AppleCoreDataCompiled")

    # Aggregate all the coredata momc commands together
    momc_commands = []
    for spec in specs:
        momc_command = _get_momc_command(ctx, spec, product_name, cmd_args("$TMPDIR"))
        momc_commands.append(momc_command)

    # Sandboxing and fs isolation on RE machines results in Xcode tools failing
    # when those are working in freshly created directories in buck-out.
    # See https://fb.workplace.com/groups/1042353022615812/permalink/1872164996301273/
    # As a workaround create a directory in tmp, use it for Xcode tools, then
    # copy the result to buck-out.
    wrapper_script, hidden = ctx.actions.write(
        "momc_wrapper.sh",
        [
            cmd_args('export TMPDIR="$(mktemp -d)"'),
            cmd_args(momc_commands),
            cmd_args(output, format = 'mkdir -p {} && cp -r "$TMPDIR"/ {}'),
        ],
        allow_args = True,
    )
    combined_command = cmd_args(["/bin/sh", wrapper_script]).hidden(hidden + momc_commands + [output.as_output()])
    processing_options = get_bundle_resource_processing_options(ctx)
    ctx.actions.run(combined_command, prefer_local = processing_options.prefer_local, category = "apple_core_data")
    return output

def _get_momc_command(ctx: "context", core_data_spec: AppleCoreDataSpec.type, product_name: str.type, output_directory: "cmd_args") -> "cmd_args":
    return cmd_args([
        ctx.attrs._apple_toolchain[AppleToolchainInfo].momc,
        "--sdkroot",
        ctx.attrs._apple_toolchain[AppleToolchainInfo].sdk_path,
        "--" + get_apple_sdk_name(ctx) + "-deployment-target",
        get_bundle_min_target_version(ctx),
        "--module",
        product_name,
        core_data_spec.path,
        output_directory,
    ], delimiter = " ")
