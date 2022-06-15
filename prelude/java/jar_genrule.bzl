load("@fbcode//buck2/prelude:genrule.bzl", "process_genrule")
load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "JavaToolchainInfo")
load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

def jar_genrule_impl(ctx: "context") -> ["provider"]:
    output_name = "{}.jar".format(ctx.label.name)
    providers = process_genrule(ctx, output_name, None)
    expect(
        len(providers) == 1,
        "expected exactly one provider of type DefaultInfo from {} ({})"
            .format(ctx.label.name, providers),
    )

    default_info = providers[0]  # DefaultInfo type
    outputs = default_info.default_outputs
    expect(
        len(outputs) == 1,
        "expected exactly one output from {} ({})"
            .format(ctx.label.name, outputs),
    )
    output_jar = outputs[0]

    java_toolchain = ctx.attr._java_toolchain[JavaToolchainInfo]
    java_cmd = cmd_args(java_toolchain.java[RunInfo])
    java_cmd.add("-jar", output_jar)

    providers.append(RunInfo(args = java_cmd))
    return providers
