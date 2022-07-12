load("@fbcode//buck2/prelude/java:java_toolchain.bzl", "JavaToolchainInfo")
load(":android_toolchain.bzl", "AndroidToolchainInfo")

def gen_aidl_impl(ctx: "context") -> ["provider"]:
    android_toolchain = ctx.attrs._android_toolchain[AndroidToolchainInfo]
    aidl_cmd = cmd_args(android_toolchain.aidl)
    aidl_cmd.add("-p", android_toolchain.framework_aidl_file)
    aidl_cmd.add("-I", ctx.attrs.import_path)

    # We need the `aidl_srcs` files - otherwise the search on the `import_path` won't find anything.
    aidl_cmd.hidden(ctx.attrs.aidl_srcs)

    aidl_out = ctx.actions.declare_output("aidl_output")
    aidl_cmd.add("-o", aidl_out.as_output())
    aidl_cmd.add(ctx.attrs.aidl)
    ctx.actions.run(aidl_cmd, category = "aidl")

    # Put the generated Java files into a zip file to be used as srcs to other rules.
    java_toolchain = ctx.attrs._java_toolchain[JavaToolchainInfo]
    jar_cmd = cmd_args(java_toolchain.jar)
    jar_cmd.add("-cfM")
    out = ctx.actions.declare_output("{}_aidl_java_output.src.zip".format(ctx.attrs.name))
    jar_cmd.add(out.as_output())
    jar_cmd.add(aidl_out)

    ctx.actions.run(jar_cmd, category = "aidl_jar")

    return [DefaultInfo(default_outputs = [out])]
