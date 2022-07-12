load("@fbcode//buck2/platform/execution:fat_platform.bzl", "fat_platform_transition")

def _fat_binary_impl(ctx):
    args = [ctx.attr.main]
    pprint(ctx.attr.bin)
    for (key, binary) in ctx.attr.bin.items():
        args += ["--{}".format(key), cmd_args(binary[RunInfo])]
    return [
        DefaultInfo(),
        RunInfo(args = args),
    ]

fat_binary = rule(
    impl = _fat_binary_impl,
    attrs = {
        "bin": attr.split_transition_dep(cfg = fat_platform_transition),
        "main": attr.source(),
    },
)

def _test_fat_binary_impl(ctx):
    remote = ctx.actions.declare_output("remote")
    local = ctx.actions.declare_output("local")

    ctx.actions.run([ctx.attr.fat_bin[RunInfo], remote.as_output()], category = "fat_test_remote")
    ctx.actions.run([ctx.attr.fat_bin[RunInfo], local.as_output()], local_only = True, category = "fat_test_local")

    combined = ctx.actions.declare_output("combined")
    ctx.actions.run([ctx.attr.cat_main, combined.as_output(), remote, local], category = "fat_test_combine")

    return [DefaultInfo(default_outputs = [combined])]

test_fat_binary = rule(
    impl = _test_fat_binary_impl,
    attrs = {
        "cat_main": attr.source(),
        "fat_bin": attr.exec_dep(providers = [RunInfo]),
    },
)
