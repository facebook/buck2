load("@prelude//utils:argfile.bzl", "at_argfile")

def _worker_impl(ctx: AnalysisContext) -> list[Provider]:
    return [
        DefaultInfo(),
        WorkerInfo(
            exe = ctx.attrs.worker[RunInfo].args,
            concurrency = None,
            remote = True,
        ),
    ]

worker = rule(
    impl = _worker_impl,
    attrs = {
        "worker": attrs.dep(providers = [RunInfo]),
    },
)

def _demo_impl(ctx: AnalysisContext) -> list[Provider]:
    output = ctx.actions.declare_output(ctx.label.name)
    worker_exe = ctx.attrs._worker[WorkerInfo].exe
    argfile = at_argfile(
        actions = ctx.actions,
        name = "demo." + ctx.label.name + ".args",
        args = cmd_args(output.as_output()),
    )
    ctx.actions.run(
        cmd_args(worker_exe, argfile),
        category = "demo",
        exe = WorkerRunInfo(worker = ctx.attrs._worker[WorkerInfo]),
    )
    return [DefaultInfo(default_output = output)]

demo = rule(
    impl = _demo_impl,
    attrs = {
        "_worker": attrs.exec_dep(
            default = "//:worker",
            providers = [WorkerInfo],
        ),
    },
)
