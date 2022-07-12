load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

def _installer_impl(ctx: "context") -> ["provider"]:
    run_info = ctx.attrs.installer[RunInfo]
    expect(run_info != None, "installer tool executable must have a RunInfo!")
    return [DefaultInfo(), InstallInfo(installer = run_info, files = ctx.attrs.files)]

installer = rule(impl = _installer_impl, attrs = {
    "files": attr.dict(key = attr.string(), value = attr.source(), default = {}),
    "installer": attr.option(attr.dep(), default = None),
})
