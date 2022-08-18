load("@fbcode//buck2/prelude/utils:utils.bzl", "expect")

def _installer_impl(ctx: "context") -> ["provider"]:
    run_info = ctx.attrs.installer[RunInfo]
    expect(run_info != None, "installer tool executable must have a RunInfo!")
    return [DefaultInfo(), InstallInfo(installer = run_info, files = ctx.attrs.files)]

installer = rule(impl = _installer_impl, attrs = {
    "files": attrs.dict(key = attrs.string(), value = attrs.source(), default = {}),
    "installer": attrs.option(attrs.dep(), default = None),
})
