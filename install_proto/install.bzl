def _installer_impl(ctx: "context") -> ["provider"]:
    installer = ctx.attrs.installer
    return [DefaultInfo(), InstallInfo(installer = installer, files = ctx.attrs.files)]

installer = rule(impl = _installer_impl, attrs = {
    "files": attrs.dict(key = attrs.string(), value = attrs.source(), default = {}),
    "installer": attrs.label(),
})
