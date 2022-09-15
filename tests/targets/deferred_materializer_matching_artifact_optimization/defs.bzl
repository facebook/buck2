def _copy_impl(ctx):
    out = ctx.actions.declare_output("action_output")
    ctx.actions.run(
        cmd_args(["cp", ctx.attrs.src, out.as_output()]).hidden(ctx.attrs.hidden),
        category = "cp",
    )

    return [DefaultInfo(default_outputs = [out])]

copy = rule(
    impl = _copy_impl,
    attrs = {
        "hidden": attrs.source(),
        "src": attrs.source(),
    },
)

def _download(ctx: "context"):
    url = "https://interncache-all.fbcdn.net/manifold/buck_build_test/tree/buck2_test/http_archive/test.tgz"
    sha1 = "1a45666759704bf08fc670aa96118a0415c470fc"

    download = ctx.actions.declare_output("download")
    ctx.actions.download_file(download, url, sha1 = sha1, is_deferrable = True)

    return [
        DefaultInfo(default_outputs = [download]),
    ]

download = rule(
    impl = _download,
    attrs = {
    },
)
