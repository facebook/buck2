def _test_impl(ctx: "context"):
    url = "https://interncache-all.fbcdn.net/manifold/buck_build_test/tree/buck2_test/http_archive/test.tgz"
    sha1 = "1a45666759704bf08fc670aa96118a0415c470fc"

    download = ctx.actions.declare_output("download")
    ctx.actions.download_file(download, url, sha1 = sha1, is_deferrable = True)

    output = ctx.actions.declare_output("output")
    ctx.actions.run(["cp", download, output.as_output()], category = "cp")

    return [
        DefaultInfo(default_outputs = [output]),
    ]

test = rule(
    impl = _test_impl,
    attrs = {
    },
)
