load("@fbcode//buck2/prelude:http_file.bzl", "http_file_shared")
load("@fbcode//buck2/prelude/utils:utils.bzl", "value_or")

# Implementation of the `remote_file` build rule.
def remote_file_impl(ctx: "context") -> ["provider"]:
    return http_file_shared(
        ctx.actions,
        name = value_or(ctx.attrs.out, ctx.label.name),
        url = ctx.attrs.url,
        is_executable = ctx.attrs.type == "executable",
        is_exploded_zip = ctx.attrs.type == "exploded_zip",
        unzip_tool = ctx.attrs._unzip_tool[RunInfo],
        sha1 = ctx.attrs.sha1,
        sha256 = ctx.attrs.sha256,
    )
