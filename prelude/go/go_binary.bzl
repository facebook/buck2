load("@prelude//utils:utils.bzl", "expect")
load(":compile.bzl", "compile", "get_filtered_srcs")
load(":link.bzl", "link")

def go_binary_impl(ctx: "context") -> ["provider"]:
    lib = compile(ctx, "main", get_filtered_srcs(ctx, ctx.attrs.srcs), deps = ctx.attrs.deps)
    bin = link(ctx, lib, deps = ctx.attrs.deps, link_mode = ctx.attrs.link_mode)

    hidden = []
    for resource in ctx.attrs.resources:
        if type(resource) == "artifact":
            hidden.append(resource)
        else:
            # Otherwise, this is a dependency, so extract the resource and other
            # resources from the `DefaultInfo` provider.
            info = resource[DefaultInfo]
            expect(
                len(info.default_outputs) == 1,
                "expected exactly one default output from {} ({})"
                    .format(resource, info.default_outputs),
            )
            [resource] = info.default_outputs
            other = info.other_outputs

            hidden.append(resource)
            hidden.extend(other)

    return [
        DefaultInfo(default_outputs = [bin], other_outputs = hidden),
        RunInfo(args = cmd_args(bin).hidden(hidden)),
    ]
