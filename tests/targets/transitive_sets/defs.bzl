def project(args: "cmd_args", f: "artifact"):
    args.add(f)

NameSet = transitive_set(args_projections = {
    "project": project,
})

NameInfo = provider(fields = ["tset"])

def _test_impl(ctx):
    # Produce a file that contains our name.
    out = ctx.actions.write("out.txt", str(ctx.label.name) + "\n")

    # Produce a tset that is our file concated wiht all the files emitted by
    # our children.
    children = [d[NameInfo].tset for d in ctx.attr.deps]
    tset = ctx.actions.tset(NameSet, value = out, children = children)

    # Concatenate all the files declared by the tset, into a single file
    # (agg.txt), which we'll return as our output.
    agg = ctx.actions.declare_output("agg.txt")
    ctx.actions.run([
        "sh",
        "-c",
        'out="$1" && shift && cat "$@" > "$out"',
        "--",
        agg.as_output(),
        tset.project_as_args("project"),
    ], category = "test")

    return [
        NameInfo(tset = tset),
        DefaultInfo(default_outputs = [agg]),
    ]

test = rule(
    impl = _test_impl,
    attrs = {
        "deps": attr.list(attr.dep(providers = [NameInfo]), default = []),
    },
)
