# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

def project(f: Artifact):
    return f

NameSet = transitive_set(args_projections = {
    "project": project,
})

NameInfo = provider(fields = ["tset"])

def _test_impl(ctx):
    # Produce a file that contains our name.
    out = ctx.actions.write("out.txt", str(ctx.label.name) + "\n")

    # Produce a tset that is our file concated wiht all the files emitted by
    # our children.
    children = [d[NameInfo].tset for d in ctx.attrs.deps]
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
        DefaultInfo(default_output = agg),
    ]

test = rule(
    impl = _test_impl,
    attrs = {
        "deps": attrs.list(attrs.dep(providers = [NameInfo]), default = []),
    },
)

def _test_duplication_impl(ctx):
    out = ctx.actions.write("out.txt", "hello world")

    tset1 = ctx.actions.tset(NameSet, value = out)
    tset2 = ctx.actions.tset(NameSet, value = out)

    combined_tset = ctx.actions.tset(NameSet, children = [tset1, tset2])

    combined_tset_traversal = list(combined_tset.traverse())

    if len(combined_tset_traversal) != 2:
        fail(
            "Expected traversal to be deduplicated by TSet identity, not value identity! Traversal is: {}".format(
                combined_tset_traversal,
            ),
        )

    return [
        DefaultInfo(default_output = out),
    ]

test_duplication = rule(
    impl = _test_duplication_impl,
    attrs = {
        "deps": attrs.list(attrs.dep(providers = [NameInfo]), default = []),
    },
)
