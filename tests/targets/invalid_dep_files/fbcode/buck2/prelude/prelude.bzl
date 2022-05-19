def _test_impl(ctx):
    tag = ctx.actions.artifact_tag()

    dep_file = ctx.actions.declare_output("depfile")
    app = ctx.actions.declare_output("app")

    seed = ctx.actions.write("seed", ctx.attr.seed)

    ctx.actions.run(
        [
            "sh",
            "-c",
            'echo "../invalid" > "$1" && touch "$2" && echo "$3"',
            "--",
            tag.tag_artifacts(dep_file.as_output()),
            app.as_output(),
            seed,
        ],
        category = "test",
        dep_files = {"deps": tag},
    )

    return [
        DefaultInfo(
            default_outputs = [app],
            sub_targets = {"dep_file": [DefaultInfo(default_outputs = [dep_file])]},
        ),
    ]

test = rule(
    attrs = {
        "seed": attr.string(default = ""),
    },
    implementation = _test_impl,
)
