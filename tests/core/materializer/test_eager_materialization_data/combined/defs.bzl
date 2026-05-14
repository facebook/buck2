# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def project(f: Artifact):
    return f

ArtifactSet = transitive_set(args_projections = {"project": project})

ArtifactInfo = provider(fields = ["tset"])

# Regular single-file producer.
def _regular_impl(ctx):
    out = ctx.actions.declare_output("regular_out", has_content_based_path = False)
    ctx.actions.run(
        cmd_args([
            "fbpython",
            "-c",
            "import sys; open(sys.argv[1], 'w').write('regular\\n')",
            out.as_output(),
        ]),
        category = "produce_regular",
        prefer_remote = True,
    )
    return [DefaultInfo(default_output = out)]

# Directory producer with projected sub-paths.
def _make_dir_impl(ctx):
    out_dir = ctx.actions.declare_output("out_dir", dir = True, has_content_based_path = False)
    ctx.actions.run(
        cmd_args([
            "fbpython",
            "-c",
            "import sys, os; d = sys.argv[1]; os.makedirs(d, exist_ok=True); open(os.path.join(d, 'file_a'), 'w').write('a\\n'); open(os.path.join(d, 'file_b'), 'w').write('b\\n')",
            out_dir.as_output(),
        ]),
        category = "mkdir",
        prefer_remote = True,
    )
    return [
        DefaultInfo(
            default_output = out_dir,
            sub_targets = {
                "file_a": [DefaultInfo(default_output = out_dir.project("file_a"))],
                "file_b": [DefaultInfo(default_output = out_dir.project("file_b"))],
            },
        )
    ]

# Transitive set node: file output + a tset combining its file with its
# children's tsets.
def _node_impl(ctx):
    out = ctx.actions.declare_output(
        "{}_out".format(ctx.label.name),
        has_content_based_path = False,
    )
    ctx.actions.run(
        cmd_args([
            "fbpython",
            "-c",
            "import sys; open(sys.argv[2], 'w').write(sys.argv[1] + '\\n')",
            ctx.label.name,
            out.as_output(),
        ]),
        category = "produce_node",
        prefer_remote = True,
    )
    children = [d[ArtifactInfo].tset for d in ctx.attrs.deps]
    tset = ctx.actions.tset(ArtifactSet, value = out, children = children)
    return [
        ArtifactInfo(tset = tset),
        DefaultInfo(default_output = out),
    ]

# Consumer that takes ONE input of each shape:
#   - `regular_dep`: regular Artifact
#   - `projected_dep`: projected sub-path artifact
#   - `tset_dep`: transitive set projection
def _consumer_combined_impl(ctx):
    regular_src = ctx.attrs.regular_dep[DefaultInfo].default_outputs[0]
    projected_src = ctx.attrs.projected_dep[DefaultInfo].default_outputs[0]
    tset = ctx.attrs.tset_dep[ArtifactInfo].tset

    out = ctx.actions.declare_output("consumer_out", has_content_based_path = False)
    ctx.actions.run(
        cmd_args([
            "fbpython",
            "-c",
            "import sys; out = open(sys.argv[1], 'w'); [out.write(open(f).read()) for f in sys.argv[2:]]",
            out.as_output(),
            regular_src,
            projected_src,
            tset.project_as_args("project"),
        ]),
        category = "consume",
        prefer_local = True,
        eager_materialization_enabled = True,
    )
    return [DefaultInfo(default_output = out)]

regular = rule(impl = _regular_impl, attrs = {})
make_dir = rule(impl = _make_dir_impl, attrs = {})
node = rule(
    impl = _node_impl,
    attrs = {
        "deps": attrs.list(attrs.dep(providers = [ArtifactInfo]), default = []),
    },
)
consumer_combined = rule(
    impl = _consumer_combined_impl,
    attrs = {
        "projected_dep": attrs.dep(),
        "regular_dep": attrs.dep(),
        "tset_dep": attrs.dep(providers = [ArtifactInfo]),
    },
)

# Content-based path producer for testing configuration → content hash bridging
def _content_based_impl(ctx):
    # Creates an artifact with content-based path
    # The path will be buck-out/v2/gen/content-hash-<digest>/...
    # But registration will use buck-out/v2/gen/config-hash/...
    out = ctx.actions.declare_output("content_out", has_content_based_path = True)
    ctx.actions.run(
        cmd_args([
            "fbpython",
            "-c",
            "import sys; open(sys.argv[1], 'w').write('content\\n')",
            out.as_output(),
        ]),
        category = "produce_content",
        prefer_remote = True,
    )
    return [DefaultInfo(default_output = out)]

content_based = rule(impl = _content_based_impl, attrs = {})

# Consumer that takes a content-based path input
def _consumer_content_impl(ctx):
    content_src = ctx.attrs.content_dep[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output("consumer_content_out", has_content_based_path = False)
    ctx.actions.run(
        cmd_args([
            "fbpython",
            "-c",
            "import sys; open(sys.argv[1], 'w').write(open(sys.argv[2]).read())",
            out.as_output(),
            content_src,
        ]),
        category = "consume_content",
        prefer_local = True,
        eager_materialization_enabled = True,
    )
    return [DefaultInfo(default_output = out)]

consumer_content = rule(
    impl = _consumer_content_impl,
    attrs = {
        "content_dep": attrs.dep(),
    },
)
