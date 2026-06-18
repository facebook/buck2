# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def _producer_impl(ctx):
    out = ctx.actions.declare_output("producer.txt", has_content_based_path = False)
    ctx.actions.run(
        [
            "/bin/sh",
            "-c",
            "printf '%s' \"$1\" > \"$2\"",
            "--",
            ctx.attrs.content,
            out.as_output(),
        ],
        category = "produce",
        prefer_remote = ctx.attrs.prefer_remote,
    )
    return [DefaultInfo(default_output = out)]

producer = rule(
    impl = _producer_impl,
    attrs = {
        "content": attrs.string(default = "generated input for action rewind\n"),
        "prefer_remote": attrs.bool(default = False),
    },
)

def _consumer_impl(ctx):
    inp = ctx.attrs.dep[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output("consumer.txt", has_content_based_path = False)
    ctx.actions.run(
        [
            "/bin/sh",
            "-c",
            "printf 'consumer saw: ' > \"$2\"; cat \"$1\" >> \"$2\"",
            "--",
            inp,
            out.as_output(),
        ],
        category = "consume",
        local_only = ctx.attrs.local_only,
    )
    return [DefaultInfo(default_output = out)]

consumer = rule(
    impl = _consumer_impl,
    attrs = {
        "dep": attrs.dep(),
        "local_only": attrs.bool(default = False),
    },
)

def _consumer_two_impl(ctx):
    first = ctx.attrs.first[DefaultInfo].default_outputs[0]
    second = ctx.attrs.second[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output("consumer_two.txt", has_content_based_path = False)
    ctx.actions.run(
        [
            "/bin/sh",
            "-c",
            "printf 'multi consumer saw:\n' > \"$3\"; cat \"$1\" >> \"$3\"; cat \"$2\" >> \"$3\"",
            "--",
            first,
            second,
            out.as_output(),
        ],
        category = "consume_two",
    )
    return [DefaultInfo(default_output = out)]

consumer_two = rule(
    impl = _consumer_two_impl,
    attrs = {
        "first": attrs.dep(),
        "second": attrs.dep(),
    },
)

def _consumer_many_impl(ctx):
    inputs = [dep[DefaultInfo].default_outputs[0] for dep in ctx.attrs.deps]
    out = ctx.actions.declare_output("consumer_many.txt", has_content_based_path = False)
    ctx.actions.run(
        [
            "/bin/sh",
            "-c",
            (
                "out=\"$1\"; shift; printf 'many consumer saw:\n' > \"$out\"; "
                "for input in \"$@\"; do cat \"$input\" >> \"$out\"; done"
            ),
            "--",
            out.as_output(),
        ] + inputs,
        category = "consume_many",
    )
    return [DefaultInfo(default_output = out)]

consumer_many = rule(
    impl = _consumer_many_impl,
    attrs = {
        "deps": attrs.list(attrs.dep()),
    },
)

def many_input_targets(count):
    deps = []
    for i in range(count):
        name = "producer_many_{}".format(i)
        producer(
            name = name,
            content = "many generated input {} for action rewind\n".format(i),
        )
        deps.append(":{}".format(name))

    consumer_many(
        name = "consumer_many",
        deps = deps,
    )

def _tree_producer_impl(ctx):
    out = ctx.actions.declare_output("tree", dir = True, has_content_based_path = False)
    ctx.actions.run(
        [
            "/bin/sh",
            "-c",
            "mkdir -p \"$2\"; printf '%s' \"$1\" > \"$2/file.txt\"",
            "--",
            ctx.attrs.content,
            out.as_output(),
        ],
        category = "produce_tree",
    )
    return [DefaultInfo(default_outputs = [out.project("file.txt")])]

tree_producer = rule(
    impl = _tree_producer_impl,
    attrs = {
        "content": attrs.string(default = "tree generated input for action rewind\n"),
    },
)

def _tree_dir_producer_impl(ctx):
    out = ctx.actions.declare_output("tree_dir", dir = True, has_content_based_path = False)
    ctx.actions.run(
        [
            "/bin/sh",
            "-c",
            "mkdir -p \"$2\"; printf '%s' \"$1\" > \"$2/file.txt\"",
            "--",
            ctx.attrs.content,
            out.as_output(),
        ],
        category = "produce_tree_dir",
    )
    return [DefaultInfo(default_output = out)]

tree_dir_producer = rule(
    impl = _tree_dir_producer_impl,
    attrs = {
        "content": attrs.string(default = "tree generated input for action rewind\n"),
    },
)

def _tree_consumer_impl(ctx):
    inp = ctx.attrs.dep[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output("tree_consumer.txt", has_content_based_path = False)
    ctx.actions.run(
        [
            "/bin/sh",
            "-c",
            "printf 'tree consumer saw: ' > \"$2\"; cat \"$1\" >> \"$2\"",
            "--",
            inp,
            out.as_output(),
        ],
        category = "consume_tree",
    )
    return [DefaultInfo(default_output = out)]

tree_consumer = rule(
    impl = _tree_consumer_impl,
    attrs = {
        "dep": attrs.dep(),
    },
)

def _tree_dir_consumer_impl(ctx):
    inp = ctx.attrs.dep[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output("tree_dir_consumer.txt", has_content_based_path = False)
    ctx.actions.run(
        [
            "/bin/sh",
            "-c",
            "printf 'tree dir consumer saw: ' > \"$2\"; cat \"$1/file.txt\" >> \"$2\"",
            "--",
            inp,
            out.as_output(),
        ],
        category = "consume_tree_dir",
    )
    return [DefaultInfo(default_output = out)]

tree_dir_consumer = rule(
    impl = _tree_dir_consumer_impl,
    attrs = {
        "dep": attrs.dep(),
    },
)

def _nondeterministic_producer_impl(ctx):
    out = ctx.actions.declare_output(
        "nondeterministic.txt",
        has_content_based_path = False,
    )
    ctx.actions.run(
        [
            "/bin/sh",
            "-c",
            "cat /proc/sys/kernel/random/uuid > \"$1\" 2>/dev/null || date +%s%N > \"$1\"",
            "--",
            out.as_output(),
        ],
        category = "produce_nondeterministic",
    )
    return [DefaultInfo(default_output = out)]

nondeterministic_producer = rule(
    impl = _nondeterministic_producer_impl,
    attrs = {},
)

def _nondeterministic_middle_impl(ctx):
    inp = ctx.attrs.dep[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output(
        "nondeterministic_middle.txt",
        has_content_based_path = False,
    )
    command = "cat \"$1\" > \"$2\"; " + (
        "cat /proc/sys/kernel/random/uuid >> \"$2\" 2>/dev/null || " +
        "date +%s%N >> \"$2\""
    )
    ctx.actions.run(
        [
            "/bin/sh",
            "-c",
            command,
            "--",
            inp,
            out.as_output(),
        ],
        category = "middle_nondeterministic",
    )
    return [DefaultInfo(default_output = out)]

nondeterministic_middle = rule(
    impl = _nondeterministic_middle_impl,
    attrs = {
        "dep": attrs.dep(),
    },
)

def _prefixed_consumer_impl(ctx):
    inp = ctx.attrs.dep[DefaultInfo].default_outputs[0]
    out = ctx.actions.declare_output(ctx.attrs.out, has_content_based_path = False)
    ctx.actions.run(
        [
            "/bin/sh",
            "-c",
            "printf '%s' \"$1\" > \"$3\"; cat \"$2\" >> \"$3\"",
            "--",
            ctx.attrs.prefix,
            inp,
            out.as_output(),
        ],
        category = ctx.attrs.category,
    )
    return [DefaultInfo(default_output = out)]

prefixed_consumer = rule(
    impl = _prefixed_consumer_impl,
    attrs = {
        "category": attrs.string(default = "consume_prefixed"),
        "dep": attrs.dep(),
        "out": attrs.string(),
        "prefix": attrs.string(),
    },
)
