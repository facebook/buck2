# Basic test
def _basic(ctx: "context") -> ["provider"]:
    input = ctx.actions.write("input", str(7 * 6))
    output = ctx.actions.declare_output("output")

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    def f(ctx: "context"):
        src = ctx.artifacts[input].read_string()
        assert_eq(src, "42")
        ctx.actions.write(ctx.outputs[output], src)

    ctx.actions.dynamic_output([input], [], [output.as_output()], f)
    return [DefaultInfo(default_outputs = [output])]

# Produce two output files
def _two(ctx: "context") -> ["provider"]:
    input = ctx.actions.write("input", "test")
    output1 = ctx.actions.declare_output("output1")
    output2 = ctx.actions.declare_output("output2")

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    def f(ctx: "context"):
        src = ctx.artifacts[input].read_string()
        ctx.actions.write(ctx.outputs[output1], "output1_" + src)
        ctx.actions.write(ctx.outputs[output2], "output2_" + src)

    ctx.actions.dynamic_output([input], [], [output1, output2], f)
    sub_targets = {
        "output1": [DefaultInfo(default_outputs = [output1])],
        "output2": [DefaultInfo(default_outputs = [output2])],
    }
    return [DefaultInfo(
        sub_targets = sub_targets,
    )]

# Produce two output files, using a command
def _command(ctx: "context") -> ["provider"]:
    hello = ctx.actions.declare_output("hello.txt")
    write_hello = ctx.actions.write(
        "hello.sh",
        [
            "#!/usr/bin/env bash",
            cmd_args(["echo Hello >", hello], delimiter = " "),
        ],
        is_executable = True,
    )
    ctx.actions.run(cmd_args(write_hello).hidden(hello.as_output()), category = "test_category")

    world = ctx.actions.declare_output("world")
    universe = ctx.actions.declare_output("universe")

    script = ctx.actions.write(
        "script.sh",
        [
            "#!/usr/bin/env bash",
            cmd_args(["echo $1 world >", world], delimiter = " "),
            cmd_args(["echo $1 universe >", universe], delimiter = " "),
        ],
        is_executable = True,
    )

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    def f(ctx: "context"):
        src = ctx.artifacts[hello].read_string().strip()
        assert_eq(src, "Hello")
        ctx.actions.run(
            cmd_args([script, src])
                .hidden(ctx.outputs[world].as_output(), ctx.outputs[universe].as_output()),
            category = "dynamic_test",
        )

    ctx.actions.dynamic_output([hello], [script], [world, universe], f)
    return [DefaultInfo(default_outputs = [world], other_outputs = [universe])]

# Create a fresh output inside the dynamic
def _create(ctx: "context") -> ["provider"]:
    input = ctx.actions.write("input", str(7 * 6))
    output = ctx.actions.declare_output("output")

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    def f(ctx: "context"):
        src = ctx.artifacts[input].read_string()
        new_file = ctx.actions.write("new_file", src)
        ctx.actions.copy(new_file, ctx.outputs[output])

    ctx.actions.dynamic_output([input], [], [output.as_output()], f)
    return [DefaultInfo(default_outputs = [output])]

# Create a fresh output inside the dynamic, which clashes
def _create_duplicate(ctx: "context") -> ["provider"]:
    input = ctx.actions.write("input", str(7 * 6))
    output = ctx.actions.declare_output("output")

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    def f(ctx: "context"):
        src = ctx.artifacts[input].read_string()

        # Deliberately reuse the names input/output
        new_output = ctx.actions.write("output", src)

        # We can't have two actions that do copy with "output" as the name
        # since then we get conflicting identifiers for category `copy`.
        # I.e. the two copy() actions below can't end "output" and ctx.outputs[output].
        # We could allow copy to take an explicit identifier, but this is a corner
        # case and I don't think its a good idea to reuse names heavily anyway.
        new_input = ctx.actions.copy(new_output, "input")
        ctx.actions.copy(new_input, ctx.outputs[output])

    ctx.actions.dynamic_output([input], [], [output.as_output()], f)
    return [DefaultInfo(default_outputs = [output])]

def _impl(ctx: "context") -> ["provider"]:
    if ctx.label.name == "basic":
        return _basic(ctx)
    elif ctx.label.name == "two":
        return _two(ctx)
    elif ctx.label.name == "command":
        return _command(ctx)
    elif ctx.label.name == "create":
        return _create(ctx)
    elif ctx.label.name == "create_duplicate":
        return _create_duplicate(ctx)
    else:
        fail("Unknown test: " + ctx.label.name)

dynamic_test = rule(implementation = _impl, attrs = {})

def assert_eq(a, b):
    if a != b:
        fail("Expected equal, but got", a, b)

def _assert_output_value_impl(ctx: "context") -> ["provider"]:
    produced = ctx.attr.dep[DefaultInfo].default_outputs[0]
    value = ctx.actions.write("value", ctx.attr.value)
    output = ctx.actions.declare_output("output")
    run = ctx.actions.write(
        "run.sh",
        [
            "#!/bin/sh",
            "set -e",
            cmd_args(["diff", value, produced], delimiter = " "),
            "echo Success > \\",
            output,
        ],
    )
    ctx.actions.run(cmd_args(run).hidden([produced, value, output.as_output()]), category = "test_category")
    return [DefaultInfo(default_outputs = [output])]

assert_output_value = rule(implementation = _assert_output_value_impl, attrs = {
    "dep": attr.dep(),
    "value": attr.string(),
})
