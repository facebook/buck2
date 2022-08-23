# Basic test
def _basic(ctx: "context") -> ["provider"]:
    input = ctx.actions.write("input", str(7 * 6))
    output = ctx.actions.declare_output("output")

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    def f(ctx: "context", artifacts, outputs):
        src = artifacts[input].read_string()
        assert_eq(src, "42")
        ctx.actions.write(outputs[output], src)

    ctx.actions.dynamic_output(dynamic = [input], inputs = [], outputs = [output.as_output()], f = f)
    return [DefaultInfo(default_outputs = [output])]

# Produce two output files
def _two(ctx: "context") -> ["provider"]:
    input = ctx.actions.write("input", "test")
    output1 = ctx.actions.declare_output("output1")
    output2 = ctx.actions.declare_output("output2")

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    def f(ctx: "context", artifacts, outputs):
        src = artifacts[input].read_string()
        ctx.actions.write(outputs[output1], "output1_" + src)
        ctx.actions.write(outputs[output2], "output2_" + src)

    ctx.actions.dynamic_output(dynamic = [input], inputs = [], outputs = [output1, output2], f = f)
    sub_targets = {
        "output1": [DefaultInfo(default_outputs = [output1])],
        "output2": [DefaultInfo(default_outputs = [output2])],
    }
    return [DefaultInfo(
        sub_targets = sub_targets,
    )]

# Nested dynamic outputs
def _nested(ctx: "context") -> ["provider"]:
    input = ctx.actions.write("input", "test")
    symlinked_dir = ctx.actions.declare_output("output1_symlinked_dir")

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    def f(ctx: "context", artifacts, outputs):
        src = artifacts[input].read_string()
        output1 = ctx.actions.declare_output("output1")
        output2 = ctx.actions.declare_output("output2")
        ctx.actions.write(output1, "output1_" + src)
        ctx.actions.write(output2, "output2_" + src)
        symlink_tree = {
            "output1": output1,
            "output2": output2,
        }
        nested_output = ctx.actions.declare_output("nested_output")

        # @lint-ignore BUCKRESTRICTEDSYNTAX
        def f2(ctx: "context", artifacts, outputs):
            nested_src1 = artifacts[output1].read_string()
            nested_src2 = artifacts[output2].read_string()
            ctx.actions.write(outputs[nested_output], [nested_src1, nested_src2])

        ctx.actions.dynamic_output(dynamic = [output1, output2], inputs = [], outputs = [nested_output], f = f2)

        symlink_tree["nested_output"] = nested_output
        ctx.actions.symlinked_dir(outputs[symlinked_dir], symlink_tree)

    ctx.actions.dynamic_output(dynamic = [input], inputs = [], outputs = [symlinked_dir], f = f)
    return [DefaultInfo(default_outputs = [symlinked_dir])]

# Produce two output files, using a command
def _command(ctx: "context") -> ["provider"]:
    hello = ctx.actions.declare_output("hello.txt")
    write_hello = ctx.actions.write(
        "hello.py",
        [
            cmd_args(["with open(r'", hello, "', 'w') as f:"], delimiter = ""),
            "  f.write('Hello\\n')",
        ],
    )
    ctx.actions.run(cmd_args(["python3", write_hello]).hidden(hello.as_output()), category = "test_category")

    world = ctx.actions.declare_output("world")
    universe = ctx.actions.declare_output("universe")

    script = ctx.actions.write(
        "script.py",
        [
            "import sys",
            "with open(sys.argv[2], 'w') as f:",
            "  f.write(sys.argv[1] + ' world\\n')",
            "with open(sys.argv[3], 'w') as f:",
            "  f.write(sys.argv[1] + ' universe\\n')",
        ],
    )

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    def f(ctx: "context", artifacts, outputs):
        src = artifacts[hello].read_string().strip()
        assert_eq(src, "Hello")
        ctx.actions.run(
            cmd_args(["python3", script, src, outputs[world].as_output(), outputs[universe].as_output()]),
            category = "dynamic_test",
        )

    ctx.actions.dynamic_output(dynamic = [hello], inputs = [script], outputs = [world, universe], f = f)
    return [DefaultInfo(default_outputs = [world], other_outputs = [universe])]

# Create a fresh output inside the dynamic
def _create(ctx: "context") -> ["provider"]:
    input = ctx.actions.write("input", str(7 * 6))
    output = ctx.actions.declare_output("output")

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    def f(ctx: "context", artifacts, outputs):
        src = artifacts[input].read_string()
        new_file = ctx.actions.write("new_file", src)
        ctx.actions.copy_file(outputs[output], new_file)

    ctx.actions.dynamic_output(dynamic = [input], inputs = [], outputs = [output.as_output()], f = f)
    return [DefaultInfo(default_outputs = [output])]

# Create a fresh output inside the dynamic, which clashes
def _create_duplicate(ctx: "context") -> ["provider"]:
    input = ctx.actions.write("input", str(7 * 6))
    output = ctx.actions.declare_output("output")

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    def f(ctx: "context", artifacts, outputs):
        src = artifacts[input].read_string()

        # Deliberately reuse the names input/output
        new_output = ctx.actions.write("output", src)

        # We can't have two actions that do copy with "output" as the name
        # since then we get conflicting identifiers for category `copy`.
        # I.e. the two copy() actions below can't end "output" and outputs[output].
        # We could allow copy to take an explicit identifier, but this is a corner
        # case and I don't think its a good idea to reuse names heavily anyway.
        new_input = ctx.actions.copy_file("input", new_output)
        ctx.actions.copy_file(outputs[output], new_input)

    ctx.actions.dynamic_output(dynamic = [input], inputs = [], outputs = [output.as_output()], f = f)
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
    elif ctx.label.name == "nested":
        return _nested(ctx)
    else:
        fail("Unknown test: " + ctx.label.name)

dynamic_test = rule(impl = _impl, attrs = {})

def assert_eq(a, b):
    if a != b:
        fail("Expected equal, but got", a, b)

def _assert_output_value_impl(ctx: "context") -> ["provider"]:
    produced = ctx.attrs.dep[DefaultInfo].default_outputs[0]
    value = ctx.actions.write("value", ctx.attrs.value)
    output = ctx.actions.declare_output("output")
    run = ctx.actions.write(
        "run.py",
        [
            "import sys",
            "with open(sys.argv[1]) as f:",
            "  value_content = f.read()",
            "with open(sys.argv[2]) as f:",
            "  produced_content = f.read()",
            "if value_content != produced_content:",
            "  print('Content does not match! Expected:', value_content, 'Got:', produced_content)",
            "  sys.exit(1)",
            "with open(sys.argv[3], 'w') as f:",
            "  f.write('Success\\n')",
        ],
    )
    ctx.actions.run(cmd_args(["python3", run, value, produced, output.as_output()]), category = "test_category")
    return [DefaultInfo(default_outputs = [output])]

assert_output_value = rule(impl = _assert_output_value_impl, attrs = {
    "dep": attrs.dep(),
    "value": attrs.string(),
})

def _proto_genrule_impl(ctx):
    out_artifact = ctx.actions.declare_output(ctx.attrs.out)
    env_vars = {
        "OUT": cmd_args(out_artifact.as_output()),
    }
    ctx.actions.run(
        cmd_args(["python3", "-c", ctx.attrs.python]),
        env = env_vars,
        category = "genrule",
    )
    return [DefaultInfo(default_outputs = [out_artifact])]

proto_genrule = rule(
    impl = _proto_genrule_impl,
    attrs = {
        "out": attrs.string(),
        "python": attrs.option(attrs.arg(), default = None),
    },
)
