def _create_artifact(ctx: "context"):
    a = ctx.actions.write("path/test.txt", "")
    return (a,)

def _create_artifact_declared(ctx: "context"):
    a = ctx.actions.declare_output("path/test.txt")
    ctx.actions.write(a, "")
    return (a,)

def _create_artifact_as_output(ctx: "context"):
    a = ctx.actions.declare_output("path/test.txt")
    ctx.actions.write(a, "")
    return (a.as_output(),)

def _check_artifact(x):
    # Want an array
    [x] = x
    if not x.startswith("buck-out/") or not x.endswith("path/test.txt"):
        fail("Output is not as expected, got " + repr(x))

def _create_cmdargs_artifact(ctx: "context"):
    a = ctx.actions.write("magic/path/test.txt", "")
    return cmd_args(["a", cmd_args(a).parent()])

def _check_cmdargs_artifact(x):
    [a, b] = x
    b = b.replace("\\", "/")
    if a != "a" or not b.startswith("buck-out/") or not b.endswith("magic/path"):
        fail("Output is not as expected, got " + repr(x))

def _create_target(ctx: "context"):
    # We don't want to hardcode the label, as different roots may change it,
    # so instead check it matches the string representation
    return [ctx.label.raw_target(), str(ctx.label.raw_target())]

def _check_target(x):
    [a, b] = x
    if a != b:
        fail("Targets should match, got " + repr(x))

def _create_label(ctx: "context"):
    # We don't want to hardcode the label, as different roots may change it,
    # so instead check it matches the string representation
    return [ctx.label, str(ctx.label)]

def _check_label(x):
    [a, b] = x
    if a != b:
        fail("Labels should match, got " + repr(x))

def _create_enum_value(_ctx: "context"):
    typ = enum("foo")
    return [typ("foo"), "foo"]

def _check_enum_value(x):
    [a, b] = x
    if a != b:
        fail("Enum values should match, got " + repr(x))

TestProvider = provider(fields = ["foo"])

def _create_provider_value(ctx: "context"):
    a = ctx.actions.write("path/test.txt", "")
    return [TestProvider(foo = a), a]

def _check_provider_value(x):
    [prov, a] = x
    if prov["foo"] != a:
        fail("Provider values should match, got " + repr(x))

tests = [
    ("atom", lambda _: "test", "test"),
    ("simple", lambda _: [1], [1]),
    ("nested", lambda _: [42, {"test": True}], [42, {"test": True}]),
    ("record", lambda _: record(hello = "", bye = "")(hello = [1], bye = {}), {"bye": {}, "hello": [1]}),
    ("struct", lambda _: struct(hello = [1], bye = struct()), {"bye": {}, "hello": [1]}),
    ("artifact", _create_artifact, _check_artifact),
    ("artifact_declared", _create_artifact_declared, _check_artifact),
    ("artifact_output", _create_artifact_as_output, _check_artifact),
    ("target", _create_target, _check_target),
    ("label", _create_label, _check_label),
    ("cmdargs", lambda _: {"more": cmd_args(["a", "b", "c"], format = "1{}")}, {"more": ["1a", "1b", "1c"]}),
    ("cmdargs_single", lambda _: {"test": cmd_args("abc")}, {"test": ["abc"]}),
    ("cmdargs_concat", lambda _: {"test": cmd_args("abc", delimiter = "")}, {"test": "abc"}),
    ("cmdargs_artifact", _create_cmdargs_artifact, _check_cmdargs_artifact),
    ("enum", _create_enum_value, _check_enum_value),
    ("provider", _create_provider_value, _check_provider_value),
]

def _write_json_test_impl(ctx: "context") -> ["provider"]:
    want = ctx.label.name
    for name, input, output in tests:
        if name == want:
            input_file = ctx.actions.write_json("input", input(ctx))
            output_file = ctx.actions.declare_output("output")

            def f(ctx: "context", artifacts, outputs):
                contents = artifacts[input_file].read_json()
                if type(output) == "function":
                    output(contents)
                elif contents == output:
                    pass
                else:
                    fail("JSON divergence in " + name + ": Got " + repr(contents) + ", wanted " + repr(output))
                ctx.actions.write(outputs[output_file], "")

            ctx.actions.dynamic_output(dynamic = [input_file], inputs = [], outputs = [output_file], f = f)
            return [DefaultInfo(default_outputs = [output_file])]
    fail("Test named " + want + " not found")

write_json_test = rule(impl = _write_json_test_impl, attrs = {})

def _write_json_with_inputs_test(ctx: "context") -> ["provider"]:
    input = ctx.actions.write("input", ctx.attrs.content)
    as_json = ctx.actions.write_json("json", input, with_inputs = True)

    output = ctx.actions.declare_output("output")

    # as_json will contain a quoted-path and we want to cat the contents of that path. piping through xargs allows us to interpret it again as cli args (and so the quotes are removed).
    script = ctx.actions.write("script", cmd_args(["cat", as_json, "| xargs cat", ">", output], delimiter = " "))
    cmd = cmd_args("/bin/sh", script)
    cmd.hidden(as_json, output.as_output())
    ctx.actions.run(cmd, category = "cmd")

    marker = ctx.actions.declare_output("marker")

    # @lint-ignore BUCKRESTRICTEDSYNTAX
    def f(ctx: "context", artifacts, outputs):
        expected = artifacts[input].read_string()
        actual = artifacts[output].read_string()
        if expected != actual:
            fail("mismatched output. expected `{}`, actual `{}`".format(expected, actual))
        ctx.actions.write(outputs[marker], "")

    ctx.actions.dynamic_output(dynamic = [input, output], inputs = [], outputs = [marker], f = f)
    return [DefaultInfo(default_outputs = [marker])]

write_json_with_inputs_test = rule(impl = _write_json_with_inputs_test, attrs = {"content": attrs.string()})

def test():
    for name, _, _ in tests:
        write_json_test(name = name)

    inputs_content = native.read_config("write_json", "content")
    if inputs_content == None:
        fail("config value write_json.content required")
    write_json_with_inputs_test(
        name = "with_inputs",
        content = inputs_content,
    )
