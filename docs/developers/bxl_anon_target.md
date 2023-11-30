---
id: anon_targets
title: BXL and Anonymous Targets
---

## Anonymous targets

[Anonymous targets](../rule_authors/anon_targets.md) are supported in BXL.
Anonymous targets are keyed by the attributes, and allow you to share/cache work
more effectively.

You might want to use anonymous targets if there is some heavy Starlark
evaluation which can be cached, or if you want to cache local actions.

**Note**: The context object within the anon target rule is **not** a BXL
context, but a normal rule analysis context.

### APIs

The `actions` object returned from `ctx.bxl_actions().actions` (equivalent of
`ctx.actions` in normal rules) has the following functions for anonymous
targets:

- `anon_target(rule: "rule", attrs: Dict[str, Any]) -> "promise"`: generates a
  single anonymous target. Return type is an unresolved `promise`.
- `anon_targets(rules: [("rule", Dict[str, Any])]) -> "promise"`: generates a
  list of anonymous targets. Return type is an unresolved `promise` representing
  the list of anonymous targets.
- `artifact_promise(promise: "promise") -> "promise_artifact"`: turns an
  unresolved promise into a kind of artifact. See
  [Convert promise to artifact](../rule_authors/anon_targets.md#convert-promise-to-artifact)
  for more info on why you might want to use this.

The resulting promise also has `map()` and `join()` functions. `map()` applies a
function to the promise's results, and `join()` turns multiple promises into a
single promise.

To resolve promises in BXL, `bxl_ctx` has a `resolve()` function, which takes in
the analysis actions instance (`actions` object returned from
`ctx.bxl_actions().actions`) and a single promise and returns an optional
promise value, if there is one. If you intend to create multiple promises, using
`join()` to produce a single promise will allow you to resolve them concurently
with a single `resolve()` call.

Small example:

```python
def _my_impl(ctx):
    bxl_actions = ctx.bxl_actions() # pass in relevant params to configure the execution platform resolution
    actions = bxl_actions.actions

    promise1 = actions.anon_target(my_anon_rule1, my_attrs1).promise
    promise2 = actions.anon_target(my_anon_rule2, my_attrs2).promise.map(my_map_function)

    joined = promise1.join(promise2)

    resolved = ctx.resolve(actions, joined)

    # do some more stuff ...
```

### Complete Example

```python
## anon_bxl_rules.bzl ############

# Define an anonymous rule.

MirrorInfo = provider(fields = ["mirrored_attrs"])

# Anonymous rule which writes some silly output, and also mirrors all attributes received
def _mirror_impl(ctx: "context") -> ["provider"]:
    out = ctx.actions.declare_output("my_output")
    ctx.actions.write(out, "my_content")
    return [DefaultInfo(default_outputs = [out]), MirrorInfo(mirrored_attrs = ctx.attrs)]

my_mirror_rule = rule(impl = _mirror_impl, attrs = {
    "false": attrs.bool(),
    "int": attrs.int(),
    "list_string": attrs.list(attrs.string()),
    "string": attrs.string(),
    "true": attrs.bool(),
})

# Will be used in a map function in my_script.bxl below
StringInfo = provider(fields = ["my_string"])

## my_script.bxl ############

load(":anon_bxl_rules.bzl", "MirrorInfo", "StringInfo", "my_mirror_rule")

def _anon_target_example(ctx):
    bxl_actions = ctx.bxl_actions()
    actions = bxl_actions.actions

    # Attrs to pass into the anonymous target. An anonymous target is defined by the hash of its attributes
    my_attrs = {
        "false": False,
        "int": 42,
        "list_string": ["a", "b", "c"],
        "string": "foo-bar-string",
        "true": True,
    }

    # A function to be applied to the promise (result of anon target), producing a promise with the resulting value.
    def my_function(providers):
        # Do something with the attrs. In this example, we are validating that the attrs are what we expect.
        mirrored_fields = providers[MirrorInfo].mirrored_attrs
        assert_eq(mirrored_fields.true, True)
        assert_eq(mirrored_fields.false, False)
        assert_eq(mirrored_fields.int, 42)
        assert_eq(mirrored_fields.string, "foo-bar-string")
        assert_eq(mirrored_fields.list_string, ["a", "b", "c"])

        outputs = providers[DefaultInfo].default_outputs
        # These are the providers this target returns
        return [DefaultInfo(default_outputs = outputs), StringInfo(my_string = "map function succeeded!")]

    # Create an anonymous target by passing in "my_attrs" into "my_mirror_rule", and returns providers.
    # Specifically, it returns "DefaultInfo" and "MirrorInfo", as defined in "my_mirror_rule"
    # Then, we map the result to "my_function", which does some validation
    promise = actions.anon_target(my_mirror_rule, my_attrs).promise.map(my_function)

    # Resolving the promise returns a "provider_collection", which was defined by "my_function" above.
    # `DefaultInfo` is at index 0, `StringInfo` is at index 1
    promise_result = ctx.resolve(actions, promise)

    ensured = ctx.output.ensure(promise_result[0].default_outputs[0])
    # should print out location of the output, which contains the "my_content" string as defined in anon_bxl_rules.bzl above
    ctx.output.print(ensured)

    # should print out "map function succeeded!"
    ctx.output.print(promise_result[1].my_string)

def assert_eq(a, b):
    if a != b:
        fail("Expected {} == {}".format(a, b))

anon_target_example = bxl_main(
    impl = _anon_target_example,
    cli_args = {
    },
)
```
