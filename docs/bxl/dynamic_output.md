---
id: dynamic_output
title: BXL and Dynamic Outputs
---

## Dynamic output

When declaring [dynamic outputs](../rule_authors/dynamic_dependencies.md) within
a BXL script, the dynamic lambda for is created with a `bxl_ctx`, which means
that you can do things like run analysis or queries to inspect the build graph
from within the dynamic lambda.

You may declare multiple dynamic outputs within a single BXL script, or declare
nested dynamic outputs. Dynamic outputs are run asynchronously after the BXL
evaluation.

### Limitations

- `ctx.output` is not available from a dynamic lambda. This means you can’t
  ensure artifacts or print cached outputs within a dynamic lambda.
- Error messages from skipping incompatible targets are only emitted to the
  console, and not cached in the stderr
- `build()` is not available from a dynamic lambda
- `bxl_actions` in a dynamic lambda always inherits the execution platform
  resolution of the root/parent BXL.
  - The expected usage of `bxl_actions` from within a dynamic lambda is to
    instantiate it without any named parameters, but the `exec_deps` and
    `toolchains` of the execution platform resolution are accessible, and return
    the same values as the root/parent BXL
- Profiling is not hooked up to dynamic BXL context

### Silly example

This is a silly example of creating a dynamic output which reads some
`query_params` input, calls some BXL functions like `uquery`,
`configured_targets` to get the resolved attributes of a target node, and then
writes the attributes to an output file.

```python
def _impl_dynamic_output(ctx):
    actions = ctx.bxl_actions().actions

    # Declare some input here to read within the lambda
    query_params = actions.write_json("query_params", {"rule_type": "apple_bundle", "universe": "fbcode//buck2/tests/..."})

    # Dynamic lambda's output artifact
    resolved_attrs = actions.declare_output("resolved_attrs")

    # Dynamic lambda function to be used in the dynamic_output
    def my_deferred(ctx, artifacts, outputs):

        # Read the input, then do some BXL things here

        params = artifacts[query_params].read_json()
        target = ctx.uquery().kind(params["rule_type"], params["universe"])[0]
        node = ctx.configured_targets(target.label)
        eager_attrs = node.resolved_attrs_eager(ctx)

        # Dynamic BXL context's `bxl_actions` does not take in named parameters because it inherits the exec platform resolution from the root/parent BXL. If the root BXL's `bxl_actions` were created with exec deps/toolchains, you can access them using `exec_deps` and `toolchains` attributes here

        ctx.bxl_actions().actions.write(outputs[resolved_attrs], str(eager_attrs))

    actions.dynamic_output(
        dynamic = [query_params],
        inputs = [],
        outputs = [
            resolved_attrs,
        ],
        f = my_deferred,
    )

    ctx.output.print(ctx.output.ensure(resolved_attrs).abs_path())

dynamic_output_example = bxl_main(
    impl = _impl_dynamic_output,
    cli_args = {
    },
)
```
