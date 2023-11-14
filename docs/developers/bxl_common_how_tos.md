---
id: bxl_how_tos
title: Common How-Tos
---

## Passing in and using CLI args

A BXL function can accept a `cli_args` attribute where args names and types are
specified to use within your script, as shown in the following example:

Example:

```python
def _impl_example(ctx):
    # ...
    pass

example = bxl_main(
    impl = _impl_example,
    cli_args = {
        # cli args that you want to receive from the command line
        "bool_arg": cli_args.bool(),
        "list_type": cli_args.list(cli_args.int()),
        "optional": cli_args.option(cli_args.string()),
        "target": cli_args.target_label(),
    },
)
```

On the command line, you can invoke the arguments as follows:

```sh
buck2 bxl //myscript.bxl:example -- --bool_arg true --list_type 1 --list_type 2 --target //foo:bar
```

For BXL functions, to read the arguments, use them as attributes from the
`cli_args` attribute on the BXL `ctx` object, as follows:

```python
def _impl_example(ctx):
    my_bool_arg = ctx.cli_args.bool_arg
```

## Running actions

You can create actions within BXL via the `actions_factory`. This is called once
globally then used on demand:

```python
def _impl_example(ctx):
    actions = ctx.bxl_actions().actions # call once, reuse wherever needed
    output = actions.write("my_output", "out")
```

You will need to have
[execution platforms](../rule_authors/configurations.md#execution-platforms)
enabled for your project, or else you will get an error. You can specify the
execution platform resolution by setting named parameters when instantiating
`bxl_actions`:

- `exec_deps` - These are dependencies you wish to access as executables for
  creating the action. This is usually the same set of targets one would pass to
  rule's `attr.exec_dep`. Accepts a list of strings, subtarget labels, target
  labels, or target nodes.
- `toolchains` - The set of toolchains needed for the actions you intend to
  create. Accepts a list of strings, subtarget labels, target labels, or target
  nodes.
- `target_platform` - The intended target platform for your toolchains. Accepts
  a string or target label.
- `exec_compatible_with` - Explicit list of configuration nodes (like platforms
  or constraints) that these actions are compatible with. This is the
  `exec_compatible_with` attribute of a target. Accepts a list of strings,
  target labels, or target nodes.

If you specify `exec_deps` or `toolchains`, you can access the resolved
`dependency` objects on the `bxl_actions` object. The `bxl_actions` object will
have `exec_deps` and `toolchains` attributes, which are `dict`s where the keys
are the unconfigured subtarget labels, and the values are the
configured/resolved `dependency` objects.

Note that the keys of `exec_deps` and `toolchains` must be unconfigured
subtarget labels (`StarlarkProvidersLabel`), and not unconfigured target labels.
You can use `ctx.unconfigured_sub_targets(...)` or `with_sub_target()` on
`target_label` to create the label.

```python
def _impl_example(ctx):
    my_exec_dep = ctx.unconfigured_sub_targets("foo//bar:baz") # has some provider that you would use in the action
    bxl_actions = ctx.bxl_actions(exec_deps = [my_exec_dep]) # call once, reuse wherever needed
    output = bxl_actions.actions.run(
        [
            "python3",
            bxl_actions.exec_deps[my_exec_dep][RunInfo], # access resolved exec_deps on the `bxl_actions`
            out.as_output(),
        ],
        category = "command",
        local_only = True,
    )
    ctx.output.ensure(output)
```

## Getting providers from an analysis

After calling `analysis()`, you can get the providers collection from
`providers()`:

```python
def _impl_example(ctx):
    my_providers = ctx.analysis(my_target).providers()
```

## Get a specific provider from an analysis

After calling `analysis()`, you can also get the providers collection from
`providers()` then grab whatever specific provider you need:

```python
def _impl_example(ctx):
    default_info = ctx.analysis(my_target).providers()[DefaultInfo]
    ctx.output.print(default_info)
```

## Get a specific subtarget from an analysis

Once you have a provider, you can get its subtargets by using the `sub_targets`
attribute on the struct to get a dict of provider labels to provider
collections:

```python
def _impl_example(ctx):
    subtarget = ctx.analysis(my_target).providers()[DefaultInfo].sub_targets["my_subtarget"]
    ctx.output.print(subtarget)
```

## Building a subtarget

You can use `analysis()` to get a specific subtarget from an analysis, or you
can pass in the subtarget literal directly into `ctx.build()`:

```python
def _impl_example(ctx):
    outputs = ctx.build("cell//path/to/my:target[my_subtarget]")
    ctx.output.ensure_multiple(outputs)
```

## Getting attributes or resolved attributes efficiently

If you need to use all of the attrs/resolved_attrs, then initializing the eager
variant once would be best. If you only need a few of the attrs, then
initializing the lazy variant is better. There’s not really a hard line, it
depends on the target node, and which attrs you are looking for. If performance
is key to your BXL script, the best way to determine this is to use the BXL
profiler.

Regardless, if you use eager or lazy versions of getting attributes, you should
cache the attrs object:

```python
def _impl_example(ctx):
    lazy = ctx.attrs_lazy() # call once and reuse wherever is necessary
    eager = ctx.attrs_eager() # call once and reuse wherever is necessary
```

## Inspecting a struct

You can use `dir(my_struct)` to inspect a struct. You can also use
`getattr(my_struct, "my_attr")` to grab individual attributes, which is
equivalent to `my_struct.my_attr`.

These are available as part of the
[Starlark language spec](https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#dir).

## Set addition/subtraction on a `target_set`

There are a few BXL actions that return a `target_set` (such as a cquery
`eval()`). The `target_set` supports set subtraction and addition (you can use
`-` and `+` directly in Starlark).

## Profiling, Testing, and Debugging a BXL script

You can use `buck2 bxl profiler`, with various measurements, to determine where
the script is least efficient.

To time individual pieces of the script, you can use BXL’s timestamp methods:

```python
def _impl_example(_ctx):
    start = now() # call once and reuse wherever is necessary
    # do something time intensive here
    end1 = start.elapsed_millis()
    # do something else time intensive here
    end2 = start.elapsed_millis()
```

BXL does not have a debugger available nor a robust testing framework for
mocking.

- **Debug** - the main method to debug a BXL script is with print statements
  (`print()` and `ctx.output.print()`).
- **Test** - the main method to test a BXL script is to actually invoke it with
  required inputs then verify the outputs.
