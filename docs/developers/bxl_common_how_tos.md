---
id: bxl_how_tos
title:  Common How-Tos
---

## Passing in and using CLI args

A BXL function can accept a `cli_args` attribute where args names and types are specified to use within your script, as shown in the following example:

Example:

```python
def _impl_example(ctx):
    # ...
    pass

example = bxl(
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

For BXL functions, to read the arguments, use them as attributes from the `cli_args` attribute on the BXL `ctx` object, as follows:

```python
def _impl_example(ctx):
    my_bool_arg = ctx.cli_args.bool_arg
```

## Running actions

You can create actions within BXL via the `action_factory()`. This is called once globally then used on demand:

```python
def _impl_example(ctx):
    actions = ctx.bxl_actions.action_factory() # call once, reuse wherever needed
    output = actions.write("my_output", "out")
```

## Getting providers from an analysis

After calling `analysis()`, you can get the providers collection from `providers()`:

```python
def _impl_example(ctx):
    my_providers = ctx.analysis(my_target).providers()
```

## Get a specific provider from an analysis

After calling `analysis()`, you can also get the providers collection from `providers()` then grab whatever specific provider you need:

```python
def _impl_example(ctx):
    default_info = ctx.analysis(my_target).providers()[DefaultInfo]
    ctx.output.print(default_info)
```

## Build a subtarget

Once you have a provider, you can get its subtargets by using the `sub_targets` attribute on the struct to get a dict of provider labels to provider collections:

```python
def _impl_example(ctx):
    subtarget = ctx.analysis(my_target).providers()[DefaultInfo].sub_targets[“my_subtarget”]
    ctx.output.print(subtarget)
```

## Getting attributes or resolved attributes efficiently

If you need to use all of the attrs/resolved_attrs, then initializing the eager variant once would be best. If you only need a few of the attrs, then initializing the lazy variant is better. There’s not really a hard line, it depends on the target node, and which attrs you are looking for. If performance is key to your BXL script, the best way to determine this is to use the BXL profiler.

Regardless, if you use eager or lazy versions of getting attributes, you should cache the attrs object:

```python
def _impl_example(ctx):
    lazy = ctx.attrs_lazy() # call once and reuse wherever is necessary
    eager = ctx.attrs_eager() # call once and reuse wherever is necessary
```

## Inspecting a struct

You can use `dir(my_struct)` to inspect a struct. You can also use `getattr(my_struct, “my_attr”)` to grab individual attributes, which is equivalent to `my_struct.my_attr`.

These are available as part of the [Starlark language spec](https://github.com/google/skylark/blob/a0e5de7e63b47e716cca7226662a4c95d47bf873/doc/spec.md#dir).

## Set addition/subtraction on a `target_set`

There are a few BXL actions that return a `target_set` (such as a cquery `eval()`). The `target_set` supports set subtraction and addition (you can use `-` and `+` directly in Starlark).

## Profiling, Testing, and Debugging a BXL script

You can use `buck2 bxl profiler`, with various measurements, to determine where the script is least efficient.

To time individual pieces of the script, you can use BXL’s timestamp methods:

```python
def _impl_example(ctx):
    now = ctx.now() # call once and reuse wherever is necessary
    start = now.elapsed_millis()
    # do something time intensive here
    end = now.elapsed_millis()
```

BXL does not have a debugger available nor a robust testing framework for mocking.

* **Debug** - the main method to debug a BXL script is with print statements (`print()` and `ctx.output.print()`).
* **Test** - the main method to test a BXL script is to actually invoke it with required inputs then verify the outputs.
