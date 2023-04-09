---
id: bxl_getting_started
title: Getting Started
---

## Writing a BXL

To create a BXL, first, create a script somewhere in the repository ending in `.bxl`. (Note that you can define a single bxl per file, or multiple BXLs per file like in Starlark rules).

In it, define a BXL function as follows:

```python
def _your_implementation(ctx):
    # ...
    pass

your_function_name = bxl(
    impl = _your_implementation,
    cli_args = {
        # cli args that you want to receive from the command line
        "bool_arg": cli_args.bool(),
        "list_type": cli_args.list(cli_args.int()),
        "optional": cli_args.option(cli_args.string()),
        "target": cli_args.target_label(),
    },
)
```

This exposes `your_function_name` as a function, with whatever arguments you defined it, so that on the command line you can invoke:

```text
buck2 bxl //myscript.bxl:your_function_name -- --bool_arg true --list_type 1 --list_type 2 --target //foo:bar`
```

You can also add helpdocs to the cli args and get them to show up in cli via `--help`:

```python
def _your_implementation(ctx):
    # ...
    pass

your_function_name = bxl(
    impl = _your_implementation,
    cli_args = {
        "my_bool": cli_args.bool(True, "this will be printed as part of `--help`")
    },
)
```

The implementation function takes a single context as parameter (see the documentation for `BxlContext`). Using it, you'll be able to access functions that enable you to perform queries, analysis, builds, and even create your own actions within BXL to build artifacts as part of a BXL function.

The primary method to return information from BXL is to either print them, or build some artifact (for details, see the `OutputStream` documentation, available as part of `ctx.output`).
At high level, `ctx.output.print(..)` prints results to stdout, and `ctx.output.ensure(artifact)` marks artifacts as to be materialized into buck-out by the end of the BXL
function, returning an object that lets you print the output path via `ctx.output.print(ensured)`.

## Running a BXL

To run a BXL function, invoke the buck2 command:

```text
buck2 bxl <bxl function> -- <function args>
```

 Where `<bxl function>` is of the form `<cell path to function>:<function name>`, and `<function args>` are the arguments that the function accepts from the command line.

The documentation for a BXL function can be seen by running:

```text
 buck2 bxl <bxl function> -- --help`
 ```

 Note that this is different from `buck2 bxl --help`, which generates the help for the buck2 command instead of the function.
