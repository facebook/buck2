# BXL

BXL is a Starlark based script that allows integrators to inspect and interact with the buck2 graph.
Integrators are able to write Starlark code that queries, analysis, and builds the buck2 graph, and
interact with the buck2 graph structures natively via Starlark in a safe, controlled manner.

BXL is currently still under actively development.
Consider a soft launch, slightly stable, with some missing APIs, subject to changes.

## Running a BXL
To run a bxl function, invoke the buck2 command `buck2 bxl <bxl function> -- <function args>`, where `<bxl function>`
is of the form `<cell path to function>:<function name>`, and `<function args>` are the arguments that the function
accepts from the command line.
You can see the docs for a bxl function by running `buck2 bxl <bxl function> -- --help`. Note that this is different
from `buck2 bxl --help`, which generates the help for the buck2 command instead of the function.


## Writing a BXL
To create a bxl, first, create a script somewhere in the repository ending in `.bxl`.
In it, define a bxl function as follows:

```python
def _your_implementation(ctx):
    # ...
    pass

your_function_name = bxl(
    implementation = _your_implementatation,
    cli_args = {
        # cli args that you want to receive from the command line
        "bool_arg": cli_args.bool(),
        "list_type": cli_args.list(cli_args.int()),
        "optional": cli_args.option(cli_args.string()),
        "target": cli_args.target_label(),
    },
)
```

This exposes `your_function_name` as a function, with whatever arguments you defined it so that on the command line
one would invoke `buck2 bxl //myscript.bxl:your_function_name -- --bool_arg true --list_type 1 --list_type 2 --target //foo:bar`.

The implementation function takes a single context as parameter. See documentation for `BxlContext`.
On it, you'll be able to access functions that lets you perform queries, analysis, builds, and even create your own
actions within bxl to build artifacts as part of a bxl function.

The primary way to return information from bxl is to either print them, or build some artifact. See the `OutputStream`
documentation available as part of `ctx.output` for details. But a high level, `ctx.output.print(..)` prints results
to stdout, and `ctx.output.ensure(artifact)` marks artifacts as to be materialized into buck-out by the end of the bxl
function, returning an object that lets you print the output path via `ctx.output.print(ensured)`.
