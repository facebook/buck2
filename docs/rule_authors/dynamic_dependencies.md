# Dynamic dependencies

Dynamic dependencies allow a rule to use information that was not available when the rule was first run at analysis time. Dynamic dependencies in Buck2 are implemented using `dynamic_outputs`, and restricted in their power compared to fully generic dynamic dependencies.

A rule for a target is run with the attributes of the target, plus the providers of its attribute dependencies, which contain artifacts. Those values (but not the artifact contents) are all available directly and immediately when running the rule. The rule generates providers containing artifacts.  Using `dynamic_outputs`, a rule can read the contents of an artifact to produce new artifacts and bind existing artifacts which were already returned in providers.

Examples of rules requiring dynamic dependencies include:

* Distributed ThinLTO, where the index file says what the dependencies are.
* OCaml builds, where the dependencies between source files can only be obtained from running `ocamldeps`.
* Erlang header files, where only a subset of the available headers are accessed, which can be determined by reading the source file.
* Erlang BEAM files, where some subset of BEAM files must be compiled in a given order, as they provide features like compiler plugins, but most can be compiled in parallel.

A full list of worked out use cases is available in [this document](https://docs.google.com/document/d/1K8RgvDMvdDFsLWAu0cehauJstHZaFe-7NeaAqWe4-L4/edit).

## Implementation

We provide the function:

```python
ctx.actions.dynamic_output(dynamic, inputs, outputs, lambda ctx: …)
```

The arguments are:

* `dynamic` is a list of artifacts whose values will be available in the function. These will be built before the function is run.
* `inputs` is a container of artifacts (`cmd_args`, list of artifact etc). These inputs must include all the inputs that are referenced by the body of the function argument, apart from those listed in `dynamic` and `outputs`. Extra inputs may be passed that are not used. These inputs are used for `buck2 aquery` functionality, but do not cause speculative building - in fact these inputs may form a cycle with other `dynamic_output` actions if they were all required. It is possible that one day in the far future it might be possible to not pass all the inputs, if the repo is set to permissive mode, allowing a more powerful form of dynamic dependencies.
* `outputs` is a list of unbound artifacts (created with `declare_artifact`) which will be bound by the function.
* The function argument is given 3 arguments:
  * `ctx` (context): which is the same which is passed to the initial rule analysis
  * `outputs`: Given one of the artifacts from the `dynamic_output`'s `outputs` (example usage: `outputs[artifact_from_dynamic_output_outputs]`), will give an unbounded artifact. The function argument must use its `outputs` argument to bind output artifacts, rather than reusing artifacts from the outputs passed into `dynamic_output` directly.
  * `artifacts`: Given one of the artifacts from `dynamic` (example usage: `artifacts[artifact_from_dynamic])`, will give an artifact value containing the methods `read_string`, `read_lines`, and `read_json` - to obtain the values from the disk in various formats. Anything too complex should be piped through a Python script for transformation to JSON.
* The function must call `ctx.actions` (probably `ctx.actions.run`) to bind all outputs. It can examine the values of the dynamic variables and depend on the inputs. The function will usually be a `def`, as `lambda` in Starlark does not allow statements, making it quite underpowered:

As an example of using this function to determine Erlang BEAM dependencies:

```python
def erlang(ctx):
  beams = {}
  for x in ctx.attr.srcs:
    dep_file = ctx.actions.declare_output(x + ".out")
    ctx.actions.run("erl", "-dump-output", x, dep_file.as_output())
    beam_file = ctx.actions.declare_output(x + ".beam")
    beams[x] = beam_file
    def f(ctx, artifacts, outputs, x=x, dep_file=dep_file):
      deps = artifacts[dep_file].read_lines()
      ctx.actions.run(
        "erl", "-comp", x,
        [beams[d] for d in deps],
        outputs[beams[x]].as_output()
      )
    ctx.actions.dynamic_output([dep_file], [x] + deps, [beam_file], f)
  return [ErlangInfo(objects = beams.values())]
```

Observe that we `declare_output` the `beam_file`, and then bind it within the function `f`, after having read the `dep_file` with `read_lines`.
