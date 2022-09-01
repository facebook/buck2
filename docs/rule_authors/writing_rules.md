# Writing rules

This document describes how to go about writing rules for Buck2. For a list of the API functions available, see [Rule APIs](rule_api.md). This document explains the flow for implementing rules that are already defined in Buck1, but see later in this document for how to write new rules. Note that "rules" like `@fbcode_macros//build_defs:native_rules.bzl buck_genrule` are not actually rules, but _macros_ - Starlark functions that eventually call out the underlying `genrule` _rule_. This file only focuses on rules, as macros in Buck2 are mostly compatible with Buck1 and should be written in the same way.

## Workflow

The builtin Buck2 rules are stored in `fbsource` in `fbcode/buck2/prelude`. To add a rule for a language, say `pascal`, we'll:

* Look at [`prelude/attributes.bzl`](https://www.internalfb.com/code/fbsource/fbcode/buck2/prelude/attributes.bzl) to see the attributes that are supported in Buck1 and are mirrored into Buck2. If `pascal` was an existing rule you would see what attributes it takes (often it will be `pascal_libary` and `pascal_binary`).

* Create a file at `prelude/pascal.bzl` which will contain your rule implementations. We'll explain all the details later, but a dummy rule looks like:

```python
def pascal_binary_impl(_ctx: "context") -> ["provider"]:
    return [DefaultInfo()]
```

* Register that rule in [`prelude/rules_impl.bzl`](https://www.internalfb.com/code/fbsource/fbcode/buck2/prelude/rules_impl.bzl), which involves adding a `load(":pascal.bzl", "pascal_binary_impl")` at the top and an additional entry in `implemented_rules` section to wire up `pascal_binary = pascal_binary_impl`.

* Create a directory in `fbcode/buck2/tests/targets/rules/pascal` with `TARGETS` and whatever source files and test targets you need to test your project. Note, Apple tests are currently located at `xplat/buck2/tests/apple/...`.

* Test your code with `buck2 build fbcode//buck2/tests/targets/rules/pascal:`. They should succeed with no actual output produced.

* Now implement the rules. See the rest of this doc.

* Before merging a diff, it's important that all your Starlark is warning free (we'll test this in CI if you don't want to set up Buck2 for local development). If you do set it up locally, see the `README.md` in the root of `fbcode/buck2` and then running `./test.py --lint-only` will confirm your Starlark code is warning free.

## Concepts and design

A _rule_ for a _target_ uses the _attributes_ to declare _actions_ which produce _artifacts_ that get included in _providers_.

For example, given:

```python
def pascal_binary_impl(ctx: "context") -> ["provider"]:
    ...
    binary = ctx.actions.declare_output(ctx.attrs.out)
    ctx.actions.run(["pascalc", args, "-o", binary.as_output()])
    return [
        DefaultInfo(default_outputs = [binary]),
    ]
```

* The _rule_ is `pascal_binary` which is implemented by `pascal_binary_impl`. The rule says how to build things.
* The _target_ will be something like `fbcode//buck2/tests/targets/rules/pascal:my_binary`. The rule implementation `pascal_binary_impl` will be called once per target.
* The _attributes_ are the fields on the target, e.g. you might have `out` - a rule can access these via `ctx.attrs.out`.
* The _actions_ are declared by the rule with things like `ctx.actions.run`, which takes a command line. Note that the actions are not run by the rule, but declared, so that Buck2 can run them later.
* The _artifacts_ represent files on disk, perhaps source, perhaps build outputs - e.g. `binary` in the above example. For build outputs, the artifact is produced by an action, and the existence of the artifact does not imply the build has been run - the artifact remembers what should be run if it is required.
* We return _providers_, which are the information that other rules get to use. These will often contain artifacts.

The rule implementation takes in a `ctx`, which is the rule context. The two most important fields are `ctx.attrs`, which picks up the rules attributes declared by the rule, and `ctx.actions`, which lets you create new actions to actually do something.

The output of any actions performed will be materialized in `buck-out`. However, only the defined outputs of providers are available for dependent rules to consume. And, only the actions necessary to produce those outputs being consumed will be run. By default, the `default_outputs` of the `DefaultInfo` provider are built and output during a `buck build`.

### Providers

Providers are the data returned from a rule, and are the only way that information from this rule is available to rules that depend on it. Every rule must return at least the `DefaultInfo` provider, but most will also return either `RunInfo` (because they are executable) or some custom provider (because they are incorporated into something that is ultimately executable).

The `DefaultInfo` provider has a field `default_outputs` which are the files that will be built when someone does `buck2 build` on this particular target, and the files that will be used when someone does `$(location target)` or adds a dependency, e.g. `srcs = [":my_target"]`. Our current rule of thumb is that if you can build the `default_outputs`, the rule must "work", and if usable, should be "ready". E.g. for a binary we might return the executable and runtime libraries it depends on. For a library, because neither the static or dynamic library is the "default", we merely have to do enough work to ensure that the static and dynamic library probably work.

Much like `DefaultInfo` wraps a list of artifacts, and `$(location)` selects from `DefaultInfo`, we have `RunInfo` which wraps a command line, and `$(exe)` which selects from `RunInfo`. See the section on the run action for more information about command lines.

For libraries, usually you need to pass some information about the library up to the binary. The _only_ information the things that depend on the library get are the providers, so designing the information that flows around the provider is critical to designing good rules. For our hypothetical rule, we might decide we want the name of the library, and the artifact that represents the `.so` file, so we might define a provider:

```python
PascalLibraryInfo = provider(fields=[
    "name",   # The name of the library
    "object"  # An artifact, the .so file that needs linking in
    ]
)
```

Often you'll grab your dependencies from all your providers:

```python
my_deps = [x[PascalLibraryInfo] for x in ctx.attrs.deps]
```

In many cases it becomes apparent you need the transitive closure of all libraries (e.g. the libraries and everything they depend upon), in which case the standard pattern is to move to a provider of a list of `record` (see the [`types.md`](https://github.com/facebookexperimental/starlark-rust/blob/main/docs/types.md) document) and the `flatten/dedupe` functions, defining it as:

```python
PascalLibraryInfo = provider(fields=["links"]) # a list of LinkData

LinkData = record(name = str.type, object = "artifact")
```

And then consuming it:

```python
my_links = dedupe(flatten([x[PascalLibraryInfo].links for x in ctx.attrs.deps]))
my_info = PascalLibraryInfo(links = my_links)
```

However, this `flatten`/`dupe` pattern can get expensive, especially when you have a deep dependency graph. To fix that we recommend using [transitive sets](transitive_sets.md).

### Actions

There are several actions you can use, to create symlink trees etc.

#### Run action

Of the various actions, the `run` action is by far the most important - it's the one that invokes a command line. A command line is both a list of string arguments, and a list of artifacts they depend on - with syntactic niceties for adding artifacts to command lines in a way that ensures the dependencies are usually correct. As some examples of manipulations on command lines:

```python
cmd = cmd_args(["some", "arguments"])
cmd.add("another-arg")
cmd.add(ctx.attrs.src) # An input artifact
out = ctx.actions.declare_output("an output")
cmd.add(out.as_output())
ctx.actions.run(cmd)
```

The action `declare_output` creates a new artifact which is not bound to anything. You can call `.as_output()` on it when adding it to a command line to say that this command line doesn't take the artifact as an input, but produces it as an output. From now on, if `out` is used as a dependency (either to another command line, or in `DefaultInfo`) then the action will be run to produce that artifact. Typically these outputs are declared (`declare_output`), bound in a `ctx.actions.run` call with `.as_output()`, then either used locally as the input to another action, or returned in a provider.

As another example:

```python
cmd = cmd_args(["cp", input, output.as_output()])
ctx.actions.run(cmd)
```

A command provides both a string (what to write when used) and a list of artifacts (what must be available when used). Normally, as in the case above, the artifacts that are used correspond to those on the command line. But let’s imagine we change the rule to write the command to a shell script first:

```python
sh = ctx.actions.write("test.sh", ["cp", input, output])
cmd = cmd_args(["sh",sh])
cmd.hidden([input, output.as_output()])
ctx.actions.run(cmd)
```

Now we've written the command to a shell script, which we run. Beforehand, all the artifacts used by the command appeared on the command line. Now they don't, but the shell script still accesses input and output. To inform the run command, we use the hidden field of the command line to declare the dependency.

For more complicated actions, where the action does meaningful logic beyond invoking a simple command, we tend to write custom Python scripts. Python scripts are used instead of shell scripts as they have better cross-platform compatibility and fewer hidden corners (especially in error paths). As an example of a Python helper, see [this action](https://www.internalfb.com/code/fbsource/fbcode/buck2/prelude/cxx/tools/make_comp_db.py). A further advantage of using Python is that these commands can be tested in isolation, outside Buck2.

## Debugging

The functions `fail` and `print` are your friends. To get things going, a `buck2 build fbcode//buck2/tests/targets/rules/pascal:` will build everything or `buck2 run fbcode//buck2/tests/targets/rules/pascal:my_binary` will run a specific binary that returns a `RunInfo`.

## Testing

A common way to tests is to use `genrule` to cause the produced binary to run, and assert some properties from it. If your rule is in Buck1 and Buck2, use a `TARGETS` file so you can test with both. If your tests are incompatible with Buck1 (e.g. because it is a new rule), use `TARGETS.v2` which will only be seen by Buck2, so won't cause errors for Buck1.

## New rules

If your rule is _not_ already in Buck1, then you can define it wherever you like, with a preference for it not being in `fbcode/buck2/prelude`. The only advantage of the `prelude` is that rules can be used without a corresponding `load`, which is generally considered a misfeature. The attributes should usually be placed adjacent to the rule itself. As an example, just below the `pascal_binary_impl` function, you could write:

```python
pascal_binary = rule(
    impl = pascal_binary_impl,
    attrs = {
        "deps": attrs.list(attrs.dep()),
        "src": attrs.source(),
    }
)
```
