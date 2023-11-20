---
id: anon_targets
title: Anonymous Targets
---

An anonymous target is defined by the hash of its attributes, rather than its
name. During analysis, rules can define and access the providers of anonymous
targets before producing their own providers. Two distinct rules might ask for
the same anonymous target, sharing the work it performs.

This solves two distinct problems:

- **The sharing problem** - if you have two processes that want to share some
  work, you can create an anon target that does that work once, which is then
  reused by the two processes. Without such a mechanism, all sharing must be
  present in the target graph: you can't create any new sharing.
- **The overlay problem** - this is the idea that you want to have a
  shadow-graph, similar in structure to the normal graph, but with additional
  information attached. Bazel accomplishes this with
  [Aspects](https://bazel.build/extending/aspects). With Anonymous (anon)
  targets, you can create a shadow-graph by convention, just by using the target
  name you wish to shadow as the attribute.

Dynamic dependencies, in their full generality, enable users to do a thing, look
at the result, then ask for fresh things. However, this full generality is not
provided as it breaks processes, like query, that power the Target Determinator.

In Buck2, dynamic dependencies are implemented using `dynamic_output`, which
provides users with the ability to create new actions, after running actions,
then look at the result. `dynamic_output` is restricted in its power when
compared to fully generic dynamic dependencies, as detailed in the
[Dynamic Dependencies](dynamic_dependencies.md) page.

Anon targets enable users to create a new analysis (that is, call an anon target
that may not have existed before) after looking at the result of a previous
analysis (which is passed in, or after looking at an anon target). In many ways,
anon target is the version of `dynamic_output` at analysis time, rather than
action time.

## Simple Example

```python
# Define an anonymous rule
UpperInfo = provider(fields = ["message"])

def _impl_upper(ctx):
    return [UpperInfo(message = ctx.attrs.message.upper()]

upper = rule(
    attrs = {"message", attrs.string()},
    impl = _impl_upper
)

# Use an anonymous target
def impl(ctx):
    def k(providers):
        print(providers[UpperInfo].message)
        # These are the providers this target returns
        return [DefaultInfo()]
    return ctx.actions.anon_target(upper, {
        name: "my//:greeting",
        message: "Hello World",
    })
    .promise
    .map(k)
```

Notes:

- An anonymous rule is defined using `rule` or `anon_rule`. For `rule`, these
  are normal rules, with the difference that they are not in a configuration, so
  `ctx.actions.label` won't show configuration information, but just
  `unspecified`. For `anon_rule`, the configuration restrictions also apply, and
  there is an `artifact_promise_mappings` field which you can specify a dict of
  artifact promise names to the map function, which would be applied to the anon
  target's promise during rule resolution.
- An anonymous rule is used via `ctx.actions.anon_target`, passing in the rule
  and the attributes for the rule.
- The return value is a `AnonTarget` type, which has a `promise` attribute, and
  `artifact()` and `artifacts()` functions.
- The `promise` attribute returns the anon target's promise (type is `promise`),
  which when evaluated returns the providers of the anonymous target. The
  `promise` type has a few special behaviors.
  - It has a `map` function, which takes a function and applies it to the
    future, returning a new future.
  - If analysis returns a `promise` type, the outer Rust layer invokes the
    future to get at the analysis result. If that future then returns another
    future, Rust keeps going until it has a final result. It must eventually get
    to a list of providers.
- Attribute resolution is handled differently from normal code:
  - String/Int/Bool happen as normal.
  - The name attribute is optional, but, if present, must be a syntactically
    valid target, but can refer to a cell/package that does not exist.
  - Deps attributes do not take strings, but dependencies, already in a
    configuration.
  - Exec_deps are available if the passed in dep's execution platform matches
    that of the anon target's caller.
  - Transitions and more complex forms of attributes are banned.
  - Default `attr.deps` (as used for toolchains) are not permitted, as the
    default can't express a dependency. They must be passed forward from the
    caller.
- The `artifact()` and `artifacts()` functions only return something if using
  `anon_rule`. `artifact()` takes in an artifact name, which should be found in
  the `artifact_promise_mappings` dict, and returns the artifact promise.
  `artifacts()` returns the dict of all promise artifact names to the artifact
  promise itself, as defined in `artifact_promise_mappings`. See
  [Convert promise to artifact](#convert-promise-to-artifact) below for more
  information about artifact promises.
- The execution platform for an anon target is that of the inherited from the
  calling target, which is part of the hash. If that is too restrictive, you
  could use execution groups, where an anon target gets told which execution
  group to use.

## Longer example

The following code represents a scenario for a compile-and-link language where,
if two targets end up compiling the same file (for example, they are in the same
package and both list it, or it gets export_file'd), then that file is compiled
just once:

```python
## BUCK ##############
@load(":silly.bzl", "silly_binary")

silly_binary(
    name = "hello",
    srcs = ["hello.sil", "world.sil"],
)

## silly.bzl ############

_SillyCompilation = provider(fields = ["compiled"])

def _silly_compilation_impl(ctx):
    out = ctx.actions.declare_output("output.o")
    ctx.actions.run(cmd_args(
        ctx.attrs.toolchain.compiler,
        ctx.attrs.src,
        "-o",
        out.as_output(),
    ), category = "compile")
    return [DefaultInfo(), _SillyCompilation(compiled = out)]

_silly_compilation = rule(
    impl = _silly_compilation_impl,
    attrs = {
        "src": attrs.src(),
        "toolchain": attrs.dep(),
    },
)

def _silly_binary_impl(ctx):
    def k(providers):
        # Step 2: now link them all together
        out = ctx.actions.declare_output("out.exe")
        objs = [p[_SillyCompilation].compiled for p in providers]
        ctx.actions.run(cmd_args(
            ctx.attrs._silly_toolchain.linker,
            objs,
            "-o",
            out.as_output(),
        ), category = "link")
        return [
            DefaultInfo(default_output = out),
            RunInfo(args = out),
        ]

    # Step 1: compile all my individual files
    return ctx.actions.anon_targets(
        [(_silly_compilation, {
            "src": src,
            "toolchain": ctx.attrs._silly_toolchain
        }) for src in ctx.attrs.srcs]
    ).promise.map(k)

silly_binary = rule(
    impl = _silly_binary_impl,
    attrs = {
        "srcs": attr.list(attr.src()),
        "link_flags": attr.args(),
        "_silly_toolchain": attr.dep(default = "toolchains//:silly"),
    },
)
```

## Convert promise to artifact

It can be challenging to pass around the promises from anon_target and structure
functions to support that. If you only need an artifact (or multiple artifacts)
from an anon_target, you can use `artifacts()` function on the anon target to
convert a promise to an artifact. This artifact can be passed to most things
that expect artifacts, but until it is resolved (at the end of the current
analysis) it can't be inspected with artifact functions like `.extension`, etc.
`.short_path` is supported if `ctx.actions.assert_short_path()` was called,
which produces an artifact type. The promise must resolve to a build (not
source) artifact with no associated artifacts.

Example:

```python
HelloInfo = provider(fields = ["hello", "world"])

def _anon_impl(ctx: AnalysisContext) -> ["provider"]:
    hello = ctx.actions.write("hello.out", "hello")
    world = ctx.actions.write("world.out", "world")
    return [DefaultInfo(), HelloInfo(hello = hello, world = world)]

_anon = anon_rule(
    impl = _anon_impl,
    attrs = {},
    artifact_promise_mappings = {
        "hello": lambda x: x[HelloInfo].hello,
        "world": lambda x: x[HelloInfo].world,
    }
)

def _use_impl(ctx: AnalysisContext) -> ["provider"]:
    anon = ctx.actions.anon_target(_anon, {})
    hello_artifact = anon.artifact("hello")
    world_artifact = anon.artifact("world")

    out = ctx.actions.declare_output("output")
    ctx.actions.run([
            ctx.attrs.some_tool,
            hello_artifact,
            world_artifact,
            out.as_output()
        ], category = "process")
    return [DefaultInfo(default_output = out)]

use_promise_artifact = rule(impl = _use_impl, attrs = {
    "some_tool": attr.exec_dep(),
})

```
