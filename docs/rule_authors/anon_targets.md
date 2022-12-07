# Anonymous targets

An anonymous target is defined by the hash of its attributes, rather than its name. During analysis, rules can define and access the providers of anonymous targets, before producing their own providers. Two distinct rules might ask for the same anonymous target, sharing the work it performs. This solves two distinct problems:

* The sharing problem. If you have two things that want to share some work, you can create an anon target that does that work once, and then they can both reuse it. Without that mechanism, all sharing must be present in the target graph - you can't create any new sharing.
* The overlay problem. This is the idea that you want to have a shadow-graph, similar in structure to the normal graph, but with additional information attached. Bazel accomplishes this with [aspects](https://bazel.build/extending/aspects). With anon targets you can create a shadow-graph by convention, just by using the target name you wish to shadow as the attribute.

Dynamic dependencies in their full generality allow users to do things, look at the result, then ask for fresh things. We don't want to provide that full generality as it breaks things like query that power Target Determinator. We already have [`dynamic_output`](dynamic_dependencies.md) which lets users create new actions after running actions and looking at the result. Anon targets lets users create new analysis (call an anon target that may not have existed before) after looking at the result of previous analysis (which they get passed in, or after looking at an anon target). In many ways anon target is the version of `dynamic_output` at analysis time rather than action time.

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
    }).map(k)
```

Notes:

* We define an anonymous rule using `rule`. These are normal rules, with the difference that they are not in a configuration, so `ctx.actions.label` won't show configuration information, but just `unspecified`.
* We use an anonymous rule using `ctx.actions.anon_target`, passing in the rule and the attributes for the rule.
* The return value is a `promise` type, which when evaluated returns the providers of the anonymous target. The `promise` type has a few special behaviors.
    * It has a `map` function, which takes a function and applies it to the future, returning a new future.
    * If analysis returns a `promise` type, the outer Rust layer invokes the future to get at the analysis result. If that future then returns another future, Rust keeps going until it has a final result. It must eventually get to a list of providers.
* Attribute resolution is handled differently from normal code:
    * String/Int/Bool happen as normal.
    * The name attribute is optional, but if present must be a syntactically valid target, but can refer to a cell/package that does not exist.
    * Deps attributes do not take strings, but dependencies, already in a configuration.
    * Exec_deps are not available
    * Transitions and more complex forms of attributes are banned.
    * Default `attr.deps` (e.g. as used for toolchains) are not permitted, as the default can't express a dependency. They must be passed forward from the caller.
* The execution platform for an anon target is that of the inherited from the calling target, which is part of the hash. If that is too restrictive, we could use execution groups, where an anon target gets told which execution group to use.


## Longer example

Let's imagine we have a compile-and-link language, but if two targets end up compiling the same file (e.g. they are in the same package and both list it, or it gets export_file'd) then we only compile that file once.


```python
## TARGETS ##############
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
    ))
    return [DefaultInfo(), _SillyCompilation(compiled = out)]

_silly_compilation = anon_rule(
    impl = _silly_compilation_impl,
    attrs = {
        "src": attrs.src(),
        "toolchain": attrs.dep(),
    },
)

# We may define `anon_targets` as a builtin in future.
# ctx.actions, but it isn't necessary, but will increase parallelism
def anon_targets(ctx, xs, k):
    def f(ctx, xs, ps, k):
        if len(xs) == 0:
            return k(ctx, ps)
        else:
            return ctx.actions.anon_target(xs[0][0], xs[0][1]).map(
                lambda ctx, p: f(ctx, xs[1..], ps+[p], k)
            )
    return f(ctx, xs, [], k)

def _silly_binary_impl(ctx):
    def k(ctx, providers):
        # Step 2: now link them all together
        out = ctx.actions.declare_output("out.exe")
        objs = [p[_SillyCompilation].compiled for p in providers]
        ctx.actions.run(cmd_args(
            ctx.attrs._silly_toolchain.linker,
            objs,
            "-o",
            out.as_output(),
        ))
        return [
            DefaultInfo(default_output = out),
            RunInfo(args = out),
        ]

    # Step 1: compile all my individual files
    return anon_targets(ctx,
        [(_silly_compilation, {
            "src": src,
            "toolchain": ctx.attrs._silly_toolchain
        }) for src in ctx.attrs.srcs],
        k
    )

silly_binary = rule(
    impl = _silly_binary_impl,
    attrs = {
        "srcs": attr.list(attr.src()),
        "link_flags": attr.args(),
        "_silly_toolchain": attr.dep(default = "toolchains//:silly"),
    },
)
```
