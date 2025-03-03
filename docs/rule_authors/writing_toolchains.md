---
id: writing_toolchains
title: Writing Toolchains
---

Toolchains are regular rules that:

- Have `is_toolchain_rule = True` passed to the `rule` call.
- Return a toolchain provider, which is just a struct with a name like
  `*ToolchainInfo`.

Toolchain rules are instantiated once in the `toolchains//` cell. The
location of the `toolchains` cell is determined by the value of
`cells.toolchains` in the `.buckconfig` file.

Regular build rules reference those toolchain targets as
[`toolchain_dep`](https://buck2.build/docs/api/build/attrs/#toolchain_dep)
attrs (often
[`default_only`](https://buck2.build/docs/api/build/attrs/#default_only)
ones). See
<https://buck2.build/docs/rule_authors/configurations/#toolchain-deps>
for more information about toolchain deps.

## Writing a custom toolchain

People will often first encounter toolchains when they want to switch
off of the demo toolchains that `buck2 init` uses by default. For
example, one might want to tweak which compiler is used, which flags are
passed to it, or where it is fetched from.

The most straightforward thing to do first is to instantiate one of the
"system" toolchain rules that the prelude offers. Studying the
toolchains defined by the `system_demo_toolchains` macro in
`@prelude//toolchains:demo.bzl` is a good way to get started.

For example, here is how one could define a C++ toolchain that builds
projects in C++23, with warnings as errors and optimizations enabled:

```python
load("@prelude//toolchains:cxx.bzl", "system_cxx_toolchain")

system_cxx_toolchain(
    name = "cxx",
    compiler_type = "clang",
    cxx_flags = [
        "-std=c++23",
        "-Wall",
        "-Wextra",
        "-Werror",
        "-O3",
    ],
    visibility = ["PUBLIC"],
)
```

One would typically use
[`select`](https://buck2.build/docs/rule_authors/configurations_by_example/)
to customize the toolchain e.g. based on the build mode (debug vs
release) or compiler type.

Note that several toolchains require you to also define a
`python_bootstrap` toolchain. This is because those toolchains use
Python scripts (e.g. to generate compilation databases for C++), and the
prelude gives you control over the Python toolchain used for this
purpose. The bootstrap Python toolchain can be different from the
toolchain used by `python_*` rules. `@prelude//toolchains:python.bzl`
provides a simple `system_python_bootstrap_toolchain` in case you do not
care about tightly controlling the Python used when building other
languages.

Completely custom toolchains can also easily be written as rules with
`is_toolchain_rule = True` which return a `*ToolchainInfo` struct. The
[prelude's toolchains](https://github.com/facebook/buck2/tree/main/prelude/toolchains),
including `system_cxx_toolchain` referenced earlier, can serve as
examples.

## Accessing a toolchain in a build rule implementation

Before going any further, note that using a toolchain is often
unnecessary. It is perfectly possible for a build rule to reference
regular targets as dependencies, without the added complexity of
defining a toolchain for them. For example, one can add an (often
`default_only`) `attrs.exec_dep` to a build rule's `attrs` to do code
generation in a custom rule without creating a toolchain for it, as long
as there is no need to decouple the code generator being used from the
rule implementation.

With that being said, when a rule must be made generic over a toolchain,
all that is needed is to add that toolchain as a `toolchain_dep` attr to
a build rule. Let's consider the following made up rule that would
simply call a compiler on a source file:

```python
# We assume that `FooToolchainInfo` is a struct with a `compiler` field.
def _foo_binary_impl(ctx: AnalysisContext) -> list[Provider]:
    output = ctx.actions.declare_output(ctx.label.name)
    ctx.actions.run(
        [ctx.attrs._compiler, ctx.attrs.source, "-o", output.as_output()],
        category = "foo_compile",
    )
    return [DefaultInfo(default_outputs = [output])]

foo_binary = rule(
    impl = _foo_binary_impl,
    attrs = {
        "source": attrs.source(),
        "_foo_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:foo", providers = [FooToolchainInfo])),
    },
)
```

## Writing a hermetic toolchain

One of the benefits of Buck2 is that it makes it quite easy to write a
hermetic toolchain, meaning one that does not look up tools in the
environment, but instead explicitly downloads and tracks them as part of
the build.

Doing is typically as follows:

- Download the tools, e.g. using the
  [`http_archive`](https://buck2.build/docs/prelude/globals/#http_archive)
  rule (see the [Zig-based C++ toolchain in the
  prelude](https://github.com/facebook/buck2/tree/main/prelude/toolchains/cxx/zig)
  as an example).
- Expose those tools as
  [`RunInfo`](https://buck2.build/docs/api/build/RunInfo/) providers in
  rules that are referenced as [exec
  deps](https://buck2.build/docs/rule_authors/configurations_by_example/#exec-deps)
  in a toolchain implementation.
