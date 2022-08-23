So, you want to go out on your own? The prelude in `buck2` is of course completely optional, so it is completely possible to write your own build definitions from scratch (with or without a toolchain). This guide is going to look at how to do this completely from scratch. First we'll look at how to build out the definitions without any toolchains, then we'll abstract out a toolchain, and finally as a bonus we'll adapt our build rules to take advantage of the toolchain definitions from the prelude so that your rules may be mixed and matched with the rest of the ecosystem.

buck2 is, at its core, a toolkit for running memoized 'actions' based on their inputs. So, in the context of building software, buck2 will uniquely identify the contents of your source code, the build strategy, and environment and, if that combination has been built previously, simply serve the previously cached result rather than recalculating. When your entire build graph is known to buck, it is able to effectively prune the tree of actions it needs to take to generate a 'correct' output meaning your CPU does less work to serve you your binaries.

This translates to faster iteration times, significantly improved CI (rerun only the set of tests that depend on changed code), and opens up a number of possibilties such as shared caches (downloading outputs that other people have produced) or remote execution (offload the build step to much more powerful machines).

So lets jump in and build ourselves a caching wrapper around the clang compiler and see how we can use it to build (and rebuild) libraries and binaries.

> Want to follow along? Clone the [starter repo](github.com/arlyon/buck2-starter)

## Our first rule

buck2 uses starlark to define the build graph, which is [a deterministic language inspired by Python3](https://github.com/facebookexperimental/starlark-rust). For a quick overview, have a look at the quick intro below.

<iframe src="//www.youtube.com/embed/3kHER3KIPj4" frameborder="0" allowfullscreen width="100%"></iframe>

Starlark exposes some basic functions to your `.bzl` files automatically, one of which is `rule`. This function defines a new build rule which can be used in your `BUILD` files to declare a new target. Lets jump straight in, and take advantage of `buck2 init` to create a new buck2 project.

> The no-prelude command just skips the step that adds the prelude cell and submodule to your project. To understand more about the prelude, [see the prelude guide](./prelude.md).

```sh
> buck2 init myproject --no-prelude
> cd myproject
> touch rules.bzl
```

[//]: # "todo(arlyon): build //... in an empty project gives a bad error"

A rule consists of an implementation (conventionally identified with the `_impl` suffix) and a number of inputs or `attrs`. Lets create a basic rule with an attribute.

`rules.bzl`

```python
def _cpp_binary_impl(ctx: "context") -> ["provider"]:
    out = ctx.actions.declare_output("main")
    cmd = cmd_args(["clang++", "-o", out.as_output(), ctx.attrs.file])
    ctx.actions.run(cmd, category = "compile")
    return [DefaultInfo(default_outputs = [out])]

cpp_binary = rule(
    impl = _cpp_binary_impl,
    attrs = {
        "file": attrs.string(),
    },
)
```

We will go over the building blocks of this rule in a moment, however lets attempt to use this.

`BUILD`

```python
load("//rules.bzl", "cpp_binary")

cpp_binary(
    name = "main",
    file = "main.cpp"
)
```

`main.cpp`

```cpp
#include <iostream>

int main()
{
    std::cout << "Hello world" << std::endl;
}
```

```sh
> buck2 build //... --show-output

Build ID: e514915b-bafb-4dde-9c3b-8241db8baf26
Jobs completed: 6. Time elapsed: 0.2s. Cache hits: 0%. Commands: 1 (cached: 0, remote: 0, local: 1)
BUILD SUCCEEDED
root//:main buck-out/v2/gen/root/a4d31070d122b816/__main__/main

> buck-out/v2/gen/root/a4d31070d122b816/__main__/main

Hello world
```

This is a great first step but it leaves some to be desired. What happens if we attempt to change the contents of our file? Lets change "Hello world" to "Hello earth" and see what happens.

```sh
> sed -i 's/Hello world/Hello earth/g' main.cpp
> buck2 build //...
> buck-out/v2/gen/root/a4d31070d122b816/__main__/main

Hello world
```

Looks like we have _under-specified_ our build which has led to an incorrect build. In this case, buck2 doesn't know enough about the inputs (the fact that our string actually refers to a file, and that it has changed) to reliably calculate changes. As far as buck is concerned, none of the inputs have changed so it 'correctly' returns the previously memoized result. How can we improve the information available to it?

## Attributes

> git checkout no-prelude-basic

Attributes define the input params for a rule. We are going to use a combination of the `source` and `list` attributes to change our rule from taking a single string file name, to taking a list of buck2-aware files. By doing this we enable more detailed errors ex. about the existence of missing files, and also get better change tracking. `attrs` is a globally available variable that gives access to all the possible attributes that buck2 supports.

[//]: # "todo(arlyon): link to documentation on list of attributes"

```diff
diff --git a/BUILD b/BUILD
index 3e7718d..e2bf931 100644
--- a/BUILD
+++ b/BUILD
@@ -2,5 +2,5 @@ load("//rules.bzl", "cpp_binary")

 cpp_binary(
     name = "main",
-    file = "main.cpp"
+    srcs = ["main.cpp"]
 )
diff --git a/rules.bzl b/rules.bzl
index 8320298..34e6f8c 100644
--- a/rules.bzl
+++ b/rules.bzl
@@ -1,12 +1,12 @@
 def _cpp_binary_impl(ctx: "context") -> ["provider"]:
     out = ctx.actions.declare_output("main")
-    cmd = cmd_args(["clang++", "-o", out.as_output(), ctx.attrs.file])
+    cmd = cmd_args(["clang++", "-o", out.as_output()] + ctx.attrs.srcs)
     ctx.actions.run(cmd, category = "compile")
     return [DefaultInfo(default_outputs = [out])]

 cpp_binary = rule(
     impl = _cpp_binary_impl,
     attrs = {
-        "file": attrs.string(),
+        "srcs": attrs.list(attrs.source()),
     },
 )

```

## Toolchains

> git checkout no-prelude-file

Now, our system makes some fairly heavy assumptions about the build environment, some obvious, some maybe more subtle. The first is the command. We assume that a binary for `clang++` is available on the `PATH`, and make an implicit assumption that the version will be stable across builds, otherwise building the same binary may not produce bit-for-bit identical outputs! We also assume you want to build on your local machine, and of course that you want to use `clang++` at all. How to we put this choice in the hands of the consumer of the rule, rather than the developer?

[//]: # "todo(arlyon): link to docs on providers"

The answer is in providers, and in a handy abstraction over them called a toolchain.

In a later step we will cover the providers that are bundled in the buck2 prelude, so that your toolchains target a common api and can be mixed-and-matched with other rules. However, for now, lets start with a completely homegrown solution and convert it into an 'official' toolchain afterwards to demonstrate the flexibility.

```sh
> touch toolchain.bzl
```

`toolchain.bzl`

```python
CxxCompilerInfo = provider(
    doc = "Information about how to invoke the cpp compiler.",
    fields = ["compiler_path", "include_directories", "lib_directories"],
)

def _cpp_local_toolchain_impl(ctx):
    return [DefaultInfo(), CxxCompilerInfo(compiler_path = ctx.attrs.command)]

cpp_local_toolchain = rule(
    impl = _cpp_local_toolchain_impl,
    is_toolchain_rule = True,
    attrs = {
        "command": attrs.string(),
    },
)
```

The above toolchain is a good 'first step'. It doesn't solve reproducibility of the compiler, but it _does_ allow for a mechanism for others to solve it, namely the `CxxCompilerInfo` provider. By using this as a junction point, consumers of your new rules can use whatever toolchain they want, whether that be containerized, local, hermetic, or not. You'll notice that the toolchain is just another rule, with a special marker to designate it as a toolchain. This means we can instantiate a target for it in our BUILD file similar to the `cpp_binary`.

```diff
diff --git a/BUILD b/BUILD
index e2bf931..e041b76 100644
--- a/BUILD
+++ b/BUILD
@@ -1,6 +1,12 @@
 load("//rules.bzl", "cpp_binary")
+load("//toolchain.bzl", "cpp_local_toolchain")

 cpp_binary(
     name = "main",
     srcs = ["main.cpp"]
 )
+
+cpp_local_toolchain(
+    name = "toolchain",
+    command = "/usr/bin/clang++"
+)
```

The next step is to adjust our rules to take advantage of a toolchain with this provider.

```diff
diff --git a/rules.bzl b/rules.bzl
index 34e6f8c..9125ab0 100644
--- a/rules.bzl
+++ b/rules.bzl
@@ -1,6 +1,8 @@
+load("//toolchain.bzl", "CxxCompilerInfo")
+
 def _cpp_binary_impl(ctx: "context") -> ["provider"]:
     out = ctx.actions.declare_output("main")
-    cmd = cmd_args(["clang++", "-o", out.as_output()] + ctx.attrs.srcs)
+    cmd = cmd_args([ctx.attrs.toolchain[CxxCompilerInfo].compiler_path, "-o", out.as_output()] + ctx.attrs.srcs)
     ctx.actions.run(cmd, category = "compile")
     return [DefaultInfo(default_outputs = [out])]

@@ -8,5 +10,6 @@ cpp_binary = rule(
     impl = _cpp_binary_impl,
     attrs = {
         "srcs": attrs.list(attrs.source()),
+        "toolchain": attrs.toolchain_dep([CxxCompilerInfo])
     },
 )
```

Running buck2 build will now fail unless a toolchain with the CxxCompilerInfo provider is supplied to the rule.

```sh
❯ buck2 build //...

Traceback (most recent call last):
  * BUILD:4, in <module>
      cpp_binary(
error: Missing parameter `toolchain` for call to cpp_binary
 --> BUILD:4:1
  |
4 | / cpp_binary(
5 | |     name = "main",
6 | |     srcs = ["main.cpp"],
7 | |     # toolchain = ":toolchain"
8 | | )
  | |_^
  |
Build ID: 0961b6cd-485a-4bd4-ae30-a92307fd3107
Jobs completed: 3. Time elapsed: 0.0s.
BUILD FAILED
```

The final step is to provide our newly defined toolchain.

```diff
diff --git a/BUILD b/BUILD
index e041b76..825c03e 100644
--- a/BUILD
+++ b/BUILD
@@ -3,7 +3,8 @@ load("//toolchain.bzl", "cpp_local_toolchain")

 cpp_binary(
     name = "main",
-    srcs = ["main.cpp"]
+    srcs = ["main.cpp"],
+    toolchain = ":toolchain"
 )

 cpp_local_toolchain(
```

[//]: # "todo(arlyon): toolchains from the prelude"
