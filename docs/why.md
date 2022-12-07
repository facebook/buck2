# Why Buck2

Buck2 is a build system from Meta. But why does Meta have a build system? Why might it be interesting to you? And why might you want to use it?

## Why does Buck2 exist?

Meta has a very large repo, consisting of lots of different programming languages (including C++, Python, Rust, Kotlin, Swift, Objective-C, Haskell, OCaml and more). Those two factors (the scale plus the multi-language nature) are beyond the capabilities of traditional build systems like `make`. As a result, many large tech companies, including Facebook and Google, built their own build systems, respectively Buck and Bazel. While the internal version of Bazel was started first (also known as Blaze), Buck was open sourced first (back in March 2013), followed by Bazel a few years later (March 2015).

Buck1 (as we have retroactively named it) was a capable build system, still in use today at Meta (although many users have migrated). Buck2 is a rewrite which aims to keep the best bits of Buck1 (with a high degree of target compatibility), but also borrowing ideas from [academic](https://ndmitchell.com/#shake_10_sep_2012) [research](https://ndmitchell.com/#shake_21_apr_2020) and build systems including [Bazel](https://bazel.build/), [Pants](https://www.pantsbuild.org/), [Shake](https://shakebuild.com/), [Tup](https://gittup.org/tup/) and more besides. Looking at the pieces that are common to both Buck1 and Buck2 (and in most cases, also Bazel):

* **Targets which can be queried** - the build is defined as a series of targets, specified in `TARGETS` files, which depend on other targets. This graph of targets can be queried to understand how they related to each other, and what the potential impact of a change might be.
* **Remote execution** - the build can send actions to a set of remote servers to be executed, increasing the parallelism significantly.
* **Multi-language composability** - there can be lots of different languages in a single build, and they can be put together. E.g. you can have a Python library that depends on a Rust library that depends on a C library.
* **File watching** - at large enough scale, simply looking for changed files is prohibitively expensive. Buck can integrate with [Watchman](https://facebook.github.io/watchman/) to discover which files have changed efficiently. However, for simplicity of setup, the open source version defaults to using `inotify` or similar functionality.
* **Uses Starlark** - Starlark is a deterministic Python-like language used to specify the targets, allowing both defining targets as literals, but also more advanced manipulation/sharing.

## Why might it be interesting?

Buck2 has many minor differences from Buck1, but there are a number that give new efficiency or expressiveness that are of note (most of these are also differences from Bazel).

* Buck2 is **written in Rust** compared to Buck1, which is written in Java. There are advantages to using Rust, such as the absence of GC pauses, but Java definitely has advantages such as better memory profiling tools.
* Buck2 is **remote execution first** - local execution is considered a special case of remote execution, in contrast to Buck1 where it was added after. That means that things like directory hashes etc can be pre-computed ready to send to remote execution, giving efficiency benefits.
* All Buck2 **rules are written in Starlark**, in contrast to Buck1 where they were written in Java as part of the binary, which makes iterating on rules much faster. Furthermore, the Buck2 binary itself is entirely language agnostic, so even our most important and complex rules (e.g. C++) don't have access to magic internal features. That lead to a number of features being made available to all rules, including:
    * [Dep files](rule_authors/dep_files.md) - the ability to declare that a subset of the files weren't actually used, and thus not be sensitive to changes within them.
    * [Incremental actions](rule_authors/incremental_actions.md) - the ability to have the action short-circuit some subset of the work if run again.
* Buck2 uses a dynamic (aka monadic) graph as its underlying computation engine. While most dependencies are specified statically, we have two particular features that expose **dynamic power to rule authors**:
    * [Dynamic dependencies](rule_authors/dynamic_dependencies.md) let you build a file, then look at its contents before specifying the dependencies and steps in future actions. Common uses are languages where the dependency structure within a project must follow imports (e.g. Haskell, OCaml) and distributed ThinLTO (where the best optimization plan is generated from summaries).
    * [Anonymous targets](rule_authors/anon_targets.md) let you create a graph that has more sharing that the original user graph, so two unrelated binaries can compile shared code only once, despite the shared code not knowing about this commonality. This feature is useful for rules like Swift feature resolution.
* **[Transitive-sets](rule_authors/transitive_sets.md)** are similar in purpose to Bazel's [depset](https://bazel.build/rules/lib/depset), but instead of being just a memory optimization, are also wired into the dependency graph, providing a reduction in the size of the dependency graph.
* Buck2 is **not phased**, meaning there is no target graph/action graph phases, but just a series of dependencies in a single graph that result in whatever you requested. That means that Buck2 can sometimes parallelise different phases and track changes very precisely.
* Buck2 can **integrate with the virtual filesystem** [Eden](https://github.com/facebook/sapling), giving good performance even when the file system is backed by source control fetches. However, Eden is not required, and a normal file system will also work well.
* The Buck2 **Starlark implementation is available** [as a standalone library](https://developers.facebook.com/blog/post/2021/04/08/rust-starlark-library/) with features such as IDE integration (both LSP and DAP bindings), linters, typecheckers etc. These features are integrated into Buck2 to give a better developer experience (which is still evolving).
* Buck2 **supports configurations**, `select` etc, to provide multi-platform/architecture builds, heavily inspired by Bazel. Within that space we have a number of small differences, such as `toolchain_deps`.

Compared to Buck1, Buck2 builds are typically 30-60% faster. There is a more [comprehensive list of benefits](benefits.md) compared to Buck1.

## Might you want to use it?

We would be delighted if you tried out Buck2! But it is early stage software, and we expect early users to run into unexpected issues, which we would love to have reported (e.g. via [Github issues](https://github.com/facebookincubator/buck2/issues)).

We are using Buck2 extensively inside Meta, and the open source version is as close as we can possibly make it, so hope it is pretty reliable. The places we differ from the internal version (and thus might be a little more raw) are:

* We use our internal version of remote execution and our builds are always hooked up to remote execution. Both the open source binding and using Buck2 without remote execution may be less polished.
* There are some configuration differences between open source and internal, for example file changes default to `inotify` open source and Watchman internally.
* The prelude (containing all the rules) is the same open source as internal, but our toolchains are not open sourced. The required custom toolchains may not work as well.

There are also some things that aren't quite yet finished:

* We don't have mechanisms to build in release mode yet (that should be achieved by modifying the toolchain).
* Our Windows/Mac builds are still in progress, our open source code is mostly tested on Linux.

If none of that puts you off, [give it a go](getting_started.md)!
