---
title: Benefits
---

# Benefits compared to Buck1

Buck2 is an improvement over the existing Buck1 build system. In this post we lay out the concrete benefits we expect for people using Buck2. All these benefits are the things that are available today for many fbcode users.

<FbInternalOnly>

For reports from real users see [our testimonials page](testimonials.md), which includes Workplace posts giving full context.

</FbInternalOnly>

## For end users

For people who use Buck on a daily basis, e.g. buck build as part of their development inner loop, Buck2 is faster and more correct.

> `buck2 build SOME_TARGET_I_ALREADY_BUILT_BEFORE` is basically instantaneous, and is a super delightful experience 🙂<FbInternalOnly> ([source](https://fb.prod.workplace.com/groups/buck2users/posts/3030704467185914))</FbInternalOnly>

We expect users who switch to experience the following benefits:

* **Performance**. Buck2 is faster than Buck1. The build system stands between a developer writing code and getting feedback to start the next iteration, so any reduction can be hugely significant. The performance of Buck2 is better in four ways:
  * _Fast things are fast_. In Buck1, simply typing `buck build` when there is nothing to do can be expensive - 23s in some benchmarks. In Buck2 that is 0.1s. Things that should be fast actually are, so developers can use Buck more freely, without trying to work around the build system.
  * _Slow things are faster_. When there is real work to do, Buck2 is significantly closer to the critical path. Benchmarks range from 5%/10s faster for changing a header file (lots of parallel C++ computations, Buck1 already nearly at the critical path) to 42%/145s faster (changing a Thrift file in a large project).
  * _Users contribute to the shared cache_. With Buck1 only Sandcastle builds write to the network cache, while with Buck2 everyone writes to the cache. That change increases the chance of cache hits, saving capacity and time.
  * _CI builds go faster_. These numbers vary day by day, but most projects are 2-4x faster. Spend less time waiting for CI and save some capacity at the same time.
* **Correctness**. In Buck2 rules are hermetic by default. Missing dependencies are errors. These restrictions apply to both the user-written `TARGETS`/`BUCK` files and the language rules. In the process of migrating to Buck2 we have fixed a huge number of missing dependencies and identified several others in Buck1 that won’t be fixed (e.g. missing headers, genrules without dependencies, OCaml rules don’t track all deps etc). The end result is that Buck2 gives the right answer more often, cutting down on user surprises.
* **Rule features**. The rules in Buck2, especially for less commonly used languages (e.g. Haskell, OCaml, Rust) support features above and beyond those in Buck1. As examples, dependencies can be given as arguments to `prebuilt_ocaml_library`, Haskell lets you use stub headers from C++, Rust has experimental pipelining support.
* **Actively developed**. The Meta build team is putting all their efforts behind Buck2. It’s vastly easier to develop than Buck1. While we’re already ahead of Buck1 in many important aspects, the difference is only going to grow with several improvements in the pipeline. We can provide much better help to those having difficulties with Buck2 than we can with Buck1.

## Rule authors

If you write the language specific rules, Buck2 is in a different league to Buck1.

> This is all rather fun! Buck2 rules are so much more hackable than Buck1<FbInternalOnly>
([source](https://fb.prod.workplace.com/groups/333784157210625/posts/928214407767594))</FbInternalOnly>

There are a number of reasons why Buck2 excels for rule authors.

* **Faster developer cycle**. In Buck1 the time from changing a rule to seeing the impact is many minutes - you first have to compile Buck1, invalidate the dependency cache etc, perhaps work between multiple OSs. With Buck2, it takes seconds, you don’t even need to restart the daemon.
* **Simple API**. Buck2 rules use a small and documented Starlark API, which is dependency-correct by construction. In Buck1 the rules must obey a lot of subtle side conditions with a much larger API.
* **Easier deployment**. For Buck2 deployment is just checking the rules in, with an atomic commit changing associated macros if you want. For Buck1 you have to make the repo work with the old and new rules, and wait for a Buck version bump to ship your changes, perhaps a few days later.
* **Low barrier to entry**. Writing rules in Buck2 is vastly easier than Buck1, significantly increasing the developer pool. That means that language experts can write the rules, rather than Buck experts.

## Integrators

For those people who integrate Buck2 into larger systems, many of the benefits above apply, but Buck2 can do even more for them.

> Buck2 is largely faster and more memory efficient than buck1, and where I’ve seen counter-examples, the buck2 team quickly optimizes and fixes that<FbInternalOnly> ([source](https://fb.prod.workplace.com/groups/devx.ci.bffs/posts/616830502778501))</FbInternalOnly>

* **Faster query**. Many integrators make extensive use of `buck uquery` and `cquery`. In Buck2 these commands are _faster_ and use _less memory_. Taking one example, the FBCode runtime TD job (a bunch of targets/queries), Buck2 is 25% faster at P50 (moving to 40% faster at P95) with 25% less memory (saving over 20Gb, and crossing below the 64Gb threshold).
* **Profiling**. Buck2 already ships with 5 types of profiling for both loading and analysis (flame graphs, statement breakdown, heap profiles etc). With Buck2 these tools are much more easily accessible to people not on the Build Infra team.

<FbInternalOnly>

* **Eden friendly**. Buck2 is tuned for the Eden architecture, performing fewer disk operations with greater parallelism. As one example, the slowdown caused by using Eden for `targets` on `fbandroid` is [reduced from 300s to 80s](https://fb.workplace.com/groups/132499338763090/posts/132580122088345).
* **Better observability**. Buck2 populates many Scuba tables with information about [loading](https://www.internalfb.com/intern/scuba/query/?dataset=buck2_loads), [analysis](https://www.internalfb.com/intern/scuba/query/?dataset=buck2_analyses), [builds](https://www.internalfb.com/intern/scuba/query/?dataset=buck2_builds) and [errors](https://www.internalfb.com/intern/scuba/query/?dataset=buck2_action_errors) etc. The architecture of Buck2 ensures that all important information can be recorded in a uniform manner, allowing us to make sensible trade offs about what to store vs for how long.

</FbInternalOnly>

## The downsides

This page lays out the value proposition for Buck2, but we’d be remiss if we didn’t include the (temporary) list of downsides.

* **Stability**. Buck2 is under active development. That means the risk of regression is correspondingly higher. We will make mistakes. But we’ll fix them rapidly and learn from them through our SEV-review process.
* **Corner cases**. Buck1 has been battle tested for nearly a decade. That has included attention to things like error messages in unlikely corner cases. Only time and user feedback will allow us to bring Buck2 to the same level. Please share all such feedback!

<FbInternalOnly>

* **Buck2 Web UI**. There isn’t a working Web UI equivalent to the one provided by Buck1 yet. But we’re working on it, and hope to share an initial version shortly.

</FbInternalOnly>
