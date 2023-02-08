---
id: buck1_vs_buck2
title: Buck1 vs Buck2
---

## At a glance

The following table provides an at-a-glance comparison of Buck1 and Buck2.

|Buck1 | Buck2 |
|:--|:--|
| Build files in Starlark | Build files in Starlark |
| Macros in Starlark | Macros in Starlark |
| Rules in Java | Rules in Starlark |
| Rules and Macros are logically similar | Rules and Macros are logically similar |
| Rules and Core are not well abstracted | Rules and Core are strongly separated |
| Core in Java | Core in Rust |
| Remote Execution (RE) not well supported | All rules support remote execution by default |
| Varying degrees of incrementality / parallelism | Unified incrementality / parallelism |

## Top-down vs Bottom-up - understanding the implications of the difference in execution models between Buck1 and Buck2

It is often said that Buck1 does 'top down' and Buck2 does 'bottom up' building. This results in cases where some topics that seem conceptually trivial in Buck1 are hard problems in Buck2, or vice versa.

### What are the differences?

**Scenario**: Imagine you are building A, which depends on both B and C, but where neither B nor C have any dependencies.

For the sake of simplicity, imagine B and C are C++ compilations (that produce object files), and A is a link (that consumes them and produces a shared library).

#### Building A with Buck1

Following is an oversimplified view of what happens:

* Buck1 computes the 'rulekey' for B.
  * This consists of mixing together the hashes of the C++ file being compiled, as well as all C++ compiler flags, and so on.
* Buck1 then does the same for C.
* Buck1 then computes the rulekey for A.
  * This consist of mixing together the rulekeys of B and C, as well as linker flags used by A. for example.
* Buck1 then looks up the rulekey for A in the cache.
  * If there's a hit, then Buck1 downloads the output and its job done.
  * If there's a cache miss, continue.
* Buck1 then queries the cache for the rulekeys of B and C:
  * If there's a hit, then the output is downloaded.
  * If there's a miss, then Buck1 runs the commands needed to produce the object file that was missed. Regardless of whether those commands run locally or on RE, Buck1  downloads the output of B and C.
* Buck1 then runs the command for A to produce the shared library.
  * At this point, Buck1 may actually do another cache lookup with a different rulekey, which is called an *input based rulekey*. This rulekey is derived from the inputs of the action that needs executing, which at this point of the build are known (since they were just built)!

#### Building A with Buck2

In contrast, if you ask Buck2 to build A, here is what happens:

* Buck2 produce the action to compile B and computes the hash of the action.
  * This is the 'action digest', which consists of mixing the hashes of all the inputs (such as the C++ file), as well as the command line (so, implicitly, the compiler flags).
* Buck2 queries the action cache for the action digest hash.
  * If there's a hit, Buck2 obtains the hash of the resulting object file (that is, the output of B).
  * If there's a miss, Buck2 runs the action on RE (or potentially locally) and obtains the hash of the object file. If the action runs remotely, Buck2 will not download the output.
* Buck2 does the same thing for C.
* Buck2 produces the action to link A.
  * This consists of mixing together all the hashes of the input files (which were retrieved earlier) and the command line to produce an action digest, then querying the cache and potentially running the action.
* Once Buck2 produces A (again, on RE), then, since this output was requested by the user (unlike the intermediary outputs B and C), Buck2 downloads A.

### Some implications

#### Rulekeys vs Action digests

The closest thing to Buck1’s rulekey in Buck2 is the action digest, but they are very different!

Since it’s a product of the (transitive) inputs of an action, the (default) rulekey can be computed without running anything or querying any caches. However, the action digest cannot: it requires the actual inputs of an action, which means you need to build all the dependencies first.

This means that:

* In Buck1, you can ask for rulekeys for a target.
* In Buck2, you’d have to run the build first then ask for the action digests (this is what the `buck2 log what-ran` would show you).

#### Buck2 queries many more caches

* Buck1 will not descend further down a tree of dependency when it gets a cache hit.
* Buck2 will always walk up all your dependencies, regardless of whether you get cache hits or not.

#### Materialization

* When Buck1 gets a cache miss, it downloads the outputs.
* Buck2, by contract, does not download outputs as part of a build (this is called 'deferred materialization').
  * Note that Buck2 does download the outputs if the user asked for them (that is, they were the targets the user put on the command line).

### Second-order implications

#### Non-determinism

Non-determinism in a build affects Buck2 and Buck1 differently. One scenario that often works fine in Buck1 but can work catastrophically bad in Buck2 is a codegen step, driven by a Python binary.

In certain configurations/modes, Python binaries are non-deterministic, because they are XARs and that is always non-deterministic, which is bad!

* In Buck1, that doesn’t really matter, because you can get a cache hit on the codegen output without ever visiting the XAR (as long as the input files haven’t changed).
* In Buck2, you need the XAR to check the action cache for the codegen step.
  * However, binaries are often not cached in certain configurations/modes, so your XAR isn’t cached.
  * Therefore, since your XAR build is non-deterministic, you’ll always miss in the action cache and the codegen step will always have to run in every build.

It can get worse! If the Python binary produces non-deterministic codegen, then the entire build might become uncacheable.

#### Cache misses don’t necessarily propagate

Say that, in Buck2, you’re trying to build a chain of actions like codegen -> compile -> link.

Even if your codegen step isn’t cached (say, because its action inputs are non-deterministic as mentioned above), as long as the codegen output is deterministic, you can still get cache hits from compile and link steps.

#### Hybrid execution

If you squint, you’ll note that Buck1’s build could be viewed as 'local first', whereas Buck2’s would be better viewed as 'remote first':

* When Buck1 builds something remotely or gets a cache hit, the outputs are always downloaded.
* When Buck2 builds something remotely or gets a cache hit, the outputs are never downloaded.

In turn, this has some important implications:

* When Buck1 builds something locally, the inputs are always already present.
* When Buck2 builds something locally, the inputs have to be downloaded, unless they were built locally (which if you’re doing any RE, is usually not the case), or if another command caused them to be downloaded.

This means that, in Buck1, running something locally when you have spare resources is usually  a no-brainer, because it’s always ready to go, and you’ll save on not having to download the output from RE (though you might have to upload the output if you need to run actions depending on it later).

On the flip side, with Buck2, that’s not necessarily the case. To run an action locally, you need to download inputs that you might otherwise not have needed, which will tax your network connection.
