# What Makes Buck so Fast?

Buck exploits a number of strategies to reduce build times.

## Buck builds dependencies in parallel

Buck is designed so that any input files required by a [build target](https://buck.build/concept/build_target.html) must be specified in the [build rule](https://buck.build/concept/build_rule.html) for that target. Therefore, we can know that the directed acyclic graph [(DAG)](http://en.wikipedia.org/wiki/Directed_acyclic_graph) that Buck constructs from the build rule is an accurate reflection of the build's dependencies, and that once a rule's dependencies are satisfied, the target for that rule can be built.
Having a DAG makes it straightforward for rules to be built in parallel, which can dramatically reduce build times. Buck starts with the leaf nodes of the graph, that is, targets that have no dependencies. Buck adds these to a queue of targets to build. When a thread is available, Buck removes a target from the front of the queue and builds it. Assuming the target builds successfully, Buck notifies all of the rules that depend on that target. When all of a rule's dependencies have been satisfied, Buck adds that rule's target to the build queue. Computation proceeds in this manner until all of the nodes in the graph have been built. This execution model means that breaking modules into finer dependencies creates opportunities for increased parallelism, which improves throughput.

## Buck uses only first-order dependencies for Java

When compiling Java, Buck uses first-order dependencies only, that is, dependencies that you specify explicitly in the `deps` argument of your build rule. This means that the compilation step in your build sees only explicitly-declared dependencies, not other libraries that those dependencies themselves depend on.
Using only first-order dependencies dramatically shrinks the set of APIs that your Java code is exposed to, which dramatically reduces the scope of changes that will trigger a rebuild.
**NOTE:** If your rule does, in fact, depend on a dependency of one of your explicitly-specified dependencies—such as a *second-order* dependency—you can make that dependency available to your rule by specifying it in an `exported_deps` argument in the rule of the explicitly-specified dependency.

## Buck uses dependency files to trim over-specified inputs

Buck's low-level build rules specify all inputs—such as source files or the outputs from other build rules—that might contribute to the output when the build rule is executed. Normally, changes to any of these inputs result in a new RuleKey and therefore trigger a rebuild. However, in practice, it's not uncommon for these build rules to *over-specify* their inputs. A good example is Buck's C/C++ compilation rules. C/C++ compilation rules specify as inputs all headers found from the transitive closure of C/C++ library dependencies, even though in many cases only a small subset of these headers are actually used. For example, a C/C++ source file might use only one of many headers exported by a C/C++ library dependency. However, there's not enough information available before running the build to know if any given input is used, and so all inputs must be considered, which can lead to unnecessary rebuilding.
In some cases, after the build completes, Buck can figure out the exact subset of the listed inputs that were actually used. In C/C++, compilers such as `gcc` provide a `-M` option which produces a dependency file. This file identifies the exact headers that were used during compilation. For supported rules, Buck uses this dependency file before the build, to try to avoid an unnecessary rebuilding:

* If the dependency file is available before the build, Buck reads the file and uses it to filter out unused inputs when constructing the RuleKey.
* If no dependency file is available before the build, Buck runs the build as normal and produces a dependency file. The dependency file is then available for subsequent builds.

Note that dependency files are used only if the standard RuleKey—which considers all inputs—doesn't match. In cases where the RuleKey matches, the output from the rule can be fetched from the cache.
