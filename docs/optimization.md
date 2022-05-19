# Optimization

Techniques for figuring out the performance of Buck2, and specific actions that Buck2 performs. This document both covers the internals for developers of Buck2, and details around Starlark that are more likely relevant to end users.

## Starlark Profiling

You can cause `buck2` to profile a specific Starlark file by setting the environment variable `BUCK2_STARLARK_PROFILE=<file>`, for example, you could set `<file>` to `fbsource//fbandroid/apps/fb4a:BUCK` (or any substring thereof) to profile the `fb4a` file. It will then write a profile in `fbsource/profile.csv`. You can additional add a suffix `=STMT` (to get statement profiling) or `=FLAME` (to get flame profiling). Note that because `buck2` is a daemon, you should `buck2 kill` the daemon first. As a concrete example:

```shell
buck2 kill
BUCK2_STARLARK_PROFILE=fb4a ./buck2.sh uquery 'fbsource//fbandroid/apps/fb4a:x'
```

That command will give an error (since `x` isn't a named target), but will cause that file to evaluate and the profile to be produced.

### Heap profiling

The first profiling mode provides the time spent within a function and the allocations that are performed. As an example, running over a folly BUCK file, we get a CSV file whose top-left corner is:

```text
Function         Time(s)  TimeRec(s)    Calls   Allocs
TOTALS            10.455      10.455  9712799  3477203
fbchain_configs    1.163       2.514    11328    33984
is_string          0.726       1.028  1514985        0
apple_library      0.725       0.725     1887        0
type               0.435       0.435  2053296        0
...
```

This tells us that the total execution was 10.455s (which will be a bit slower than normal, because profiling is on), and that 1.163s was spent in `fbchain_configs` itself, and 2.514s in that function and things it calls. A disturbing 1.5M calls and 1s is spent testing if things are strings, which is almost certainly responsible for half the type calls. Happily, `is_string` doesn't allocate, but `fbchain_configs` does - and scrolling to the right on the full CSV file shows it allocates 1 tuple and 2 dict per call. We can also see that `fbchain_configs` is mostly called by `_add_code_coverage_configs`.

This profiling mode is implemented by turning off garbage collection, so the heap retains everything, and pushing function entry/exit entries on to the heap with the time they happen. After execution, we can scan the heap in order to reconstruct the call tree and allocation patterns. As a result, this profile mode may consume significantly more memory.

### Statement profiling

The second profiling mode tells us which statements spent most time executing. Running it over a structured-logger `BUCK` file gives us a CSV file starting with:

```text
File                            Span  Duration(s)    Count
TOTAL                                        4.03  7187761
fbcode_allowed_list.bzl  420:9-423:1         0.27   455884
cell_defs.bzl             13:5-13:60         0.17   117736
read_configs.bzl          46:5-46:55         0.08    65042
prelude.bzl               28:9-29:20         0.07     1004
...
```

This profile shows how much time is spent in each statement. Looking at the relevant portion of `fbode_allowed_list.bzl`:

```python
for _package in _recursive_allowlist:
    if base_path == _package or base_path.startswith(_package + "/"):
        return True
```

The `if` statement is at location 420:9-423:1 and takes 0.27s. The `if` statement runs 456K times, while looking at the outer statement in the profile (not shown) we see that the `for` loop is only called 3188 times, implying an average of 143 iterations per call. I suspect this loop could be rewritten as some clever dictionary lookup, perhaps iterating over the path components of `_package`.

Line profiling builds on top of the `before_stmt` hook that is used for debugging. It records the time each statement is entered, and then blames that statement for all time until the next statement. That means that sometimes due to statements making function calls, the `return` of the function call may be "blamed" until the next statement executes. As a result, treat the results with slight caution.

### Flame profiling

The flame profile prints out a file that can be fed to [`flameprofile.pl`](https://github.com/brendangregg/FlameGraph/blob/master/flamegraph.pl), e.g. `flameprofile.pl profile.csv > profile.svg`. The flame profile gives a list of where time goes based on call stacks. You can find an example [here](https://www.internalfb.com/intern/px/p/1Mz2W).

## Native Profiling

* Profiling on Linux can be done with `perf record -g --call-graph=dwarf,20000 ...` and `perf report --call-graph`
  * Don't profile the `buck2` process directly unless you are interested in profiling the CLI; you likely want to profile
    the `buck2` daemon process. You can find the pid with `buck2 status` and attach `perf` to that PID.
* Profiling on Mac can be done with [`Instruments` on mac](https://www.internalfb.com/intern/wiki/GraphQL/Build_Infra/Running_and_Testing_Builds/#profiling-the-rust-code).
* To run jemalloc profiling on Linux, you can run `_RJEM_MALLOC_CONF=prof:true,prof_prefix:/tmp/buck2,lg_prof_interval:28,lg_prof_sample:24 ...`. This will dump jemalloc profiles as /tmp/buck2.*.heap files. You may need to change `lg_prof_interval` or `lg_prof_sample` to finetune the number of profile files dumped. Then run `jeprof --text $PATH_TO_BINARY /tmp/buck2.*.heap | pastry` to visualize the output in text. For profiling the daemon, you may need to kill the daemon in between profiling to initialize _RJEM_MALLOC_CONF to different settings.

## Benchmarking

* If you want to do proper statistically relevant A/B testing, use `absh -a testa -b testb` - see [the repo](https://github.com/stepancheg/absh).
* To measure number of instructions, on Linux do `perf stat foo`, on Mac do `/usr/bin/time -lp foo`.
* On Mac, to run something with the time profiler on the command line, do `xcrun xctrace record --template 'Time Profiler' --launch -- foo` , then `open Foo.trace` for the name of the trace file it spits out (or pass `--output` to control the output filename).
