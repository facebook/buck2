# Heap Profiling

Buck2, when built by Buck, uses `jemalloc` as its allocator (the default for fbcode when not building
with `@mode/dev`). Jemalloc is a powerful general-purpose allocator that, in addition to providing the
familiar `malloc`/`free` interface, also provides deep profiling capabilities that we can use to profile
Buck2. Jemalloc is also a popular allocator in the open-source world and, for a time, was even the
default allocator for all Rust programs.

When using jemalloc as its allocator, Buck2 is capable of dumping a heap profile on command. This
document covers how to use this mechanism to debug memory performance problems in Buck2.

## Prerequisites

This guide assumes a few things about your setup:

1. **You're on a devserver or OnDemand**. While this is not strictly required, the powerful
   `stackstoscuba` program is only available on your devserver.
2. **You have installed jemalloc's developer tools**. You can do this by pulling from our local package
   mirror: `sudo dnf install jemalloc-devel-5.2.1-2.el8.x86_64`. You can check whether or not this
   package is installed by running `which jeprof`; it should resolve to an installed binary.

Otherwise, we'll be operating on a vanilla devserver.

## Getting Started

Suppose you have a memory usage problem. Great! (or, not great, I guess that's probably why you're here)
. The first step to performing a memory profile of buck2 is to launch it with profiling enabled. Buck2
performs most of its work in a daemon process, so we must take care that the daemon process is also
launched with profiling enabled. You can enable it in your sell by setting the environment variable
`MALLOC_CONF` to have the value `prof:true`.

For convenience, we also have a script, `buck2/facebook/scripts/buck2prof.py`, which handles this for
you. We will be using this script for the remainder of the guide. `buck2prof.py` takes a single
argument: a path to a `buck2` binary to profile.

`buck2prof.py` will, upon run, re-launch the buck2 daemon with profiling enabled and await further
instructions.

**Note**: You will need to profile a version of Buck with symbols embedded into the binary. This notably
does not include the `buck2` binary that we ship; we recommend that you build a `buck2` yourself using
buck and `@mode/dbgo` to produce a profilable binary.

```shell
devvm2595 :: fbsource/fbcode/buck2 1 » python3 facebook/scripts/buck2prof.py ~/local/buck2
profiling binary: /home/swgillespie/local/buck2
buck2 daemon is running with pid 661914, killing it...done!
launching daemon with profiling enabled...done!
buck2 daemon running with pid 888560, ready to receive requests
Buck2 profiler shell, at your service. Type help or ? to list commands.

(buck2prof)
```

At this point, `buck2prof` has launched a daemon (with pid 888560) and is ready to receive further
instructions. You are also free to begin instructing the daemon to do whatever you want to profile.
I'm interested in punishing my devserver, so I'm going to run `buck2 targets //...` in another terminal
and let that run for a bit.

**Note**: You must launch your Buck2 command using the same `buck2` binary that you gave to
`buck2prof.py`, otherwise the daemon will be restarted and you will not be happy!

`buck2prof.py` is waiting for further instructions (you can run the `help` command for a list of all
commands it supports, but it only supports one command right now...). You can give it the `dump` command
followed by a path to request that buck2 take a heap dump and save it to the given path. Like so:

```shell
(buck2prof) dump /tmp/during_load_1.dmp
writing heap dump to /tmp/during_load_1.dmp...done!
uploading heap dump to Scuba...
I0819 17:02:14.833994 925667 Readers.cpp:315] Parsing jeprof.out output from /tmp/during_load_1.dmp for pid 895222
I0819 17:02:15.874034 925667 RemoteOnlySymbolizer.cpp:278] Unable to resolve package for file at (dev=0x1b,inode=282485853) from "/proc/895222/map_files/55f0d7622000-55f0d9561000" / "/proc/895222/root/data/users/swgillespie/buck2" using "/proc/925667/fd/6"
I0819 17:02:16.429167 925667 StacksToScuba.cpp:178] Read 8470 samples from /tmp/during_load_1.dmp
I0819 17:02:19.852669 925667 StacksToScuba.cpp:245]
I0819 17:02:19.852739 925667 StacksToScuba.cpp:246] SUCCESS!
I0819 17:02:19.852769 925667 StacksToScuba.cpp:247]
I0819 17:02:19.852782 925667 StacksToScuba.cpp:248] Sent 8470 samples to Scuba! (might take a few mins to arrive)
I0819 17:02:19.852807 925667 StacksToScuba.cpp:251] Icicle view: https://fburl.com/scuba/stacks_to_scuba/iul9ktfy
I0819 17:02:19.852829 925667 StacksToScuba.cpp:252] GraphProfiler view: https://fburl.com/scuba/stacks_to_scuba/gu4oitpt
I0819 17:02:19.852856 925667 StacksToScuba.cpp:254] Tags: run_id=1629417736435
```

This command takes a heap dump, saves it to the given location, and writes the heap dump to Scuba. When
writing to Scuba, the `stackstoscuba` tool symbolicates the dump using the running `buck2` process,
which is why your binary needs debug symbols embedded.

Clicking on this dump in Scuba reveals a lot of Starlark allocations, which is not particularly
unexpected since we took this dump while loading lots of build files. Now that `buck2 targets //...` is
done, let's look at the daemon heap again:

```shell
(buck2prof) dump /tmp/after_targets.dmp
writing heap dump to /tmp/after_targets.dmp...done!
uploading heap dump to Scuba...
I0819 17:06:08.511217 948568 Readers.cpp:315] Parsing jeprof.out output from /tmp/after_targets.dmp for pid 895222
I0819 17:06:09.076061 948568 RemoteOnlySymbolizer.cpp:278] Unable to resolve package for file at (dev=0x1b,inode=282485853) from "/proc/895222/map_files/55f0d7622000-55f0d9561000" / "/proc/895222/root/data/users/swgillespie/buck2" using "/proc/948568/fd/6"
I0819 17:06:09.591980 948568 StacksToScuba.cpp:178] Read 9416 samples from /tmp/after_targets.dmp
I0819 17:06:12.301870 948568 StacksToScuba.cpp:245]
I0819 17:06:12.301915 948568 StacksToScuba.cpp:246] SUCCESS!
I0819 17:06:12.301934 948568 StacksToScuba.cpp:247]
I0819 17:06:12.301950 948568 StacksToScuba.cpp:248] Sent 9416 samples to Scuba! (might take a few mins to arrive)
I0819 17:06:12.301964 948568 StacksToScuba.cpp:251] Icicle view: https://fburl.com/scuba/stacks_to_scuba/3f92pxdp
I0819 17:06:12.301975 948568 StacksToScuba.cpp:252] GraphProfiler view: https://fburl.com/scuba/stacks_to_scuba/h9e2zi42
I0819 17:06:12.301982 948568 StacksToScuba.cpp:254] Tags: run_id=1629417969596
```

A bunch of Starlark objects again. Again, not surprising.

## Comparing Two Dumps

Let's check out these two dumps in more detail by comparing them directly against one another. The
`Tags:` line output by `StacksToStuba` in the above commands corresponds directly to a filter on the
`Sample Tags` column in the `Stacks to Scuba` Scuba database, so let's compare them directly using
that: [https://fburl.com/scuba/stacks_to_scuba/7kesdr26](https://fburl.com/scuba/stacks_to_scuba/7kesdr26).

When sorting by `delta`, we can see a `1.21%` increase in `<buck2_build_api::nodes::TargetNode>::from_params`
allocations; we can probably assume these are target graph nodes, which would make since since the
second dump was farther along in the load than the first one.
