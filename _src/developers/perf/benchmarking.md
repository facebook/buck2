---
id: perf_benchmarking
title: Benchmarking
---

[basics.md](basics.md) covers the perf basics shared with profiling. This
page is benchmarking-specific: variance, sample sizes, fair comparison
between two binaries.

## Effect sizes

We typically aim to detect changes down to 0.3–0.5%; anything over 1% is a large win or regression.
With absh this may require ~50 samples or more. Single-shot measurements detect ~nothing in this
range — every benchmark needs many iterations.

`absh` is the standard A/B benchmarking tool:

```bash
# See `--help` for details
buck2 build @fbcode//mode/opt fbsource//third-party/rust:absh-absh --out /tmp/absh
```

## Per-iteration variance

| Metric                     | Stddev across runs                |
|----------------------------|-----------------------------------|
| Wall time                  | 100 ms – 1 s on a 15 s build      |
| Daemon `VmHWM` (peak RSS)  | ~50 MB on a 4–5 GB build          |
| jemalloc `allocated`       | a few MB; very stable             |

`allocated` is stable enough that small samples are usable; for `VmHWM` and
wall time the only way to detect sub-1% effects is `absh`'s
paired-difference setup across many iterations.

## absh flag reference

```sh
absh -a 'buck2 ...A...' -b 'buck2 ...B...' -i -r -m -n 30
```

| Flag             | Effect                                                |
|------------------|-------------------------------------------------------|
| `-i`             | Ignore the first iteration of each variant            |
| `-r`             | Randomize order of A and B within each iteration      |
| `-m`             | Also measure max RSS of the spawned process           |
| `-n N`           | Stop after N successful iterations                    |
| `--max-time SEC` | Mark a run as failed past this many seconds           |

Always pass `-i` and `-r`.

`-m` is a key reason to prefer `--no-buckd` for memory benchmarks
(see [basics.md](basics.md#the-process-model)). With `--no-buckd` the
spawned process *is* the daemon doing the work, so `-m` captures the
meaningful peak RSS directly. With daemon mode `-m` only sees the thin
gRPC client; for real daemon RSS you'd need
[`scripts/measure.sh`](scripts/measure.sh) to read `VmHWM` from
`/proc/<daemon-pid>/status` instead.

## Two-binary comparison

When `A` and `B` are different `buck2` binaries:

- For `--no-buckd` benchmarks (the typical case for wall-time and peak
  RSS), each invocation is fresh — no daemon state to manage.
- For daemon-mode benchmarks (needed for retained memory), alternating
  binaries between iterations naturally kills and restarts the daemon
  via version skew, which gives you fresh DICE per iteration with no
  extra setup.

One checkout is enough either way. Worktrees only matter if you want to
keep a real daemon alive in your main checkout while a long absh loop
runs in another.

## What metric answers what

| Question                            | Metric                                                              |
|-------------------------------------|---------------------------------------------------------------------|
| "How long does buck2 take?"         | Wall time (`absh` reports it; `/usr/bin/time` works for `--no-buckd`) |
| "How much memory at peak?"          | `--no-buckd` + absh `-m`, or `VmHWM` from daemon `/proc` in daemon mode |
| "How much is the daemon retaining?" | `allocator-stats.allocated`, daemon mode only                       |
| "How much CPU?"                     | User+sys time across daemon + client + forkserver                   |
| "Is buck2 doing more I/O?"          | Page faults / `/proc/<pid>/io`                                      |

Reporting "client max RSS" via `time -v` while in daemon mode is the most
common mistake — it's nearly constant regardless of build complexity
because the client is just a gRPC shim.
