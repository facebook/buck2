/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! CPU budgets for one ingest or viewer load.
//!
//! Cap the stages that benefit from parallel work instead of starting a pool
//! the size of the machine for each stage. Independent trace/graph loads share
//! a budget, including their coordinating callers. This does not impose a
//! process-wide thread/RSS limit: the UI, other analyses and a cooperatively
//! canceling previous load can still be alive. Batch limits bound ordinary
//! in-flight work, not the retained graph or an accepted oversized record.

const GRAPH_MAX: usize = 4;
const TRACE_PARSE_MAX: usize = 16;

fn available() -> usize {
    std::thread::available_parallelism().map_or(1, usize::from)
}

fn pool(cores: usize, coordinators: usize, ceiling: usize) -> usize {
    cores.max(1).saturating_sub(coordinators).min(ceiling)
}

fn trace_limit(requested: usize) -> usize {
    if requested == 0 {
        TRACE_PARSE_MAX
    } else {
        requested
    }
}

fn trace_threads_for(cores: usize, requested: usize) -> usize {
    pool(cores, 1, trace_limit(requested)).max(1)
}

/// One parses inline. Larger values count workers in addition to the caller.
/// An explicit --threads value is an upper bound, still capped by hardware.
pub(crate) fn trace_threads(requested: usize) -> usize {
    trace_threads_for(available(), requested)
}

fn graph_workers_for(cores: usize) -> usize {
    match pool(cores, 1, GRAPH_MAX) {
        0 | 1 => 0,
        workers => workers,
    }
}

/// Zero is serial; otherwise there is one read/merge caller plus N workers.
pub(crate) fn graph_workers() -> usize {
    graph_workers_for(available())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn explicit_limits_do_not_oversubscribe() {
        assert_eq!(trace_threads_for(8, usize::MAX), 7);
        assert_eq!(trace_threads_for(96, 0), 16);
        assert_eq!(trace_threads_for(96, 64), 64);
    }
}
