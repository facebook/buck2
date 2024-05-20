/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Trackers that records dependencies and reverse dependencies during execution of requested nodes

use allocative::Allocative;
use static_assertions::assert_eq_size;
use typed_arena::Arena;

use crate::impls::deps::graph::SeriesParallelDeps;
use crate::impls::key::DiceKey;
use crate::impls::value::DiceValidity;

pub(crate) mod encoding;
pub(crate) mod graph;
pub(crate) mod iterator;

/// The 'DepsTracker' is used to record dependencies of a particular compute node by calling
/// 'record' for each dependency, and then getting a list of 'Dependency's at the end by calling
/// 'collect_deps'.
#[derive(Allocative)]
pub(crate) struct RecordingDepsTracker {
    deps: RecordedDeps,

    /// While a parallel computation is happening (from ctx.compute_many()/etc), we'll have an Arena here
    /// where each parallel ctx gets a slot for its deps. After the parallel computation is finished, we'll
    /// then record this into deps above.
    #[allocative(skip)] // TODO(cjhopman): Fix this.
    curr_parallel: Option<Box<SyncArena<RecordedDeps>>>,
}

#[derive(Allocative)]
pub(crate) struct RecordedDeps {
    pub(crate) deps: SeriesParallelDeps,
    pub(crate) deps_validity: DiceValidity,
}

impl RecordedDeps {
    fn record(&mut self, k: DiceKey, validity: DiceValidity) {
        self.deps.insert(k);
        self.deps_validity.and(validity);
    }

    pub(crate) fn new() -> Self {
        RecordedDeps {
            deps: SeriesParallelDeps::None,
            deps_validity: DiceValidity::Valid,
        }
    }

    #[allow(clippy::boxed_local)]
    fn insert_parallel(&mut self, mut parallel: Arena<RecordedDeps>) {
        let mut new_keys = 0;
        let mut new_specs = 0;

        for dep in parallel.iter_mut() {
            self.deps_validity.and(dep.deps_validity);
            let header = dep.deps.header();
            new_keys += header.keys_len();
            new_specs += header.encoded_len();
        }
        if new_keys == 0 {
            return;
        }

        self.deps.insert_parallel(
            parallel
                .iter_mut()
                .map(|v| std::mem::replace(&mut v.deps, SeriesParallelDeps::None)),
            new_keys,
            new_specs,
        );
    }

    #[cfg(test)]
    pub(crate) fn iter_keys(&self) -> impl Iterator<Item = DiceKey> + '_ {
        self.deps.iter_keys()
    }
}

assert_eq_size!(RecordingDepsTracker, [usize; 4]);

fn _check_deps_trackers_send_and_sync() {
    fn _assert_send_sync<T: Send + Sync>() {}
    _assert_send_sync::<RecordingDepsTracker>();
}

impl RecordingDepsTracker {
    pub(crate) fn new() -> Self {
        Self {
            deps: RecordedDeps::new(),
            curr_parallel: None,
        }
    }

    pub(crate) fn record(&mut self, k: DiceKey, validity: DiceValidity) {
        self.flatten_parallel();
        self.deps.record(k, validity)
    }

    /// Used to start a new parallel computation. Returns the Arena that each parallel ctx should record its deps to.
    pub(crate) fn push_parallel(&mut self, size_hint: usize) -> &Arena<RecordedDeps> {
        self.flatten_parallel();
        assert!(self.curr_parallel.is_none());
        self.curr_parallel
            .insert(Box::new(SyncArena::with_capacity(size_hint)))
            .inner()
    }

    pub(crate) fn collect_deps(mut self) -> RecordedDeps {
        self.flatten_parallel();
        self.deps
    }

    /// "Flattens" the previous parallel computation into deps if there is one. This should be called on any function
    /// that accesses/writes to deps.
    fn flatten_parallel(&mut self) {
        if let Some(parallel) = self.curr_parallel.take() {
            self.deps.insert_parallel(parallel.into_inner())
        }
    }
}

mod sync_arena {
    // We put SyncArena in its own mod to make the inner Arena truly private.
    use typed_arena::Arena;

    pub(super) struct SyncArena<T>(Arena<T>);

    /// Safety: SyncArena only exposes apis taking `&mut self` and `self`.
    unsafe impl<T: Sync> Sync for SyncArena<T> {}

    impl<T> SyncArena<T> {
        pub(super) fn with_capacity(s: usize) -> Self {
            Self(Arena::with_capacity(s))
        }

        pub(super) fn inner(&mut self) -> &mut Arena<T> {
            &mut self.0
        }

        pub(super) fn into_inner(self) -> Arena<T> {
            self.0
        }
    }
}
use sync_arena::SyncArena;

#[cfg(test)]
pub(crate) mod testing {
    use crate::impls::deps::RecordingDepsTracker;
    use crate::impls::key::DiceKey;
    use crate::HashSet;

    pub(crate) trait RecordingDepsTrackersExt {
        fn recorded_deps(&self) -> HashSet<DiceKey>;
    }

    impl RecordingDepsTrackersExt for RecordingDepsTracker {
        fn recorded_deps(&self) -> HashSet<DiceKey> {
            self.deps.iter_keys().collect()
        }
    }
}

#[cfg(test)]
mod tests {

    use itertools::Itertools;
    use typed_arena::Arena;

    use crate::impls::deps::iterator::ParallelNodeIterator;
    use crate::impls::deps::iterator::SeriesParallelDepsIteratorItem;
    use crate::impls::deps::RecordedDeps;
    use crate::impls::deps::RecordingDepsTracker;
    use crate::impls::key::DiceKey;
    use crate::impls::value::DiceValidity;
    use crate::HashSet;

    struct DisplaySPDeps<'a, T: Iterator<Item = SeriesParallelDepsIteratorItem<'a>>>(T);
    impl<'a, T: Iterator<Item = SeriesParallelDepsIteratorItem<'a>>> DisplaySPDeps<'a, T> {
        fn debug_string(self) -> String {
            SeriesNodeDisplay(self.0)
                .to_lines()
                .into_iter()
                .map(|v| v.trim().to_owned())
                .join("\n")
        }
    }

    struct SeriesNodeDisplay<'a, T: Iterator<Item = SeriesParallelDepsIteratorItem<'a>>>(T);
    impl<'a, T: Iterator<Item = SeriesParallelDepsIteratorItem<'a>>> SeriesNodeDisplay<'a, T> {
        fn to_lines(&mut self) -> Vec<String> {
            let mut lines = Vec::new();
            lines.push("S".to_owned());
            for item in self.0.by_ref() {
                lines.push("|".to_owned());
                match item {
                    SeriesParallelDepsIteratorItem::Key(k) => lines.push(format!("K({})", k.index)),
                    SeriesParallelDepsIteratorItem::Parallel(p) => {
                        lines.extend(ParallelNodeDisplay(p).to_lines())
                    }
                }
            }
            lines.push("|".to_owned());
            lines.push("E".to_owned());
            lines
        }
    }

    struct ParallelNodeDisplay<'a>(ParallelNodeIterator<'a>);
    impl ParallelNodeDisplay<'_> {
        fn to_lines(&mut self) -> Vec<String> {
            let mut inner_lines = Vec::new();
            let mut inner_widths = Vec::new();
            let mut longest = 0;
            for item in self.0.by_ref() {
                let lines = SeriesNodeDisplay(item).to_lines();
                let width = lines.iter().map(|v| v.len()).max().unwrap_or(1);
                longest = std::cmp::max(longest, lines.len());
                inner_lines.push(lines);
                inner_widths.push(width);
            }

            let mut lines = Vec::new();
            lines.push("P".to_owned());

            let mut prefix_line = String::new();
            let mut suffix_line = String::new();
            let mut total_width = 0;
            for (i, width) in inner_widths.iter().enumerate() {
                total_width += width + 1;
                if i == 0 {
                    prefix_line += "|";
                    suffix_line += "|";
                } else {
                    prefix_line += "\\";
                    suffix_line += "/";
                }
                // 1 short because we want to offset the \ and /
                prefix_line += &" ".repeat(total_width - prefix_line.len() - 1);
                suffix_line += &" ".repeat(total_width - suffix_line.len() - 1);
            }

            lines.push(prefix_line);
            for i in 0..longest {
                let mut line = String::new();
                let mut total_width = 0;
                for j in 0..inner_lines.len() {
                    total_width += inner_widths[j] + 1;
                    match inner_lines[j].get(i) {
                        Some(v) => line += v,
                        None => line += "|",
                    }
                    line += &" ".repeat(total_width - line.len());
                }
                lines.push(line);
            }
            lines.push(suffix_line);
            lines.push("J".to_owned());
            lines
        }
    }

    #[tokio::test]
    async fn recording_deps_tracker_tracks_deps() -> anyhow::Result<()> {
        let mut deps_tracker = RecordingDepsTracker::new();

        deps_tracker.record(DiceKey { index: 2 }, DiceValidity::Valid);
        deps_tracker.record(DiceKey { index: 3 }, DiceValidity::Valid);

        let recorded_deps = deps_tracker.collect_deps();
        let expected = HashSet::from_iter([DiceKey { index: 2 }, DiceKey { index: 3 }]);
        let actual: HashSet<_> = recorded_deps.iter_keys().collect();
        assert_eq!(actual, expected);
        assert_eq!(recorded_deps.deps_validity, DiceValidity::Valid);

        Ok(())
    }

    #[test]
    fn recording_deps_tracker_tracks_deps_invalid() -> anyhow::Result<()> {
        let mut deps_tracker = RecordingDepsTracker::new();

        deps_tracker.record(DiceKey { index: 2 }, DiceValidity::Valid);
        deps_tracker.record(DiceKey { index: 3 }, DiceValidity::Transient);

        let recorded_deps = deps_tracker.collect_deps();
        let expected = HashSet::from_iter([DiceKey { index: 2 }, DiceKey { index: 3 }]);
        let actual: HashSet<_> = recorded_deps.iter_keys().collect();
        assert_eq!(actual, expected);
        assert_eq!(recorded_deps.deps_validity, DiceValidity::Transient);

        Ok(())
    }

    #[test]
    fn test_series_parallel_record_and_iter() -> anyhow::Result<()> {
        let mut tracker = RecordingDepsTracker::new();

        {
            let p1 = tracker.push_parallel(0);
            {
                let s1 = p1.alloc(RecordedDeps::new());
                s1.record(DiceKey { index: 11 }, DiceValidity::Valid);
                s1.record(DiceKey { index: 12 }, DiceValidity::Valid);
                s1.record(DiceKey { index: 13 }, DiceValidity::Valid);
                s1.record(DiceKey { index: 14 }, DiceValidity::Valid);
                s1.record(DiceKey { index: 15 }, DiceValidity::Valid);
                s1.record(DiceKey { index: 16 }, DiceValidity::Valid);
                s1.record(DiceKey { index: 17 }, DiceValidity::Valid);
                s1.record(DiceKey { index: 18 }, DiceValidity::Valid);
                s1.record(DiceKey { index: 19 }, DiceValidity::Valid);
            }
            {
                let s2 = p1.alloc(RecordedDeps::new());
                s2.record(DiceKey { index: 21 }, DiceValidity::Valid);
                {
                    let p2 = Box::new(Arena::new());
                    {
                        let s3 = p2.alloc(RecordedDeps::new());
                        s3.record(DiceKey { index: 22 }, DiceValidity::Valid);
                        s3.record(DiceKey { index: 23 }, DiceValidity::Valid);
                    }
                    {
                        let s3 = p2.alloc(RecordedDeps::new());
                        s3.record(DiceKey { index: 24 }, DiceValidity::Valid);
                        s3.record(DiceKey { index: 25 }, DiceValidity::Valid);
                    }
                    s2.insert_parallel(*p2);
                }
            }
        }

        tracker.record(DiceKey { index: 91 }, DiceValidity::Valid);
        tracker.record(DiceKey { index: 92 }, DiceValidity::Valid);
        tracker.record(DiceKey { index: 93 }, DiceValidity::Valid);

        {
            let p2 = tracker.push_parallel(3);
            for i in 0..5 {
                let s = p2.alloc(RecordedDeps::new());
                s.record(DiceKey { index: 32 + i }, DiceValidity::Valid);
            }
        }
        tracker.record(DiceKey { index: 94 }, DiceValidity::Valid);
        tracker.record(DiceKey { index: 95 }, DiceValidity::Valid);

        let deps = tracker.collect_deps();

        eprintln!("{:?}", &deps.deps);

        let rendered = DisplaySPDeps(deps.deps.iter()).debug_string();

        let expected = indoc::indoc! {r"
                S
                |
                P
                |    \
                S     S
                |     |
                K(11) K(21)
                |     |
                K(12) P
                |     |    \
                K(13) S     S
                |     |     |
                K(14) K(22) K(24)
                |     |     |
                K(15) K(23) K(25)
                |     |     |
                K(16) E     E
                |     |    /
                K(17) J
                |     |
                K(18) E
                |     |
                K(19) |
                |     |
                E     |
                |    /
                J
                |
                K(91)
                |
                K(92)
                |
                K(93)
                |
                P
                |    \     \     \     \
                S     S     S     S     S
                |     |     |     |     |
                K(32) K(33) K(34) K(35) K(36)
                |     |     |     |     |
                E     E     E     E     E
                |    /     /     /     /
                J
                |
                K(94)
                |
                K(95)
                |
                E
        "}
        .trim();
        assert_eq!(
            rendered, expected,
            "rendered:\n{}\nexpected:\n{}\n\n{:?}",
            rendered, expected, deps.deps
        );

        Ok(())
    }
}
