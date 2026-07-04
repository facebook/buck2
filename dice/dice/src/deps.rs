/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

//! Trackers that records dependencies and reverse dependencies during execution of requested nodes

use crate::deps::graph::SeriesParallelDeps;
use crate::epoch::branches::ParallelGroup;
use crate::key::DiceKey;
use crate::value::DiceValidity;
use crate::value::TrackedInvalidationPaths;

pub(crate) mod encoding;
pub(crate) mod graph;
pub(crate) mod iterator;

/// The 'DepsTracker' is used to record dependencies of a particular compute node by calling
/// 'record' for each dependency, and then getting a list of 'Dependency's at the end by calling
/// 'collect_deps'.
pub(crate) struct RecordingDepsTracker {
    deps: RecordedDeps,

    /// While a parallel computation is happening (from ctx.compute_many()/etc), this owns the
    /// parallel branches' ctxs. After the parallel computation is finished, the deps the branches
    /// recorded are gathered up and written to the deps above.
    curr_parallel: Option<Box<ParallelGroup>>,
}

pub(crate) struct RecordedDeps {
    pub(crate) deps: SeriesParallelDeps,
    pub(crate) deps_validity: DiceValidity,
    pub(crate) invalidation_paths: TrackedInvalidationPaths,
}

impl RecordedDeps {
    fn record(
        &mut self,
        k: DiceKey,
        validity: DiceValidity,
        invalidation_paths: &TrackedInvalidationPaths,
    ) {
        self.deps.insert(k);
        self.deps_validity.and(validity);
        self.update_invalidation_paths(invalidation_paths)
    }

    #[cfg(test)]
    fn record_fresh_valid_key(&mut self, index: u32) {
        self.record(
            DiceKey { index },
            DiceValidity::Valid,
            &TrackedInvalidationPaths::clean(),
        );
    }

    fn update_invalidation_paths(&mut self, paths: &TrackedInvalidationPaths) {
        self.invalidation_paths.update(paths)
    }

    pub(crate) fn new() -> Self {
        RecordedDeps {
            deps: SeriesParallelDeps::None,
            deps_validity: DiceValidity::Valid,
            invalidation_paths: TrackedInvalidationPaths::clean(),
        }
    }

    fn insert_parallel(&mut self, parallel: Vec<RecordedDeps>) {
        let mut new_keys = 0;
        let mut new_specs = 0;

        for dep in &parallel {
            self.deps_validity.and(dep.deps_validity);
            self.invalidation_paths.update(&dep.invalidation_paths);
            let header = dep.deps.header();
            new_keys += header.keys_len();
            new_specs += header.encoded_len();
        }
        if new_keys == 0 {
            return;
        }

        self.deps
            .insert_parallel(parallel.into_iter().map(|v| v.deps), new_keys, new_specs);
    }

    #[cfg(test)]
    pub(crate) fn iter_keys(&self) -> impl Iterator<Item = DiceKey> {
        self.deps.iter_keys()
    }
}

mini_vec::size_assert::words_of_type!(RecordingDepsTracker, 5);

fn _check_deps_trackers_send_and_sync() {
    fn _assert_send_sync<T: Send + Sync>() {}
    _assert_send_sync::<RecordingDepsTracker>();
}

impl RecordingDepsTracker {
    pub(crate) fn new(invalidation_paths: TrackedInvalidationPaths) -> Self {
        let mut deps = RecordedDeps::new();
        deps.invalidation_paths = invalidation_paths;
        Self {
            deps,
            curr_parallel: None,
        }
    }

    pub(crate) fn record(
        &mut self,
        k: DiceKey,
        validity: DiceValidity,
        invalidation_paths: &TrackedInvalidationPaths,
    ) {
        self.flatten_parallel();
        self.deps.record(k, validity, invalidation_paths);
    }

    #[cfg(test)]
    fn record_fresh_valid_key(&mut self, index: u32) {
        self.record(
            DiceKey { index },
            DiceValidity::Valid,
            &TrackedInvalidationPaths::clean(),
        );
    }
    pub(crate) fn update_invalidation_paths(
        &mut self,
        invalidation_paths: &TrackedInvalidationPaths,
    ) {
        self.deps.update_invalidation_paths(&invalidation_paths);
    }

    /// Used to start a new parallel computation, storing the group that owns the parallel
    /// branches' ctxs.
    pub(crate) fn push_parallel(&mut self, group: ParallelGroup) -> &mut ParallelGroup {
        self.flatten_parallel();
        assert!(self.curr_parallel.is_none());
        self.curr_parallel.insert(Box::new(group))
    }

    pub(crate) fn collect_deps(mut self) -> RecordedDeps {
        self.flatten_parallel();
        self.deps
    }

    /// "Flattens" the previous parallel computation into deps if there is one. This should be called on any function
    /// that accesses/writes to deps.
    fn flatten_parallel(&mut self) {
        if let Some(mut parallel) = self.curr_parallel.take() {
            self.deps.insert_parallel(parallel.take_deps())
        }
    }

    pub(crate) fn invalidation_paths(&mut self) -> &TrackedInvalidationPaths {
        self.flatten_parallel();
        &self.deps.invalidation_paths
    }

    /// Tests inject parallel deps directly instead of going through real parallel ctxs.
    #[cfg(test)]
    pub(crate) fn insert_parallel_for_test(&mut self, parallel: Vec<RecordedDeps>) {
        self.flatten_parallel();
        self.deps.insert_parallel(parallel);
    }
}

#[cfg(test)]
pub(crate) mod testing {
    use crate::HashSet;
    use crate::deps::RecordingDepsTracker;
    use crate::key::DiceKey;

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

    use crate::HashSet;
    use crate::deps::RecordedDeps;
    use crate::deps::RecordingDepsTracker;
    use crate::deps::iterator::ParallelNodeIterator;
    use crate::deps::iterator::SeriesParallelDepsIteratorItem;
    use crate::key::DiceKey;
    use crate::value::DiceValidity;
    use crate::value::TrackedInvalidationPaths;
    use crate::value::testing::MakeInvalidationPaths;

    struct DisplaySPDeps<'a, T: Iterator<Item = SeriesParallelDepsIteratorItem<'a>>>(T);
    impl<'a, T: Iterator<Item = SeriesParallelDepsIteratorItem<'a>>> DisplaySPDeps<'a, T> {
        fn debug_string(self) -> String {
            SeriesNodeDisplay(self.0)
                .as_lines()
                .into_iter()
                .map(|v| v.trim().to_owned())
                .join("\n")
        }
    }

    struct SeriesNodeDisplay<'a, T: Iterator<Item = SeriesParallelDepsIteratorItem<'a>>>(T);
    impl<'a, T: Iterator<Item = SeriesParallelDepsIteratorItem<'a>>> SeriesNodeDisplay<'a, T> {
        fn as_lines(&mut self) -> Vec<String> {
            let mut lines = Vec::new();
            lines.push("S".to_owned());
            for item in self.0.by_ref() {
                lines.push("|".to_owned());
                match item {
                    SeriesParallelDepsIteratorItem::Key(k) => lines.push(format!("K({})", k.index)),
                    SeriesParallelDepsIteratorItem::Parallel(p) => {
                        lines.extend(ParallelNodeDisplay(p).as_lines())
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
        fn as_lines(&mut self) -> Vec<String> {
            let mut inner_lines = Vec::new();
            let mut inner_widths = Vec::new();
            let mut longest = 0;
            for item in self.0.by_ref() {
                let lines = SeriesNodeDisplay(item).as_lines();
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
        let mut deps_tracker = RecordingDepsTracker::new(TrackedInvalidationPaths::clean());

        deps_tracker.record(
            DiceKey { index: 2 },
            DiceValidity::Valid,
            &TrackedInvalidationPaths::clean(),
        );
        deps_tracker.record(
            DiceKey { index: 3 },
            DiceValidity::Valid,
            &TrackedInvalidationPaths::clean(),
        );

        let recorded_deps = deps_tracker.collect_deps();
        let expected = HashSet::from_iter([DiceKey { index: 2 }, DiceKey { index: 3 }]);
        let actual: HashSet<_> = recorded_deps.iter_keys().collect();
        assert_eq!(actual, expected);
        assert_eq!(recorded_deps.deps_validity, DiceValidity::Valid);

        Ok(())
    }

    #[tokio::test]
    async fn recording_deps_tracker_tracks_invalidations() -> anyhow::Result<()> {
        let mut deps_tracker = RecordingDepsTracker::new(TrackedInvalidationPaths::clean());

        deps_tracker.record(
            DiceKey { index: 2 },
            DiceValidity::Valid,
            &MakeInvalidationPaths {
                normal: (DiceKey { index: 101 }, 8),
                high: None,
            }
            .into(),
        );

        assert_eq!(
            deps_tracker.invalidation_paths(),
            &MakeInvalidationPaths {
                normal: (DiceKey { index: 101 }, 8),
                high: None,
            }
            .into()
        );

        {
            let mut s1 = RecordedDeps::new();
            s1.record(
                DiceKey { index: 11 },
                DiceValidity::Valid,
                &MakeInvalidationPaths {
                    normal: (DiceKey { index: 102 }, 6),
                    high: Some((DiceKey { index: 102 }, 6)),
                }
                .into(),
            );
            deps_tracker.insert_parallel_for_test(vec![s1]);
        }
        assert_eq!(
            deps_tracker.invalidation_paths(),
            &MakeInvalidationPaths {
                normal: (DiceKey { index: 101 }, 8),
                high: Some((DiceKey { index: 102 }, 6)),
            }
            .into()
        );

        Ok(())
    }

    #[test]
    fn recording_deps_tracker_tracks_deps_invalid() -> anyhow::Result<()> {
        let mut deps_tracker = RecordingDepsTracker::new(TrackedInvalidationPaths::clean());

        deps_tracker.record(
            DiceKey { index: 2 },
            DiceValidity::Valid,
            &TrackedInvalidationPaths::clean(),
        );
        deps_tracker.record(
            DiceKey { index: 3 },
            DiceValidity::Transient,
            &TrackedInvalidationPaths::clean(),
        );

        let recorded_deps = deps_tracker.collect_deps();
        let expected = HashSet::from_iter([DiceKey { index: 2 }, DiceKey { index: 3 }]);
        let actual: HashSet<_> = recorded_deps.iter_keys().collect();
        assert_eq!(actual, expected);
        assert_eq!(recorded_deps.deps_validity, DiceValidity::Transient);

        Ok(())
    }

    #[test]
    fn test_series_parallel_record_and_iter() -> anyhow::Result<()> {
        let mut tracker = RecordingDepsTracker::new(TrackedInvalidationPaths::clean());

        {
            let mut s1 = RecordedDeps::new();
            for i in 11..=19 {
                s1.record_fresh_valid_key(i);
            }

            let mut s2 = RecordedDeps::new();
            s2.record_fresh_valid_key(21);
            {
                let mut s3a = RecordedDeps::new();
                s3a.record_fresh_valid_key(22);
                s3a.record_fresh_valid_key(23);

                let mut s3b = RecordedDeps::new();
                s3b.record_fresh_valid_key(24);
                s3b.record_fresh_valid_key(25);

                s2.insert_parallel(vec![s3a, s3b]);
            }

            tracker.insert_parallel_for_test(vec![s1, s2]);
        }

        tracker.record_fresh_valid_key(91);
        tracker.record_fresh_valid_key(92);
        tracker.record_fresh_valid_key(93);

        {
            let mut branches = Vec::new();
            for i in 0..5 {
                let mut s = RecordedDeps::new();
                s.record_fresh_valid_key(32 + i);
                branches.push(s);
            }
            tracker.insert_parallel_for_test(branches);
        }
        tracker.record_fresh_valid_key(94);
        tracker.record_fresh_valid_key(95);

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
