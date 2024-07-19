/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::io_counters::IoCounterKey;
use buck2_event_observer::humanized::HumanizedBytes;
use buck2_event_observer::two_snapshots::TwoSnapshots;
use gazebo::prelude::*;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Line;
use superconsole::Lines;

use crate::subscribers::superconsole::SuperConsoleConfig;

pub(crate) struct IoHeader<'s> {
    pub(crate) super_console_config: &'s SuperConsoleConfig,
    pub(crate) two_snapshots: &'s TwoSnapshots,
}

impl<'s> Component for IoHeader<'s> {
    fn draw_unchecked(&self, dimensions: Dimensions, mode: DrawMode) -> anyhow::Result<Lines> {
        render(
            self.two_snapshots,
            mode,
            dimensions.width,
            self.super_console_config.enable_io,
        )
    }
}

/// Place space-separated words on lines.
fn words_to_lines(words: Vec<String>, width: usize) -> Vec<String> {
    let mut lines = Vec::new();
    let mut current_line = String::new();
    for word in words {
        if current_line.is_empty() {
            current_line = word;
            continue;
        }
        // This works correctly only for ASCII strings.
        if current_line.len() + 1 + word.len() > width {
            lines.push(current_line);
            current_line = word;
        } else {
            current_line.push(' ');
            current_line.push_str(&word);
        }
    }
    if !current_line.is_empty() {
        lines.push(current_line);
    }
    lines
}

pub fn io_in_flight_non_zero_counters(
    snapshot: &buck2_data::Snapshot,
) -> impl Iterator<Item = (IoCounterKey, u32)> + '_ {
    IoCounterKey::ALL
        .iter()
        .map(|key| {
            let value = match key {
                IoCounterKey::Stat => snapshot.io_in_flight_stat,
                IoCounterKey::Copy => snapshot.io_in_flight_copy,
                IoCounterKey::Symlink => snapshot.io_in_flight_symlink,
                IoCounterKey::Hardlink => snapshot.io_in_flight_hardlink,
                IoCounterKey::MkDir => snapshot.io_in_flight_mk_dir,
                IoCounterKey::ReadDir => snapshot.io_in_flight_read_dir,
                IoCounterKey::ReadDirEden => snapshot.io_in_flight_read_dir_eden,
                IoCounterKey::RmDir => snapshot.io_in_flight_rm_dir,
                IoCounterKey::RmDirAll => snapshot.io_in_flight_rm_dir_all,
                IoCounterKey::StatEden => snapshot.io_in_flight_stat_eden,
                IoCounterKey::Chmod => snapshot.io_in_flight_chmod,
                IoCounterKey::ReadLink => snapshot.io_in_flight_read_link,
                IoCounterKey::Remove => snapshot.io_in_flight_remove,
                IoCounterKey::Rename => snapshot.io_in_flight_rename,
                IoCounterKey::Read => snapshot.io_in_flight_read,
                IoCounterKey::Write => snapshot.io_in_flight_write,
                IoCounterKey::Canonicalize => snapshot.io_in_flight_canonicalize,
                IoCounterKey::EdenSettle => snapshot.io_in_flight_eden_settle,
            };
            (*key, value)
        })
        .filter(|(_, value)| *value > 0)
}

fn do_render(
    two_snapshots: &TwoSnapshots,
    snapshot: &buck2_data::Snapshot,
    width: usize,
) -> anyhow::Result<Lines> {
    let mut lines = Vec::new();
    let mut parts = Vec::new();
    if let Some(buck2_rss) = snapshot.buck2_rss {
        parts.push(format!("RSS = {}", HumanizedBytes::new(buck2_rss)));
    } else {
        // buck2_rss is only available on Linux. On other platforms, buck2 keeps track of buck2_max_rss so show that instead.
        parts.push(format!(
            "Max RSS = {}",
            HumanizedBytes::new(snapshot.buck2_max_rss)
        ));
    }

    // We prefer to display malloc_bytes_active instead of malloc_bytes_allocated
    // because it represents active pages which is more than allocated and better reflects actual memory use of buck2.
    if let Some(malloc_bytes_active) = snapshot.malloc_bytes_active {
        parts.push(format!(
            "Malloc active = {}",
            HumanizedBytes::new(malloc_bytes_active)
        ));
    }
    let user_cpu_percents = two_snapshots.user_cpu_percents();
    let system_cpu_percents = two_snapshots.system_cpu_percents();
    if user_cpu_percents.is_some() || system_cpu_percents.is_some() {
        let mut cpu_str_parts = vec!["buckd CPU".to_owned()];
        if let Some(p) = user_cpu_percents {
            cpu_str_parts.push(format!("user = {}%", p));
        }
        if let Some(p) = system_cpu_percents {
            cpu_str_parts.push(format!("system = {}%", p));
        }
        let cpu_str = cpu_str_parts.join("  ");
        parts.push(cpu_str);
    }
    if snapshot.deferred_materializer_queue_size > 0 {
        parts.push(format!(
            "DM Queue = {}",
            snapshot.deferred_materializer_queue_size
        ));
    }
    if snapshot.blocking_executor_io_queue_size > 0 {
        parts.push(format!(
            "IO Queue = {}",
            snapshot.blocking_executor_io_queue_size
        ));
    }
    if !parts.is_empty() {
        lines.push(Line::from_iter([superconsole::Span::new_unstyled(
            parts.join("  "),
        )?]));
    }

    let mut counters = Vec::new();
    for (key, value) in io_in_flight_non_zero_counters(snapshot) {
        counters.push(format!("{:?} = {}", key, value));
    }
    lines.extend(words_to_lines(counters, width).into_try_map(|s| Line::unstyled(&s))?);

    Ok(Lines(lines))
}

fn render(
    two_snapshots: &TwoSnapshots,
    draw_mode: DrawMode,
    width: usize,
    enabled: bool,
) -> anyhow::Result<Lines> {
    if !enabled {
        return Ok(Lines::new());
    }
    if let DrawMode::Final = draw_mode {
        return Ok(Lines::new());
    }
    if let Some((_, snapshot)) = &two_snapshots.last {
        do_render(two_snapshots, snapshot, width)
    } else {
        Ok(Lines::new())
    }
}

#[cfg(test)]
mod tests {
    use super::words_to_lines;

    #[test]
    fn test_words_to_lines() {
        assert_eq!(Vec::<String>::new(), words_to_lines(vec![], 5));
        assert_eq!(
            vec!["ab".to_owned()],
            words_to_lines(vec!["ab".to_owned()], 5)
        );
        assert_eq!(
            vec!["ab cd".to_owned()],
            words_to_lines(vec!["ab".to_owned(), "cd".to_owned()], 5)
        );
        assert_eq!(
            vec!["ab".to_owned(), "cd".to_owned()],
            words_to_lines(vec!["ab".to_owned(), "cd".to_owned()], 4)
        );
        assert_eq!(
            vec!["abcd".to_owned()],
            words_to_lines(vec!["abcd".to_owned()], 3)
        );
    }
}
