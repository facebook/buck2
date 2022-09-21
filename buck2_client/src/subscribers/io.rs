/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_core::io_counters::IoCounterKey;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Line;
use superconsole::Lines;
use superconsole::State;

use crate::subscribers::humanized_bytes::HumanizedBytes;

#[derive(Default)]
pub(crate) struct IoState {
    last: Option<buck2_data::Snapshot>,
    pub(crate) enabled: bool,
}

impl IoState {
    pub(crate) fn update(&mut self, snapshot: &buck2_data::Snapshot) {
        self.last = Some(snapshot.clone());
    }

    fn do_render(&self, snapshot: &buck2_data::Snapshot) -> anyhow::Result<Vec<Line>> {
        let mut lines = Vec::new();
        if snapshot.buck2_rss != 0 {
            lines.push(Line::from_iter([superconsole::Span::new_unstyled(
                format!("RSS = {}", HumanizedBytes(snapshot.buck2_rss)),
            )?]));
        }
        // Using a loop to make sure no key is missing.
        for key in IoCounterKey::ALL {
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
            if value != 0 {
                lines.push(Line::unstyled(&format!("{:?} = {}", key, value))?);
            }
        }
        Ok(lines)
    }

    pub(crate) fn render(&self, draw_mode: DrawMode) -> anyhow::Result<Vec<Line>> {
        if !self.enabled {
            return Ok(Vec::new());
        }
        if let DrawMode::Final = draw_mode {
            return Ok(Vec::new());
        }
        if let Some(snapshot) = &self.last {
            self.do_render(snapshot)
        } else {
            Ok(Vec::new())
        }
    }
}

#[derive(Debug)]
pub(crate) struct IoHeader;

impl Component for IoHeader {
    fn draw_unchecked(
        &self,
        state: &State,
        _dimensions: Dimensions,
        mode: DrawMode,
    ) -> anyhow::Result<Lines> {
        let io = state.get::<IoState>()?;
        io.render(mode)
    }
}
