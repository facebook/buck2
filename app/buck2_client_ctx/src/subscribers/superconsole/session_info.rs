/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::iter;

use buck2_event_observer::session_info::SessionInfo;
use superconsole::Component;
use superconsole::Dimensions;
use superconsole::DrawMode;
use superconsole::Line;
use superconsole::Lines;
use superconsole::Span;

/// This component is used to display session information for a command e.g. RE session ID
pub struct SessionInfoComponent<'s> {
    pub session_info: &'s SessionInfo,
}

impl<'s> Component for SessionInfoComponent<'s> {
    fn draw_unchecked(&self, dimensions: Dimensions, _mode: DrawMode) -> anyhow::Result<Lines> {
        let mut headers = Lines::new();
        let mut ids = vec![];
        if cfg!(fbcode_build) {
            headers.push(Line::unstyled("Buck UI:")?);
            ids.push(Span::new_unstyled(format!(
                "https://www.internalfb.com/buck2/{}",
                self.session_info.trace_id
            ))?);
        } else {
            headers.push(Line::unstyled("Build ID:")?);
            ids.push(Span::new_unstyled(&self.session_info.trace_id)?);
        }
        if let Some(buck2_data::TestSessionInfo { info }) = &self.session_info.test_session {
            headers.push(Line::unstyled("Test UI:")?);
            ids.push(Span::new_unstyled(info)?);
        }
        if self.session_info.legacy_dice {
            headers.push(Line::unstyled("Note:")?);
            ids.push(Span::new_unstyled(
                "Using deprecated legacy dice".to_owned(),
            )?);
        }
        // pad all headers to the max width.
        headers.justify();
        headers.pad_lines_right(1);

        let max_len = headers
            .iter()
            .zip(ids.iter())
            .map(|(header, id)| header.len() + id.len())
            .max()
            .unwrap_or(0);

        let lines = if max_len > dimensions.width {
            headers
                .into_iter()
                .zip(ids)
                .flat_map(|(header, id)| {
                    iter::once(header).chain(iter::once(Line::from_iter([id])))
                })
                .collect()
        } else {
            headers
                .iter_mut()
                .zip(ids)
                .for_each(|(header, id)| header.push(id));
            headers
        };

        let max_len = lines.iter().map(|line| line.len()).max().unwrap_or(0);

        Ok(if max_len > dimensions.width {
            Lines(vec![Line::unstyled(
                "<Terminal too small for build details>",
            )?])
        } else {
            lines
        })
    }
}
