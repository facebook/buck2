/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use superconsole::Component;

use crate::subscribers::re_panel::RePanel;

/// Draw the test summary line above the `timed_list`
#[derive(Debug)]
pub struct ReHeader;

impl ReHeader {
    pub fn boxed() -> Box<dyn Component> {
        box Self
    }
}

impl Component for ReHeader {
    fn draw_unchecked(
        &self,
        state: &superconsole::State,
        _dimensions: superconsole::Dimensions,
        mode: superconsole::DrawMode,
    ) -> anyhow::Result<superconsole::Lines> {
        let re = state.get::<RePanel>()?;
        re.render(mode)
    }
}
