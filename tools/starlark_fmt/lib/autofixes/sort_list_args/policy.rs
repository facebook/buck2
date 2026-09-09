/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

use super::super::parsed_module::ParsedModule;
use super::super::utils::line_start;

pub(super) fn leading_comments_before(
    module: &ParsedModule,
    keyword_start: TextSize,
) -> Option<TextRange> {
    let source = module.source();
    let keyword_line_start = line_start(source, keyword_start.to_usize());
    if keyword_line_start == 0
        || !source[keyword_line_start..keyword_start.to_usize()]
            .trim()
            .is_empty()
    {
        return None;
    }

    let mut leading_start = keyword_line_start;
    while leading_start > 0 {
        let candidate_start = line_start(source, leading_start - 1);
        if !source[candidate_start..leading_start]
            .trim_start()
            .starts_with('#')
        {
            break;
        }
        leading_start = candidate_start;
    }

    (leading_start < keyword_line_start).then(|| {
        TextRange::new(
            TextSize::from(leading_start as u32),
            TextSize::from(keyword_line_start as u32),
        )
    })
}
