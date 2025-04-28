/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::io;
use std::path::Path;

use crate::extract_from_outputs;
use crate::runtime::FishRuntime;

fn reconstruct_with_beginning_omitted(out: String, last_input_word: &str) -> String {
    let Some(end) = out.strip_prefix('…') else {
        return out;
    };

    for (i, _) in end.char_indices().rev() {
        if let Some(omited) = last_input_word.strip_suffix(&end[0..i]) {
            return format!("{omited}{end}");
        }
    }
    // Unreachable because of `i == 0` case
    unreachable!()
}

pub(crate) fn run_fish(
    completion_name: &str,
    script: &str,
    input: &str,
    tempdir: &Path,
) -> io::Result<Vec<String>> {
    let home = tempdir;

    let mut r = FishRuntime::new(home.to_owned())?;
    r.register(completion_name, script)?;

    let outs = extract_from_outputs(
        input,
        std::iter::empty()
            .chain(std::iter::once_with(|| r.complete(&format!("{}\t", input))))
            .chain(std::iter::once_with(|| {
                r.complete(&format!("{}\t\t", input))
            })),
    )?;

    let last_input_word = input
        .rsplit_once(|c: char| c.is_ascii_whitespace())
        .map_or(input, |x| x.1);

    Ok(outs
        .into_iter()
        .map(|out| reconstruct_with_beginning_omitted(out, last_input_word))
        .collect())
}
