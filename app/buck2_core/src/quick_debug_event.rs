/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use crate::event::EVENT_DISPATCH;

/// Intended to be used sort of like a `eprintln!`, except that it goes into the event log and so is
/// easy to extract in structured form.
///
/// Must not be called in checked-in code!
///
/// For copy-pasting, command to extract all the values for your key as newline separated strings:
///
/// ```text
/// log show | rg UnstableE2eData | jq -r '.Event.data.Instant.data.UnstableE2eData | select(.key == "yourkey") | .data'
/// ```
///
/// Extract all `{key, data}` pairs with the data parsed as json:
///
/// ```text
/// log show | rg UnstableE2eData | jq -c '.Event.data.Instant.data.UnstableE2eData | .data |= fromjson'
/// ```
pub fn send(key: impl ToString, s: impl ToString) {
    EVENT_DISPATCH.get().unwrap().emit_instant_event_for_data(
        buck2_data::QuickUnstableE2eData {
            key: key.to_string(),
            data: s.to_string(),
        }
        .into(),
    );
}
