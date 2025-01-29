/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;

use buck2_event_observer::what_ran::emit_what_ran_entry;
use buck2_event_observer::what_ran::CommandReproducer;
use buck2_event_observer::what_ran::WhatRanOptions;
use buck2_event_observer::what_ran::WhatRanOptionsRegex;
use buck2_event_observer::what_ran::WhatRanOutputWriter;
use buck2_event_observer::what_ran::WhatRanState;
use buck2_events::span::SpanId;
use derive_more::From;
use dupe::Dupe;

/// Presented with an event and its containing span, emit it to the output if it's relevant. The
/// state is used to associate the parent with something meaningful. This does not take the parent
/// directly because *most* events are *not* relevant so we save the lookup in that case.
pub(crate) fn emit_event_if_relevant(
    parent_span_id: OptionalSpanId,
    data: &buck2_data::buck_event::Data,
    state: &impl WhatRanState,
    output: &mut impl WhatRanOutputWriter,
) -> buck2_error::Result<()> {
    let options = WhatRanOptions::default();
    let options_regex = WhatRanOptionsRegex::from_options(&options)?;
    if let Some(repro) = CommandReproducer::from_buck_data(data, options_regex.options) {
        // Find and format the parent span (if any), then emit the relevant command.
        let action = parent_span_id.0.and_then(|id| state.get(id));

        emit_what_ran_entry(action.as_ref(), repro, output, &options_regex, None, None)?;
    }

    Ok(())
}

/// A wrapper type to make calls to emit_event_if_relevant more convenient, since parent_id is
/// `Option<SpanId>` on BuckEvent.
#[derive(From, Copy, Clone, Dupe, Eq, PartialEq, Hash)]
pub(crate) struct OptionalSpanId(pub Option<SpanId>);

impl fmt::Display for OptionalSpanId {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        if let Some(this) = self.0 {
            write!(formatter, "{}", this)
        } else {
            write!(formatter, "(none)")
        }
    }
}
