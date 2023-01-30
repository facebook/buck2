/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_data::buck_event;
use buck2_data::InstantEvent;
use buck2_data::SpanEndEvent;
use buck2_data::SpanStartEvent;
use buck2_events::BuckEvent;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum VisitorError {
    #[error("Sent an event missing one or more fields: `{0:?}`")]
    MissingField(BuckEvent),
    #[error("Sent an unexpected Record event: `{0:?}`")]
    UnexpectedRecord(BuckEvent),
}

/// Just a simple structure that makes it easier to deal with BuckEvent rather than
/// needing to deal with the unpacking of optional fields yourself.
pub enum UnpackedBuckEvent<'a> {
    SpanStart(
        &'a BuckEvent,
        &'a SpanStartEvent,
        &'a buck2_data::span_start_event::Data,
    ),
    SpanEnd(
        &'a BuckEvent,
        &'a SpanEndEvent,
        &'a buck2_data::span_end_event::Data,
    ),
    Instant(
        &'a BuckEvent,
        &'a InstantEvent,
        &'a buck2_data::instant_event::Data,
    ),
}

pub fn unpack_event(event: &BuckEvent) -> anyhow::Result<UnpackedBuckEvent> {
    match &event.data() {
        buck_event::Data::SpanStart(v) => Ok(UnpackedBuckEvent::SpanStart(
            event,
            v,
            v.data
                .as_ref()
                .ok_or_else(|| VisitorError::MissingField(event.clone()))?,
        )),
        buck_event::Data::SpanEnd(v) => Ok(UnpackedBuckEvent::SpanEnd(
            event,
            v,
            v.data
                .as_ref()
                .ok_or_else(|| VisitorError::MissingField(event.clone()))?,
        )),
        buck_event::Data::Instant(v) => Ok(UnpackedBuckEvent::Instant(
            event,
            v,
            v.data
                .as_ref()
                .ok_or_else(|| VisitorError::MissingField((*event).clone()))?,
        )),
        buck_event::Data::Record(_) => Err(VisitorError::UnexpectedRecord(event.clone()).into()),
    }
}
