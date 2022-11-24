/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

#![cfg_attr(feature = "gazebo_lint", feature(plugin))]
#![cfg_attr(feature = "gazebo_lint", allow(deprecated))] // :(
#![cfg_attr(feature = "gazebo_lint", plugin(gazebo_lint))]
#![feature(box_syntax)]

//!
//! Events and event streams for Buck2.
//!
//! The `Event` enum is the set of events that Buck2 can produce. Events can be produced both during the course of
//! a command or through background operation (such as DICE invalidations of changed files).
//!
//! There are three critical nouns in this data model:
//!  * An **event**, which is a structure representing a point-in-time occurrence, with some additional data.
//!  * A **trace**, which is a collection of events that are to be interpreted as semantically linked. Traces are
//!    globally identified by a trace ID, which is a v4 UUID. Traces are often associated with individual commands,
//!    although they do not have to.
//!  * A **span**, which is a pair of two events that represent a start and stop pair. A span covers a range of time
//!    points. All events are parented to a span that was currently active at the location the event was emitted.

pub mod dispatch;
pub mod metadata;
pub mod sink;
pub mod source;
pub mod span;
pub mod trace;

use std::num::NonZeroU64;
use std::time::SystemTime;

use anyhow::Context;
use async_trait::async_trait;
use cli_proto::CommandResult;
use derive_more::From;
use gazebo::variants::UnpackVariants;
use serde::Serialize;
use thiserror::Error;
use uuid::Uuid;

use crate::sink::channel::ChannelEventSink;
use crate::source::ChannelEventSource;
use crate::span::SpanId;
use crate::trace::TraceId;

/// An event that can be produced by Buck2. Events are points in time with additional metadata attached to them,
/// depending on the nature of the event.
///
/// Some events are special in that they represent points in time where an operation started or ended. These events
/// introduce new "spans". All events belong to a span except the first and last events of a trace. All spans except
/// the span created by the first and last events of the trace have a parent; as such, spans form a tree.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BuckEvent {
    /// Full event, the rest of the fields are caches.
    event: buck2_data::BuckEvent,

    /// A timestamp for when this event was emitted.
    timestamp: SystemTime,

    /// If this event starts a new span, the span ID assigned to this span, or None if this event is a leaf event
    /// that does not start a new span.
    span_id: Option<SpanId>,

    /// The ID of the span that contains this event. Will be non-None in all Events except the first and last events
    /// of a trace.
    parent_id: Option<SpanId>,
}

impl BuckEvent {
    pub fn new(
        timestamp: SystemTime,
        trace_id: TraceId,
        span_id: Option<SpanId>,
        parent_id: Option<SpanId>,
        data: buck2_data::buck_event::Data,
    ) -> BuckEvent {
        let event = buck2_data::BuckEvent {
            timestamp: Some(timestamp.into()),
            trace_id: trace_id.0.to_hyphenated().to_string(),
            span_id: span_id.map_or(0, |s| s.0.into()),
            parent_id: parent_id.map_or(0, |s| s.0.into()),
            data: Some(data),
        };
        BuckEvent {
            event,
            timestamp,
            span_id,
            parent_id,
        }
    }

    pub fn timestamp(&self) -> SystemTime {
        self.timestamp
    }

    pub fn trace_id(&self) -> anyhow::Result<TraceId> {
        Ok(TraceId(Uuid::parse_str(&self.event.trace_id)?))
    }

    pub fn span_id(&self) -> Option<SpanId> {
        self.span_id
    }

    pub fn parent_id(&self) -> Option<SpanId> {
        self.parent_id
    }

    pub fn event(&self) -> &buck2_data::BuckEvent {
        &self.event
    }

    pub fn data(&self) -> &buck2_data::buck_event::Data {
        self.event
            .data
            .as_ref()
            .expect("data is set, it is validated")
    }

    pub fn data_mut(&mut self) -> &mut buck2_data::buck_event::Data {
        self.event
            .data
            .as_mut()
            .expect("data is set, it is validated")
    }

    pub fn span_start_event(&self) -> Option<&buck2_data::SpanStartEvent> {
        match self.data() {
            buck2_data::buck_event::Data::SpanStart(start) => Some(start),
            _ => None,
        }
    }

    pub fn span_end_event(&self) -> Option<&buck2_data::SpanEndEvent> {
        match self.data() {
            buck2_data::buck_event::Data::SpanEnd(end) => Some(end),
            _ => None,
        }
    }

    pub fn command_start(&self) -> anyhow::Result<Option<&buck2_data::CommandStart>> {
        match self.span_start_event() {
            None => Ok(None),
            Some(span_start_event) => {
                match span_start_event
                    .data
                    .as_ref()
                    .with_context(|| BuckEventError::MissingField(self.clone()))?
                {
                    buck2_data::span_start_event::Data::Command(command_start) => {
                        Ok(Some(command_start))
                    }
                    _ => Ok(None),
                }
            }
        }
    }

    fn instant_event(&self) -> Option<&buck2_data::InstantEvent> {
        match self.data() {
            buck2_data::buck_event::Data::Instant(instant) => Some(instant),
            _ => None,
        }
    }

    fn instant_event_data(&self) -> anyhow::Result<Option<&buck2_data::instant_event::Data>> {
        match self.instant_event() {
            None => Ok(None),
            Some(instant) => {
                Ok(Some(instant.data.as_ref().with_context(|| {
                    BuckEventError::MissingField(self.clone())
                })?))
            }
        }
    }

    pub fn raw_output(&self) -> anyhow::Result<Option<&str>> {
        match self.instant_event_data()? {
            Some(buck2_data::instant_event::Data::RawOutput(raw_output)) => {
                Ok(Some(&raw_output.raw_output))
            }
            _ => Ok(None),
        }
    }
}

impl From<BuckEvent> for buck2_data::BuckEvent {
    fn from(e: BuckEvent) -> Self {
        e.event
    }
}

impl TryFrom<buck2_data::BuckEvent> for BuckEvent {
    type Error = anyhow::Error;

    fn try_from(event: buck2_data::BuckEvent) -> anyhow::Result<BuckEvent> {
        event.data.as_ref().ok_or(BuckEventError::MissingData)?;
        fn new_span_id(num: u64) -> Option<SpanId> {
            NonZeroU64::new(num).map(SpanId)
        }
        Ok(Self {
            timestamp: SystemTime::try_from(
                event
                    .timestamp
                    .clone()
                    .ok_or(BuckEventError::MissingTimestamp)?,
            )?,
            span_id: new_span_id(event.span_id),
            parent_id: new_span_id(event.parent_id),
            event,
        })
    }
}

/// An event that can be produced by Buck2 that is not intended to be presented to the user, but rather is used to
/// communicate with other parts of Buck2.
#[derive(Clone, From)]
pub enum ControlEvent {
    /// A command result, produced upon completion of a command.
    CommandResult(CommandResult),
}

/// The set of events that can flow out of an EventSource. Control events are not intended to be sent across the gRPC
/// boundary, while Buck events are.
#[derive(Clone, From, UnpackVariants)]
#[allow(clippy::large_enum_variant)]
pub enum Event {
    /// A control event: events that are not to be exposed across gRPC, but inform stream consumers of important control
    /// changes, such the CommandResult event, which implies that there will be no further events.
    Control(ControlEvent),
    /// A buck event: events that are to be exposed across gRPC verbatim.
    Buck(BuckEvent),
}

/// A sink for events, easily plumbable to the guts of systems that intend to produce events consumeable by
/// higher-level clients. Sending an event is synchronous.
pub trait EventSink: Send + Sync {
    /// Sends an event into this sink, to be consumed elsewhere. Explicitly does not return a Result type; if sending
    /// an event does fail, implementations will handle the failure by panicking or performing some other graceful
    /// recovery; callers of EventSink are not expected to handle failures.
    fn send(&self, event: BuckEvent);

    /// Sends a control event into this sink, to be consumed elsewhere. Control events are not sent to gRPC clients.
    fn send_control(&self, control_event: ControlEvent);
}

/// A source for events, suitable for reading a stream of events coming out of Buck.
#[async_trait]
pub trait EventSource: Send {
    fn receive(&mut self) -> Option<Event>;
}

/// Creates a pair of an EventSource and an EventSink such that writes to the sink can be read by the event source.
pub fn create_source_sink_pair() -> (impl EventSource, impl EventSink) {
    let (send, recv) = crossbeam_channel::unbounded();
    let sink = ChannelEventSink::new(send);
    let source = ChannelEventSource::new(recv);
    (source, sink)
}

#[allow(clippy::large_enum_variant)]
#[derive(Error, Debug)]
enum BuckEventError {
    #[error("The `buck2_data::BuckEvent` provided has no `Timestamp`")]
    MissingTimestamp,
    #[error("The `buck2_data::BuckEvent` provided has no `Data`")]
    MissingData,
    #[error("Sent an event missing one or more fields: `{0:?}`")]
    MissingField(BuckEvent),
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use buck2_data::CommandStart;
    use buck2_data::SpanStartEvent;

    use super::*;

    #[test]
    fn round_trip_success() {
        let test = BuckEvent::new(
            SystemTime::now(),
            TraceId::new(),
            Some(SpanId::new()),
            Some(SpanId::new()),
            SpanStartEvent {
                data: Some(
                    CommandStart {
                        data: None,
                        metadata: HashMap::new(),
                    }
                    .into(),
                ),
            }
            .into(),
        );
        assert_eq!(
            test,
            BuckEvent::try_from(buck2_data::BuckEvent::from(test.clone())).unwrap()
        );
    }

    #[test]
    fn trace_id_hash_produces_a_reasonable_number() {
        let trace_id = TraceId(Uuid::parse_str("0436430c-2b02-624c-2032-570501212b57").unwrap());
        let hash = trace_id.hash();
        assert_eq!(2320012437054630743i64, hash);

        let other_trace_id =
            TraceId(Uuid::parse_str("586615bb-f57a-45a6-8804-3c6fcb0347de").unwrap());
        let hash = other_trace_id.hash();
        assert_eq!(-8645718933799483426i64, hash);
    }
}
