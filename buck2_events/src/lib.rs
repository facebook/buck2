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
pub mod subscriber;

use std::fmt::Display;
use std::fmt::Formatter;
use std::hash::Hash;
use std::hash::Hasher;
use std::num::NonZeroU64;
use std::str::FromStr;
use std::sync::atomic::AtomicU64;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use std::time::SystemTime;

use async_trait::async_trait;
use byteorder::ByteOrder;
use byteorder::NetworkEndian;
use cli_proto::CommandResult;
use derive_more::Display;
use derive_more::From;
use gazebo::prelude::*;
use gazebo::variants::UnpackVariants;
use serde::Serialize;
use thiserror::Error;
use uuid::Uuid;

use crate::sink::channel::ChannelEventSink;
use crate::source::ChannelEventSource;

/// A TraceId is a unique identifier for a trace. Trace IDs are globally unique; their textual form is a v4 UUID.
///
/// TraceIds generally correspond to commands, but they do not have to, e.g. in the case of a Buck daemon producing
/// events even when a command is not running.
#[derive(Debug, Clone, Dupe, PartialEq, Eq)]
pub struct TraceId(Arc<Uuid>);

impl serde::ser::Serialize for TraceId {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.to_string())
    }
}

impl Display for TraceId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = Uuid::encode_buffer();
        f.write_str(self.0.to_hyphenated().encode_lower(&mut buf))
    }
}

impl FromStr for TraceId {
    type Err = uuid::Error;
    fn from_str(s: &str) -> Result<TraceId, Self::Err> {
        Ok(TraceId(Arc::new(Uuid::parse_str(s)?)))
    }
}

impl TraceId {
    /// Generates a new TraceId, suitable for identifying a particular trace.
    pub fn new() -> TraceId {
        TraceId(Arc::new(Uuid::new_v4()))
    }

    /// Retrieves the cached hash of this TraceId.
    pub fn hash(&self) -> i64 {
        NetworkEndian::read_i64(&self.0.as_bytes()[8..16])
    }
}

#[allow(clippy::derive_hash_xor_eq)] // The derived PartialEq is still correct.
impl Hash for TraceId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash().hash(state);
    }
}

/// A SpanId is a unique identifier for a span, which is a pair of events that represent a conceptual start and stop
/// of a particular operation.
#[derive(Debug, Copy, Clone, Dupe, Serialize, PartialEq, Eq, Hash, Display)]
pub struct SpanId(NonZeroU64);

impl SpanId {
    /// Generates a new SpanId, suitable for identifying a particular span within the context of a trace. Span IDs are
    /// increasing nonzero 64-bit integers.
    pub fn new() -> SpanId {
        static NEXT_ID: AtomicU64 = AtomicU64::new(1);
        loop {
            let next_id = NEXT_ID.fetch_add(1, Ordering::AcqRel);
            match NonZeroU64::new(next_id) {
                Some(id) => return SpanId(id),
                // 64-bit wrap around; continue the loop to generate the next non-zero ID.
                None => continue,
            }
        }
    }
}

/// An event that can be produced by Buck2. Events are points in time with additional metadata attached to them,
/// depending on the nature of the event.
///
/// Some events are special in that they represent points in time where an operation started or ended. These events
/// introduce new "spans". All events belong to a span except the first and last events of a trace. All spans except
/// the span created by the first and last events of the trace have a parent; as such, spans form a tree.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct BuckEvent {
    /// A timestamp for when this event was emitted.
    pub timestamp: SystemTime,

    /// A trace ID, globally identifying a series of events. Often corresponds to a particular command, although it
    /// does not have to.
    pub trace_id: TraceId,

    /// If this event starts a new span, the span ID assigned to this span, or None if this event is a leaf event
    /// that does not start a new span.
    pub span_id: Option<SpanId>,

    /// The ID of the span that contains this event. Will be non-None in all Events except the first and last events
    /// of a trace.
    pub parent_id: Option<SpanId>,

    /// The kind of this event, combined with the event payload.
    pub data: buck2_data::buck_event::Data,

    /// A temporary field to verify that the locally-held dispatcher is the same as the one in global state.
    /// True if global dispatcher is not set.
    pub is_global_dispatcher_diff: bool,
}

impl From<BuckEvent> for buck2_data::BuckEvent {
    fn from(e: BuckEvent) -> Self {
        buck2_data::BuckEvent {
            timestamp: Some(e.timestamp.into()),
            trace_id: e.trace_id.0.to_hyphenated().to_string(),
            span_id: e.span_id.map_or(0, |s| s.0.into()),
            parent_id: e.parent_id.map_or(0, |s| s.0.into()),
            data: Some(e.data),
            is_global_dispatcher_diff: e.is_global_dispatcher_diff,
        }
    }
}

impl TryFrom<buck2_data::BuckEvent> for BuckEvent {
    type Error = BuckEventError;

    fn try_from(
        buck2_data::BuckEvent {
            timestamp,
            trace_id,
            span_id,
            parent_id,
            data,
            is_global_dispatcher_diff,
        }: buck2_data::BuckEvent,
    ) -> Result<BuckEvent, BuckEventError> {
        fn new_span_id(num: u64) -> Option<SpanId> {
            NonZeroU64::new(num).map(SpanId)
        }
        Ok(Self {
            timestamp: SystemTime::try_from(timestamp.ok_or(BuckEventError::MissingTimestamp)?)?,
            trace_id: TraceId(Arc::new(Uuid::parse_str(&trace_id)?)),
            span_id: new_span_id(span_id),
            parent_id: new_span_id(parent_id),
            data: data.ok_or(BuckEventError::MissingData)?,
            is_global_dispatcher_diff,
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
#[derive(Error, Debug)]
pub enum BuckEventError {
    #[error("The `buck2_data::BuckEvent` provided has no `Timestamp`")]
    MissingTimestamp,
    #[error("The `buck2_data::BuckEvent` provided has no `Data`")]
    MissingData,
    #[error("The `buck2_data::BuckEvent` contains an invalid UUID")]
    InvalidUUID(#[from] uuid::Error),
    #[error("Expected BuckEvent, found Result")]
    FoundResult,
    #[error("The `buck2_data::BuckEvent` provided a timestamp out of the system range")]
    TimestampOutOfRange(#[from] prost_types::TimestampOutOfSystemRangeError),
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use buck2_data::CommandStart;
    use buck2_data::SpanStartEvent;

    use super::*;

    #[test]
    fn round_trip_success() {
        let test = BuckEvent {
            timestamp: SystemTime::now(),
            trace_id: TraceId::new(),
            span_id: Some(SpanId::new()),
            parent_id: Some(SpanId::new()),
            data: SpanStartEvent {
                data: Some(
                    CommandStart {
                        data: None,
                        metadata: HashMap::new(),
                    }
                    .into(),
                ),
            }
            .into(),
            is_global_dispatcher_diff: false,
        };
        assert_eq!(
            test,
            BuckEvent::try_from(buck2_data::BuckEvent::from(test.clone())).unwrap()
        );
    }

    #[test]
    fn trace_id_hash_produces_a_reasonable_number() {
        let trace_id = TraceId(Arc::new(
            Uuid::parse_str("0436430c-2b02-624c-2032-570501212b57").unwrap(),
        ));
        let hash = trace_id.hash();
        assert_eq!(2320012437054630743i64, hash);

        let other_trace_id = TraceId(Arc::new(
            Uuid::parse_str("586615bb-f57a-45a6-8804-3c6fcb0347de").unwrap(),
        ));
        let hash = other_trace_id.hash();
        assert_eq!(-8645718933799483426i64, hash);
    }
}
