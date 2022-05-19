//! Mechanism for dispatching events. Where the rubber meets the road, for producers of events.
//!
//! The [`EventDispatcher`] is a type-erased, dupe-able container for a [`crate::EventSink`]. It is intended to be
//! liberally duplicated and passed around to the depths of buck2 so that consumers can insert events into it.

use std::{
    cell::Cell,
    process::{Command, Stdio},
    sync::Arc,
    time::{Duration, Instant, SystemTime},
};

use buck2_data::{
    buck_event, instant_event::Data::HgInfo, span_end_event, span_start_event, InstantEvent,
    MercurialInfo, SpanEndEvent, SpanStartEvent,
};
use futures::{future, Future};
use gazebo::prelude::*;

use crate::{sink::null::NullEventSink, BuckEvent, ControlEvent, EventSink, SpanId, TraceId};

/// A type-erased and dupe-able container for EventSinks, containing some additional metadata useful for all events
/// emitted through the dispatcher.
#[derive(Clone, Dupe)]
pub struct EventDispatcher {
    /// The Trace ID of the current trace.
    trace_id: TraceId,
    /// The sink to log events to.
    sink: Arc<dyn EventSink>,
}

impl EventDispatcher {
    /// Creates a new Event Dispatcher using the given TraceId and logging events to the given sink.
    pub fn new<T: EventSink + 'static>(trace_id: TraceId, sink: T) -> EventDispatcher {
        EventDispatcher {
            trace_id,
            sink: Arc::new(sink),
        }
    }

    /// Creates a new null Event Dispatcher that accepts events but does not write them anywhere.
    pub fn null() -> EventDispatcher {
        EventDispatcher {
            trace_id: TraceId::new(),
            sink: Arc::new(NullEventSink::new()),
        }
    }

    /// Emits an event annotated with the current trace ID.
    pub fn event<E: Into<buck_event::Data>>(&self, data: E) {
        self.event_with_span_id(data, None, current_span());
    }

    /// Emits an InstantEvent annotated with the current trace ID
    pub fn instant_event<E: Into<buck2_data::instant_event::Data>>(&self, data: E) {
        let instant = buck2_data::InstantEvent {
            data: Some(data.into()),
        };
        self.event_with_span_id(instant, None, current_span());
    }

    fn event_with_span_id<E: Into<buck_event::Data>>(
        &self,
        data: E,
        span_id: Option<SpanId>,
        parent_id: Option<SpanId>,
    ) {
        let now = SystemTime::now();
        let event = BuckEvent {
            timestamp: now,
            trace_id: self.trace_id.dupe(),
            span_id,
            parent_id,
            data: data.into(),
        };
        self.sink.send(event);
    }

    pub fn control_event<E: Into<ControlEvent>>(&self, data: E) {
        self.sink.send_control(data.into());
    }

    pub fn info(&self, msg: &str, filepath: &str, lineno: u32, col: u32) {
        self.log(buck2_data::log::Level::Info, msg, filepath, lineno, col);
    }

    pub fn warn(&self, msg: &str, filepath: &str, lineno: u32, col: u32) {
        self.log(buck2_data::log::Level::Warn, msg, filepath, lineno, col);
    }

    pub fn error(&self, msg: &str, filepath: &str, lineno: u32, col: u32) {
        self.log(buck2_data::log::Level::Error, msg, filepath, lineno, col);
    }

    fn log(&self, level: buck2_data::log::Level, msg: &str, filepath: &str, lineno: u32, col: u32) {
        self.instant_event(buck2_data::Log {
            level: level as i32,
            message: msg.to_owned(),
            location: Some(buck2_data::Location {
                file: filepath.to_owned(),
                line: lineno,
                column: col,
            }),
        });
    }

    // Logs mercurial data
    pub async fn instant_hg(&self) {
        // TODO use tokio/tokio::process::Command instead of command (see D29824148)
        let committed = Command::new("hg").arg("status").arg("-mard").output();
        let log_changes = if let Ok(status) = committed {
            !status.stdout.is_empty()
        } else {
            return;
        };

        // TODO use `hg debughiddencommit` instead of `hg id`, `hg diff`, and `pastry`
        let hash = if let Ok(commit) = Command::new("hg")
            .arg("--debug")
            .arg("id")
            .arg("-i")
            .output()
        {
            String::from_utf8_lossy(&commit.stdout).to_string()
        } else {
            return;
        };
        let mut pastry = "".to_owned();
        if log_changes {
            if let Ok(diff) = Command::new("hg")
                .arg("diff")
                .arg("-r")
                .arg("master")
                .stdout(Stdio::piped())
                .spawn()
            {
                if let Some(d) = diff.stdout {
                    if let Ok(paste) = Command::new("pastry").stdin(d).output() {
                        pastry = String::from_utf8_lossy(&paste.stdout).to_string();
                    }
                }
            }
        }
        self.event(InstantEvent {
            data: {
                Some(HgInfo(MercurialInfo {
                    commit: hash,
                    diff: pastry,
                }))
            },
        });
    }

    /// Introduces a new span and immediately fires the given start event. When the given synchronous function returns,
    /// the span is closed and the end event emitted.
    pub fn span<Start, End, F, R>(&self, start: Start, func: F) -> R
    where
        Start: Into<span_start_event::Data>,
        End: Into<span_end_event::Data>,
        F: FnOnce() -> (R, End),
    {
        let mut span = Span::start(self, start);
        let (result, end) = span.call_in_span(func);
        span.end(end);
        result
    }

    /// Introduces a new span and immediately fires the given start event. When the given future resolves,  the span is
    /// closed and the event is emitted. This span is a "suspending span"; it is intended to suspend and resume whenever
    /// the future itself is suspended and resumed, respectively.
    ///
    /// TODO(swgillespie) actually implement suspend/resume
    pub async fn span_async<Start, End, Fut, R>(&self, start: Start, fut: Fut) -> R
    where
        Start: Into<span_start_event::Data>,
        End: Into<span_end_event::Data>,
        Fut: Future<Output = (R, End)>,
    {
        let mut span = Span::start(self, start);

        futures::pin_mut!(fut);
        let (result, end) = future::poll_fn(|cx| span.call_in_span(|| fut.as_mut().poll(cx))).await;

        span.end(end);
        result
    }

    /// Returns the traceid for this event dispatcher.
    pub fn trace_id(&self) -> &TraceId {
        &self.trace_id
    }
}

#[derive(Default)]
struct SpanStats {
    max_poll_time: Duration,
    total_poll_time: Duration,
}

/// Utility to track the start & end of a span
#[must_use = "You must end a Span you started"]
struct Span<'a> {
    start_instant: Instant,
    dispatcher: &'a EventDispatcher,
    span_id: SpanId,
    stats: SpanStats,
    parent_id: Option<SpanId>,
    event_data: Option<span_end_event::Data>,
}

impl<'a> Span<'a> {
    fn start<D>(dispatcher: &'a EventDispatcher, data: D) -> Self
    where
        D: Into<span_start_event::Data>,
    {
        let parent_id = current_span();
        let span_id = SpanId::new();
        let start_instant = Instant::now();

        dispatcher.event_with_span_id(
            SpanStartEvent {
                data: Some(data.into()),
            },
            Some(span_id),
            parent_id,
        );

        Self {
            start_instant,
            dispatcher,
            span_id,
            parent_id,
            stats: Default::default(),
            event_data: None,
        }
    }

    fn end<D>(mut self, data: D)
    where
        D: Into<span_end_event::Data>,
    {
        self.event_data = Some(data.into());
        drop(self);
    }

    fn call_in_span<T, F>(&mut self, f: F) -> T
    where
        F: FnOnce() -> T,
    {
        let now = Instant::now();

        let ret = CURRENT_SPAN.with(|tl_span| {
            let previous_span = tl_span.replace(Some(self.span_id));
            let ret = f();
            tl_span.set(previous_span);
            ret
        });

        let elapsed = now.elapsed();
        self.stats.max_poll_time = std::cmp::max(elapsed, self.stats.max_poll_time);
        self.stats.total_poll_time += elapsed;

        ret
    }
}

impl<'a> Drop for Span<'a> {
    /// When dropping a Span, send a SpanEndEvent. We do this in Drop to ensure that even if we
    /// never `end()` a Span, we notify clients (if any exist).
    fn drop(&mut self) {
        let data = self
            .event_data
            .take()
            .unwrap_or_else(|| buck2_data::SpanCancelled {}.into());

        self.dispatcher.event_with_span_id(
            SpanEndEvent {
                duration: Some(self.start_instant.elapsed().into()),
                data: Some(data),
                stats: Some(buck2_data::SpanStats {
                    max_poll_time_us: self.stats.max_poll_time.as_micros() as u64,
                    total_poll_time_us: self.stats.total_poll_time.as_micros() as u64,
                }),
            },
            Some(self.span_id),
            self.parent_id,
        );
    }
}

thread_local! {
    pub static CURRENT_SPAN: Cell<Option<SpanId>> = Cell::new(None);
}

fn current_span() -> Option<SpanId> {
    CURRENT_SPAN.with(|tl_span| tl_span.get())
}

#[macro_export]
macro_rules! info {
    ( $dispatcher:expr, $fmt_str:literal ) => {
        $dispatcher.info($fmt_str, file!(), line!(), column!());
    };

    ( $dispatcher:expr, $fmt_str:literal, $( $arg:tt ),* ) => {
        let msg: String = format!($fmt_str, $($arg),*);
        $dispatcher.info(&msg, file!(), line!(), column!());
    };
}

#[macro_export]
macro_rules! warn {
    ( $dispatcher:expr, $fmt_str:literal ) => {
        $dispatcher.warn($fmt_str, file!(), line!(), column!());
    };

    ( $dispatcher:expr, $fmt_str:literal, $( $arg:tt ),* ) => {
        let msg: String = format!($fmt_str, $($arg),*);
        $dispatcher.warn(&msg, file!(), line!(), column!());
    };
}

#[macro_export]
macro_rules! error {
    ( $dispatcher:expr, $fmt_str:literal ) => {
        $dispatcher.error($fmt_str, file!(), line!(), column!());
    };

    ( $dispatcher:expr, $fmt_str:literal, $( $arg:tt ),* ) => {
        let msg: String = format!($fmt_str, $($arg),*);
        $dispatcher.error(&msg, file!(), line!(), column!());
    };
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use buck2_data::{CommandStart, SpanStartEvent};

    use super::{EventDispatcher, *};
    use crate::{sink::channel::ChannelEventSink, source::ChannelEventSource, EventSource};

    async fn next_event(source: &mut ChannelEventSource) -> BuckEvent {
        source.receive().unwrap().unpack_buck().unwrap().clone()
    }

    #[tokio::test]
    async fn send_event_smoke() {
        let (send, recv) = crossbeam_channel::unbounded();
        let sink = ChannelEventSink::new(send);
        let mut source = ChannelEventSource::new(recv);
        let trace_id = TraceId::new();
        let dispatcher = EventDispatcher::new(trace_id.dupe(), sink);
        dispatcher.event(SpanStartEvent {
            data: Some(
                CommandStart {
                    data: None,
                    metadata: HashMap::new(),
                }
                .into(),
            ),
        });
        let event = next_event(&mut source).await;
        assert_eq!(event.trace_id, trace_id);
        assert!(event.parent_id.is_none());
        assert!(event.span_id.is_none());
    }

    #[tokio::test]
    async fn send_event_with_span() {
        let (send, recv) = crossbeam_channel::unbounded();
        let sink = ChannelEventSink::new(send);
        let mut source = ChannelEventSource::new(recv);
        let trace_id = TraceId::new();
        let dispatcher = EventDispatcher::new(trace_id.dupe(), sink);

        let start = buck2_data::CommandStart {
            data: None,
            metadata: HashMap::new(),
        };
        dispatcher.span(start, || {
            (
                (),
                buck2_data::CommandEnd {
                    data: None,
                    metadata: HashMap::new(),
                    is_success: true,
                    error_messages: vec![],
                },
            )
        });

        let event = next_event(&mut source).await;
        let span_id = event.span_id.unwrap();
        assert_eq!(event.trace_id, trace_id);
        assert!(event.parent_id.is_none());

        let end = next_event(&mut source).await;
        assert_eq!(end.span_id.unwrap(), span_id);
    }

    #[tokio::test]
    async fn send_event_with_span_async() {
        let (send, recv) = crossbeam_channel::unbounded();
        let sink = ChannelEventSink::new(send);
        let mut source = ChannelEventSource::new(recv);
        let trace_id = TraceId::new();
        let dispatcher = EventDispatcher::new(trace_id.dupe(), sink);

        let start = buck2_data::CommandStart {
            data: None,
            metadata: HashMap::new(),
        };
        dispatcher
            .span_async(start, async {
                (
                    (),
                    buck2_data::CommandEnd {
                        data: None,
                        metadata: HashMap::new(),
                        is_success: true,
                        error_messages: vec![],
                    },
                )
            })
            .await;

        let event = next_event(&mut source).await;
        let span_id = event.span_id.unwrap();
        assert_eq!(event.trace_id, trace_id);
        assert!(event.parent_id.is_none());

        let end = next_event(&mut source).await;
        assert_eq!(end.span_id.unwrap(), span_id);
    }

    #[tokio::test]
    async fn send_event_with_nested_span() {
        let (send, recv) = crossbeam_channel::unbounded();
        let sink = ChannelEventSink::new(send);
        let mut source = ChannelEventSource::new(recv);
        let trace_id = TraceId::new();
        let dispatcher = EventDispatcher::new(trace_id.dupe(), sink);

        let start = buck2_data::CommandStart {
            data: Default::default(),
            metadata: Default::default(),
        };

        let end = buck2_data::CommandEnd {
            data: Default::default(),
            metadata: Default::default(),
            is_success: true,
            error_messages: vec![],
        };

        dispatcher.span(start.clone(), || {
            dispatcher.span(start.clone(), || ((), end.clone()));
            ((), end.clone())
        });

        let e1 = next_event(&mut source).await;
        let e2 = next_event(&mut source).await;
        let e3 = next_event(&mut source).await;
        let e4 = next_event(&mut source).await;

        assert_eq!(e1.span_id, e4.span_id);
        assert_eq!(e2.span_id, e3.span_id);
        assert_eq!(e2.parent_id, e1.span_id);
    }

    #[tokio::test]
    async fn send_event_with_nested_span_async() {
        let (send, recv) = crossbeam_channel::unbounded();
        let sink = ChannelEventSink::new(send);
        let mut source = ChannelEventSource::new(recv);
        let trace_id = TraceId::new();
        let dispatcher = EventDispatcher::new(trace_id.dupe(), sink);

        let start = buck2_data::CommandStart {
            data: Default::default(),
            metadata: Default::default(),
        };

        let end = buck2_data::CommandEnd {
            data: Default::default(),
            metadata: Default::default(),
            is_success: true,
            error_messages: vec![],
        };

        dispatcher
            .span_async(start.clone(), async {
                dispatcher
                    .span_async(start.clone(), async { ((), end.clone()) })
                    .await;
                ((), end.clone())
            })
            .await;

        let e1 = next_event(&mut source).await;
        let e2 = next_event(&mut source).await;
        let e3 = next_event(&mut source).await;
        let e4 = next_event(&mut source).await;

        assert_eq!(e1.span_id, e4.span_id);
        assert_eq!(e2.span_id, e3.span_id);
        assert_eq!(e2.parent_id, e1.span_id);
    }

    #[test]
    fn test_span_stats() {
        let (send, recv) = crossbeam_channel::unbounded();
        let sink = ChannelEventSink::new(send);
        let _source = ChannelEventSource::new(recv);
        let trace_id = TraceId::new();
        let dispatcher = EventDispatcher::new(trace_id.dupe(), sink);

        let start = buck2_data::CommandStart {
            data: Default::default(),
            metadata: Default::default(),
        };

        let mut span = Span::start(&dispatcher, start);
        span.call_in_span(|| std::thread::sleep(Duration::from_millis(10)));
        assert_eq!(span.stats.max_poll_time, span.stats.total_poll_time);

        span.call_in_span(|| std::thread::sleep(Duration::from_millis(10)));
        assert!(span.stats.max_poll_time < span.stats.total_poll_time);
    }
}
