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
    buck_event, instant_event::Data::HgInfo, span_end_event, span_start_event, MercurialInfo,
    SpanEndEvent, SpanStartEvent,
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

    // TODO(mufeezamjad): remove once all uses of this function are changed to use the global event dispatcher.
    //
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
        self.instant_event(HgInfo(MercurialInfo {
            commit: hash,
            diff: pastry,
        }));
    }

    // TODO(mufeezamjad): remove once all uses of this function are changed to use the global event dispatcher.
    //
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

    // TODO(mufeezamjad): remove once all uses of this function are changed to use the global event dispatcher.
    //
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

tokio::task_local! {
    pub static EVENTS: EventDispatcher;
}

/// Invokes function func, setting the dispatcher to the task_local for the duration
/// of the call (and for any downstream events).
pub fn with_dispatcher<R, F>(dispatcher: EventDispatcher, func: F) -> R
where
    F: FnOnce() -> R,
{
    EVENTS.sync_scope(dispatcher, func)
}

/// Resolves the Future fut, setting the dispatcher to the task_local for the Task
/// (and for any downstream events).
pub async fn with_dispatcher_async<R, Fut>(dispatcher: EventDispatcher, fut: Fut) -> R
where
    Fut: Future<Output = R>,
{
    futures::pin_mut!(fut);
    EVENTS.scope(dispatcher, fut).await
}

fn get_dispatcher() -> EventDispatcher {
    EVENTS.with(|dispatcher| dispatcher.dupe())
}

fn current_span() -> Option<SpanId> {
    CURRENT_SPAN.with(|tl_span| tl_span.get())
}

/// Introduces a new span and immediately fires the given start event. When the given synchronous function returns,
/// the span is closed and the end event emitted.
pub fn span<Start, End, F, R>(start: Start, func: F) -> R
where
    Start: Into<span_start_event::Data>,
    End: Into<span_end_event::Data>,
    F: FnOnce() -> (R, End),
{
    let events = get_dispatcher();

    let mut span = Span::start(&events, start);
    let (result, end) = span.call_in_span(func);
    span.end(end);
    result
}

/// Emits an InstantEvent annotated with the current trace ID
pub fn instant_event<E: Into<buck2_data::instant_event::Data>>(data: E) {
    let events = get_dispatcher();

    let instant = buck2_data::InstantEvent {
        data: Some(data.into()),
    };
    events.event_with_span_id(instant, None, current_span())
}

/// Introduces a new span and immediately fires the given start event. When the given future resolves,  the span is
/// closed and the event is emitted. This span is a "suspending span"; it is intended to suspend and resume whenever
/// the future itself is suspended and resumed, respectively.
///
/// TODO(swgillespie) actually implement suspend/resume
pub async fn span_async<Start, End, Fut, R>(start: Start, fut: Fut) -> R
where
    Start: Into<span_start_event::Data>,
    End: Into<span_end_event::Data>,
    Fut: Future<Output = (R, End)>,
{
    let events = get_dispatcher();

    let mut span = Span::start(&events, start);

    futures::pin_mut!(fut);
    let (result, end) = future::poll_fn(|cx| span.call_in_span(|| fut.as_mut().poll(cx))).await;

    span.end(end);
    result
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
    use std::result::Result::{Err, Ok};

    use buck2_data::{CommandEnd, CommandStart, SpanStartEvent};
    use tokio::task::JoinHandle;

    use super::{EventDispatcher, *};
    use crate::{sink::channel::ChannelEventSink, source::ChannelEventSource, EventSource};

    async fn next_event(source: &mut ChannelEventSource) -> BuckEvent {
        source.receive().unwrap().unpack_buck().unwrap().clone()
    }

    fn create_dispatcher() -> (EventDispatcher, ChannelEventSource, TraceId) {
        let (send, recv) = crossbeam_channel::unbounded();
        let source = ChannelEventSource::new(recv);

        let sink = ChannelEventSink::new(send);
        let trace_id = TraceId::new();
        let dispatcher = EventDispatcher::new(trace_id.dupe(), sink);

        (dispatcher, source, trace_id)
    }

    fn create_start_end_events() -> (CommandStart, CommandEnd) {
        let start = CommandStart {
            data: Default::default(),
            metadata: Default::default(),
        };
        let end = CommandEnd {
            data: Default::default(),
            metadata: Default::default(),
            is_success: true,
            error_messages: vec![],
        };

        (start, end)
    }

    #[tokio::test]
    async fn send_event_smoke() {
        let (dispatcher, mut source, trace_id) = create_dispatcher();
        let (start, _) = create_start_end_events();

        dispatcher.event(SpanStartEvent {
            data: Some(start.into()),
        });

        let event = next_event(&mut source).await;
        assert_eq!(event.trace_id, trace_id);
        assert!(event.parent_id.is_none());
        assert!(event.span_id.is_none());
    }

    #[tokio::test]
    async fn send_event_with_span() {
        let (dispatcher, mut source, trace_id) = create_dispatcher();
        let (start, end) = create_start_end_events();

        dispatcher.span(start, || ((), end));

        let event = next_event(&mut source).await;
        let span_id = event.span_id.unwrap();
        assert_eq!(event.trace_id, trace_id);
        assert!(event.parent_id.is_none());

        let end = next_event(&mut source).await;
        assert_eq!(end.span_id.unwrap(), span_id);
    }

    #[tokio::test]
    async fn send_event_with_span_async() {
        let (dispatcher, mut source, trace_id) = create_dispatcher();
        let (start, end) = create_start_end_events();

        dispatcher.span_async(start, async { ((), end) }).await;

        let event = next_event(&mut source).await;
        let span_id = event.span_id.unwrap();
        assert_eq!(event.trace_id, trace_id);
        assert!(event.parent_id.is_none());

        let end = next_event(&mut source).await;
        assert_eq!(end.span_id.unwrap(), span_id);
    }

    #[tokio::test]
    async fn send_event_with_nested_span() {
        let (dispatcher, mut source, _) = create_dispatcher();
        let (start, end) = create_start_end_events();

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
        let (dispatcher, mut source, _) = create_dispatcher();
        let (start, end) = create_start_end_events();

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
        let (dispatcher, _, _) = create_dispatcher();
        let (start, _) = create_start_end_events();

        let mut span = Span::start(&dispatcher, start);
        span.call_in_span(|| std::thread::sleep(Duration::from_millis(10)));
        assert_eq!(span.stats.max_poll_time, span.stats.total_poll_time);

        span.call_in_span(|| std::thread::sleep(Duration::from_millis(10)));
        assert!(span.stats.max_poll_time < span.stats.total_poll_time);
    }

    async fn dispatcher_task(
        dispatcher: EventDispatcher,
        start: CommandStart,
        end: CommandEnd,
    ) -> Result<u32, ()> {
        let mut err = false;
        let mut events = 0u32;

        async fn event(
            start: CommandStart,
            end: CommandEnd,
            events: &mut u32,
            trace_id: &TraceId,
            err: &mut bool,
        ) {
            async fn yield_and_check(trace_id: &TraceId, err: &mut bool) {
                tokio::task::yield_now().await;
                *err |= get_dispatcher().trace_id != *trace_id;
            }

            // 2*2 for Start and End events, for 2 spans
            *events += 4;

            span_async(start.clone(), async {
                for _ in 0..4 {
                    yield_and_check(trace_id, err).await;
                }
                // TODO(mufeez): re-consider adding recursive calls to event
                span_async(start.clone(), async {
                    for _ in 0..4 {
                        yield_and_check(trace_id, err).await;
                    }
                    ((), end.clone())
                })
                .await;
                ((), end.clone())
            })
            .await
        }

        with_dispatcher_async(
            dispatcher.dupe(),
            event(
                start.clone(),
                end.clone(),
                &mut events,
                &dispatcher.trace_id,
                &mut err,
            ),
        )
        .await;

        if err { Err(()) } else { Ok(events) }
    }

    // Test function used in test_concurrent_single_thread and
    // test_concurrent_multi_thread.
    async fn test_concurrent() {
        let (events1, source1, trace_id1) = create_dispatcher();
        let (events2, source2, trace_id2) = create_dispatcher();

        let mut event_sources: [ChannelEventSource; 2] = [source1, source2];
        let trace_ids: [TraceId; 2] = [trace_id1, trace_id2];

        let (start1, end1) = create_start_end_events();
        let (start2, end2) = create_start_end_events();

        let handles: [JoinHandle<Result<u32, ()>>; 2] = [
            tokio::spawn(dispatcher_task(events1, start1, end1)),
            tokio::spawn(dispatcher_task(events2, start2, end2)),
        ];

        let mut sent_counts = Vec::new();

        for handle in handles {
            match handle.await {
                Ok(result) => {
                    sent_counts.push(result.unwrap());
                }
                Err(e) => assert!(!e.is_panic()),
            }
        }

        for (i, sent) in sent_counts.iter().enumerate() {
            for _ in 0..*sent {
                let e = next_event(&mut event_sources[i]).await;
                assert!(e.trace_id == trace_ids[i]);
            }
        }
    }

    #[tokio::test]
    async fn test_concurrent_single_thread() {
        test_concurrent().await;
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn test_concurrent_multi_thread() {
        test_concurrent().await;
    }
}
