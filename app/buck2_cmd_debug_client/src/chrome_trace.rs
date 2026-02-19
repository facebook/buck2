/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

// TraceEvent spec used in this file documented here: https://docs.google.com/document/d/1CvAClvFfyA5R-PhYUmn5OOQtYMH4h6I0nSsKchNAySU/preview?tab=t.0
// Note: "rendering" centric stuff like cname colors are not supported: https://github.com/google/perfetto/issues/208, we'd have to switch to the protobuf API

use std::borrow::Cow;
use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::io::BufWriter;
use std::io::Write;
use std::sync::Arc;
use std::time::Duration;
use std::time::SystemTime;

use buck2_client_ctx::client_ctx::BuckSubcommand;
use buck2_client_ctx::client_ctx::ClientCommandContext;
use buck2_client_ctx::common::BuckArgMatches;
use buck2_client_ctx::event_log_options::EventLogOptions;
use buck2_client_ctx::events_ctx::EventsCtx;
use buck2_client_ctx::exit_result::ExitResult;
use buck2_client_ctx::path_arg::PathArg;
use buck2_common::convert::ProstDurationExt;
use buck2_error::BuckErrorContext;
use buck2_error::buck2_error;
use buck2_error::internal_error;
use buck2_event_log::read::EventLogPathBuf;
use buck2_event_log::stream_value::StreamValue;
use buck2_event_log::utils::Invocation;
use buck2_event_observer::display;
use buck2_event_observer::display::CriticalPathEntryDisplay;
use buck2_event_observer::display::TargetDisplayOptions;
use buck2_event_observer::unpack_event::UnpackedBuckEvent;
use buck2_event_observer::unpack_event::unpack_event;
use buck2_events::BuckEvent;
use buck2_fs::paths::abs_path::AbsPathBuf;
use derive_more::Display;
use dupe::Dupe;
use futures::TryStreamExt;
use futures::stream::BoxStream;
use serde::Serialize;
use serde_json::json;

#[derive(Debug, clap::Parser)]
pub struct ChromeTraceCommand {
    #[clap(
        long,
        help = "Where to write the chrome trace JSON. If a directory is passed, the filename of the event log will be used as a base filename."
    )]
    pub trace_path: PathArg,

    /// The path to read the event log from.
    #[clap(
        long,
        help = "A path to an event-log file to read from. Only works for log files with a single command in them. If no event-log is passed, the most recent one will be used.",
        value_name = "PATH",
        // Hide because `event_log` below subsumes this.
        hide = true
    )]
    pub path: Option<PathArg>,

    #[clap(
        long,
        help = "Places a global instant event at the specified time. Floating point seconds and integer nanoseconds unixtime is accepted.",
        value_name = "TIMESTAMP DESCRIPTION",
        number_of_values = 2
    )]
    instant: Vec<String>,

    #[clap(
        long,
        help = "How many tracks to render events into",
        value_name = "MAX_TRACKS"
    )]
    pub max_tracks: Option<u64>,

    #[clap(flatten)]
    pub(crate) event_log: EventLogOptions,
}

struct ChromeTraceFirstPass {
    /// Track assignment needs to know, when it sees a SpanStart, whether that
    /// span is going to be included in the final trace.
    /// But some spans need to be filtered based on later events, like:
    ///
    /// 1. We shouldn't assign tracks to StartLoad events whose SpanEnd records
    ///    a really short duration.
    ///
    /// 2. We shouldn't assign tracks to ActionExecutionStart events who have
    ///    no child LocalStage spans.
    ///
    /// 3. (eventually) We should assign tracks to ActionExecutionStart events
    ///    only if they appear in the CriticalPath, but the CriticalPath is one
    ///    of the last events.
    ///
    /// So this first pass builds up several lists of "interesting" span IDs.
    pub long_analyses: HashSet<buck2_events::span::SpanId>,
    pub long_loads: HashSet<buck2_events::span::SpanId>,
    pub long_load_packages: HashSet<buck2_events::span::SpanId>,
    pub local_actions: HashSet<buck2_events::span::SpanId>,
    pub critical_path_action_keys: HashSet<buck2_data::ActionKey>,
    pub critical_path_span_ids: HashSet<u64>,
    pub command_start: SystemTime,
    pub command_options: Option<buck2_data::CommandOptions>,
}

impl ChromeTraceFirstPass {
    const LONG_ANALYSIS_CUTOFF: Duration = Duration::from_millis(50);
    const LONG_LOAD_CUTOFF: Duration = Duration::from_millis(50);
    const LONG_LOAD_PACKAGE_CUTOFF: Duration = Duration::from_millis(50);
    fn new() -> Self {
        Self {
            long_analyses: HashSet::new(),
            long_loads: HashSet::new(),
            long_load_packages: HashSet::new(),
            local_actions: HashSet::new(),
            critical_path_action_keys: HashSet::new(),
            critical_path_span_ids: HashSet::new(),
            command_start: SystemTime::UNIX_EPOCH,
            command_options: None,
        }
    }

    fn handle_event(&mut self, event: &BuckEvent) -> buck2_error::Result<()> {
        match event.data() {
            buck2_data::buck_event::Data::SpanStart(start) => {
                if let Some(buck2_data::span_start_event::Data::Command(..)) = start.data.as_ref() {
                    self.command_start = event.timestamp();
                } else if let Some(buck2_data::span_start_event::Data::ExecutorStage(exec)) =
                    start.data.as_ref()
                {
                    // A local stage means that we want to show the entire action execution.
                    use buck2_data::executor_stage_start::Stage;

                    if let Some(Stage::Local(local)) = &exec.stage {
                        use buck2_data::local_stage::Stage;

                        let local_execution = match local.stage.as_ref() {
                            Some(Stage::Queued(..)) => false,
                            Some(Stage::Execute(..)) => true,
                            Some(Stage::MaterializeInputs(..)) => false,
                            Some(Stage::PrepareOutputs(..)) => false,
                            Some(Stage::AcquireLocalResource(..)) => false,
                            Some(Stage::WorkerInit(..)) => false,
                            Some(Stage::WorkerExecute(..)) => true,
                            Some(Stage::WorkerQueued(..)) => false,
                            Some(Stage::WorkerWait(..)) => false,
                            None => false,
                        };

                        if local_execution {
                            self.local_actions.insert(event.parent_id().unwrap());
                        }
                    }
                }
            }
            buck2_data::buck_event::Data::SpanEnd(end) => {
                if let Some(buck2_data::span_end_event::Data::Analysis(_)) = end.data.as_ref() {
                    if end
                        .duration
                        .as_ref()
                        .expect("Analysis SpanEnd missing duration")
                        .try_into_duration()?
                        > Self::LONG_ANALYSIS_CUTOFF
                    {
                        self.long_analyses.insert(event.span_id().unwrap());
                    }
                } else if let Some(buck2_data::span_end_event::Data::Load(_)) = end.data.as_ref() {
                    if end
                        .duration
                        .as_ref()
                        .expect("Load SpanEnd missing duration")
                        .try_into_duration()?
                        > Self::LONG_LOAD_CUTOFF
                    {
                        self.long_loads.insert(event.span_id().unwrap());
                    }
                } else if let Some(buck2_data::span_end_event::Data::LoadPackage(_)) =
                    end.data.as_ref()
                {
                    if end
                        .duration
                        .as_ref()
                        .expect("LoadPackage SpanEnd missing duration")
                        .try_into_duration()?
                        > Self::LONG_LOAD_PACKAGE_CUTOFF
                    {
                        self.long_load_packages.insert(event.span_id().unwrap());
                    }
                }
            }
            buck2_data::buck_event::Data::Instant(instant) => {
                if let Some(buck2_data::instant_event::Data::BuildGraphInfo(info)) =
                    instant.data.as_ref()
                {
                    self.critical_path_span_ids = info
                        .critical_path2
                        .iter()
                        .flat_map(|entry| entry.span_ids.iter().copied())
                        .collect()
                } else if let Some(buck2_data::instant_event::Data::CommandOptions(options)) =
                    instant.data.as_ref()
                {
                    self.command_options = Some(*options);
                }
            }
            buck2_data::buck_event::Data::Record(_) => {}
        };
        Ok(())
    }
}

enum SpanTrackAssignment {
    Owned(TrackId),
    Inherited(TrackId),
}

impl SpanTrackAssignment {
    fn get_track_id(&self) -> TrackId {
        match self {
            Self::Owned(tid) => *tid,
            Self::Inherited(tid) => *tid,
        }
    }
}

#[allow(dead_code)] // Process and Thread aren't used at this time, but are included for completeness.
enum ChromeTraceInstantScope {
    Global,
    Process(u64),
    Thread(u64, SpanTrackAssignment),
}
struct ChromeTraceInstant {
    name: String,
    timestamp: SystemTime,
    scope: ChromeTraceInstantScope,
    args: Option<serde_json::Value>,
}

impl ChromeTraceInstant {
    fn to_json(self) -> buck2_error::Result<serde_json::Value> {
        let mut js = json!(
            {
                "name": self.name,
                "ts": self.timestamp.duration_since(SystemTime::UNIX_EPOCH)?.as_micros() as u64,
                "ph": "i", // Chrome trace "instant event"
                "s": match self.scope {
                    ChromeTraceInstantScope::Global => "g",
                    ChromeTraceInstantScope::Process(_) => "p",
                    ChromeTraceInstantScope::Thread(_, _) => "t",
                },
            }
        );
        let obj = js
            .as_object_mut()
            .ok_or(buck2_error::internal_error!("expected a mutable object"))?;

        if let Some(args) = self.args {
            obj.insert("args".to_owned(), json!(args));
        }

        match self.scope {
            ChromeTraceInstantScope::Global => {}
            ChromeTraceInstantScope::Process(process_id) => {
                obj.insert("pid".to_owned(), json!(process_id));
            }
            ChromeTraceInstantScope::Thread(process_id, track) => {
                obj.insert("pid".to_owned(), json!(process_id));
                obj.insert("tid".to_owned(), json!(String::from(track.get_track_id())));
            }
        }

        Ok(js)
    }
}

struct ChromeTraceOpenSpan {
    name: String,
    start: SystemTime,
    process_id: u64,
    track: SpanTrackAssignment,
    categories: Vec<&'static str>,
    // Any misc. per-event unstructured data.
    args: serde_json::Value,
}

struct ChromeTraceClosedSpan {
    open: ChromeTraceOpenSpan,
    duration: Duration,
}

impl ChromeTraceClosedSpan {
    fn to_json(self) -> buck2_error::Result<serde_json::Value> {
        Ok(json!(
            {
                "name": self.open.name,
                "ts": self.open.start.duration_since(SystemTime::UNIX_EPOCH)?.as_micros() as u64,
                "dur": self.duration.as_micros() as u64,
                "ph": "X", // Chrome trace "complete event"
                "pid": self.open.process_id,
                "tid": String::from(self.open.track.get_track_id()),
                "cat": self.open.categories.join(","),
                "args": self.open.args,
            }
        ))
    }
}

/// Spans are directed to a category, like "critical-path" or "misc". Spans in a
/// category that would overlap are put on different tracks within that category.
#[derive(Clone, Copy, Dupe)]
struct TrackId(SpanCategorization, u64);

impl From<TrackId> for String {
    fn from(tid: TrackId) -> String {
        // Outputs like "misc-00", "misc-01", ...
        format!("{}-{:02}", tid.0, tid.1)
    }
}

struct TrackIdAllocator {
    unused_track_ids: BTreeSet<u64>,
    // Used to extend |unused_track_ids| when it's empty.
    lowest_never_used: u64,
}

impl TrackIdAllocator {
    pub fn new() -> Self {
        Self {
            unused_track_ids: BTreeSet::new(),
            lowest_never_used: 0,
        }
    }

    /// Assign a track, unless we'd have > max tracks, in which case do nothing.
    fn assign_track(&mut self, max: Option<u64>) -> Option<u64> {
        let maybe_smallest = self.unused_track_ids.iter().next().copied();
        if let Some(n) = maybe_smallest {
            if let Some(max) = max {
                if max < n {
                    return None;
                }
            }

            self.unused_track_ids.remove(&n);
            Some(n)
        } else {
            let n = self.lowest_never_used;
            if let Some(max) = max {
                if max < n {
                    return None;
                }
            }
            self.lowest_never_used += 1;
            Some(n)
        }
    }

    pub fn mark_unused(&mut self, tid: u64) {
        self.unused_track_ids.insert(tid);
    }
}

struct SimpleCounters<T> {
    name: &'static str,
    // timeseries are flushed every BUCKET_DURATION, if any changed.
    next_flush: SystemTime,
    /// Stores the current value of each timeseries.
    /// Set to None when we output a zero, so we can save a bit of filesize
    /// by omitting them from the JSON output.
    counters: HashMap<String, SimpleCounter<T>>,
    zero_value: T,
    trace_events: Vec<serde_json::Value>,
}

struct SimpleCounter<T> {
    value: T,
    /// Whether this counter is currently represented in the trace as implicitly zero by not being
    /// emitted.
    implicitly_zero: bool,
}

impl<T> SimpleCounters<T>
where
    T: std::ops::SubAssign
        + std::cmp::PartialEq
        + std::ops::AddAssign
        + std::marker::Copy
        + Serialize,
{
    const BUCKET_DURATION: Duration = Duration::from_millis(100);
    pub fn new(name: &'static str, zero_value: T) -> Self {
        Self {
            name,
            next_flush: SystemTime::UNIX_EPOCH,
            counters: HashMap::new(),
            trace_events: vec![],
            zero_value,
        }
    }

    /// Process the given timestamp and flush if needed and update next_flush accordingly
    fn process_timestamp(&mut self, timestamp: SystemTime) -> buck2_error::Result<()> {
        if self.next_flush == SystemTime::UNIX_EPOCH {
            self.next_flush = timestamp + Self::BUCKET_DURATION;
        }
        if timestamp > self.next_flush + Self::BUCKET_DURATION {
            self.flush()?;
            self.next_flush = timestamp - Duration::from_micros(1);
        }
        Ok(())
    }

    /// If the given key is new to the map, initialize it to self.zero_value;
    fn counter_entry(&mut self, key: &str) -> &mut SimpleCounter<T> {
        self.counters
            .entry(key.to_owned())
            .or_insert_with(|| SimpleCounter {
                value: self.zero_value,
                implicitly_zero: false,
            })
    }

    fn set(&mut self, timestamp: SystemTime, key: &str, amount: T) -> buck2_error::Result<()> {
        self.process_timestamp(timestamp)?;
        let entry = self.counter_entry(key);
        entry.value = amount;
        Ok(())
    }

    fn bump(&mut self, timestamp: SystemTime, key: &str, amount: T) -> buck2_error::Result<()> {
        self.process_timestamp(timestamp)?;
        let entry = self.counter_entry(key);
        entry.value += amount;
        Ok(())
    }

    fn subtract(&mut self, timestamp: SystemTime, key: &str, amount: T) -> buck2_error::Result<()> {
        self.process_timestamp(timestamp)?;
        let entry = self.counter_entry(key);
        entry.value -= amount;
        Ok(())
    }

    fn flush(&mut self) -> buck2_error::Result<()> {
        // Output size optimization: omit counters that were previously, and still are, zero.
        let mut counters_to_zero = Vec::new();
        let mut counters_to_output = json!({});

        for (key, counter) in self.counters.iter_mut() {
            // TODO: With float counters this equality comparison seems sketchy.
            if counter.value == self.zero_value {
                // If the counter is currently at its zero value, then emit the zero once, and then
                // stop emitting this counter altogether.
                if !counter.implicitly_zero {
                    counters_to_output[key] = json!(counter.value);
                    counter.implicitly_zero = true;
                }
            } else {
                // If the counter isn't zero, then we want to avoid the renderer interpolating from
                // its last zero value, if any. So, if the counter was previously "zeroed" by not
                // emitting it we'll emit an extra event setting it to zero.
                if counter.implicitly_zero {
                    counter.implicitly_zero = false;
                    counters_to_zero.push(key.clone());
                }

                counters_to_output[key] = json!(counter.value);
            }
        }

        let ts = self
            .next_flush
            .duration_since(SystemTime::UNIX_EPOCH)?
            .as_micros() as u64;

        if !counters_to_zero.is_empty() {
            let counters_to_zero = counters_to_zero
                .into_iter()
                .map(|k| (k, json!(0)))
                .collect::<serde_json::Map<_, _>>();

            self.trace_events.push(json!(
                {
                    "name": self.name,
                    "pid": 0,
                    "tid": "counters",
                    "ph": "C",
                    "ts": ts - 1,
                    "args": counters_to_zero,
                }
            ));
        }

        self.trace_events.push(json!(
            {
                "name": self.name,
                "pid": 0,
                "tid": "counters",
                "ph": "C",
                "ts": ts,
                "args": counters_to_output,
            }
        ));
        self.next_flush += Self::BUCKET_DURATION;
        Ok(())
    }

    pub fn flush_all_to(&mut self, output: &mut Vec<serde_json::Value>) -> buck2_error::Result<()> {
        self.flush()?;
        output.append(&mut self.trace_events);
        Ok(())
    }
}

struct TimestampAndAmount {
    timestamp: SystemTime,
    amount: u64,
}

struct AverageRateOfChangeCounters {
    counters: SimpleCounters<u64>,
    previous_timestamp_and_amount_by_key: HashMap<String, TimestampAndAmount>,
}

impl AverageRateOfChangeCounters {
    pub fn new(name: &'static str) -> Self {
        Self {
            previous_timestamp_and_amount_by_key: HashMap::new(),
            counters: SimpleCounters::<u64>::new(name, 0),
        }
    }

    fn set_average_rate_of_change_per_s(
        &mut self,
        timestamp: SystemTime,
        key: &str,
        amount: u64,
    ) -> buck2_error::Result<()> {
        // We only plot if there exists a previous item to compute the rate of change off of
        if let Some(previous) = self.previous_timestamp_and_amount_by_key.get(key) {
            let secs_since_last_datapoint =
                timestamp.duration_since(previous.timestamp)?.as_secs_f64();
            let value_change_since_last_datapoint = (amount - previous.amount) as f64;
            if secs_since_last_datapoint > 0.0 {
                self.counters.set(
                    timestamp,
                    key,
                    (value_change_since_last_datapoint / secs_since_last_datapoint) as u64,
                )?;
            }
        }
        self.previous_timestamp_and_amount_by_key
            .insert(key.to_owned(), TimestampAndAmount { timestamp, amount });

        Ok(())
    }
}

struct SpanCounters {
    counter: SimpleCounters<i32>,
    // Stores how current open spans contribute to counter values.
    open_spans: HashMap<buck2_events::span::SpanId, (&'static str, i32)>,
}

impl SpanCounters {
    pub fn new(name: &'static str) -> Self {
        Self {
            counter: SimpleCounters::new(name, 0),
            open_spans: HashMap::new(),
        }
    }

    fn bump_counter_while_span(
        &mut self,
        event: &BuckEvent,
        key: &'static str,
        amount: i32,
    ) -> buck2_error::Result<()> {
        self.open_spans
            .insert(event.span_id().unwrap(), (key, amount));
        self.counter.bump(event.timestamp(), key, amount)
    }

    fn handle_event_end(
        &mut self,
        _end: &buck2_data::SpanEndEvent,
        event: &BuckEvent,
    ) -> buck2_error::Result<()> {
        if let Some((key, value)) = self.open_spans.remove(&event.span_id().unwrap()) {
            self.counter.subtract(event.timestamp(), key, value)?;
        }
        Ok(())
    }
}

struct ChromeTraceWriter {
    trace_events: Vec<serde_json::Value>,
    open_spans: HashMap<buck2_events::span::SpanId, ChromeTraceOpenSpan>,
    invocation: Invocation,
    first_pass: ChromeTraceFirstPass,
    max_tracks: u64,
    span_counters: SpanCounters,
    unused_track_ids: HashMap<SpanCategorization, TrackIdAllocator>,
    // Wrappers to contain values from InstantEvent.Data.Snapshot as a timeseries
    snapshot_counters: SimpleCounters<u64>,
    process_memory_counters: SimpleCounters<f64>,
    rate_of_change_counters: AverageRateOfChangeCounters,
}

#[derive(Copy, Clone, Dupe, Debug, Display, Hash, PartialEq, Eq)]
enum SpanCategorization {
    #[display("uncategorized")]
    Uncategorized,
    #[display("critical-path")]
    CriticalPath,
    #[display("detailed-critical-path")]
    DetailedCriticalPath,
    #[display("detailed-slowest-path")]
    DetailedSlowestPath,
}

impl ChromeTraceWriter {
    const BYTES_PER_GIGABYTE: f64 = 1000000000.0;

    pub fn new(invocation: Invocation, first_pass: ChromeTraceFirstPass, max_tracks: u64) -> Self {
        Self {
            trace_events: vec![],
            open_spans: HashMap::new(),
            invocation,
            first_pass,
            max_tracks,
            unused_track_ids: HashMap::new(),
            span_counters: SpanCounters::new("spans"),
            snapshot_counters: SimpleCounters::<u64>::new("snapshot_counters", 0),
            process_memory_counters: SimpleCounters::<f64>::new("process_memory", 0.0),
            rate_of_change_counters: AverageRateOfChangeCounters::new("rate_of_change_counters"),
        }
    }

    fn assign_track_for_span(
        &mut self,
        track_key: SpanCategorization,
        event: &BuckEvent,
    ) -> buck2_error::Result<Option<SpanTrackAssignment>> {
        let parent_track_id = event.parent_id().and_then(|parent_id| {
            self.open_spans
                .get(&parent_id)
                .map(|open_span| open_span.track.get_track_id())
        });

        match parent_track_id {
            None => {
                let max = match track_key {
                    SpanCategorization::CriticalPath => None,
                    SpanCategorization::Uncategorized => Some(self.max_tracks),
                    SpanCategorization::DetailedCriticalPath => None,
                    SpanCategorization::DetailedSlowestPath => None,
                };

                let track = self
                    .unused_track_ids
                    .entry(track_key)
                    .or_insert_with(TrackIdAllocator::new)
                    .assign_track(max);

                let assignment =
                    track.map(|track| SpanTrackAssignment::Owned(TrackId(track_key, track)));

                Ok(assignment)
            }
            Some(track_id) => Ok(Some(SpanTrackAssignment::Inherited(track_id))),
        }
    }

    pub fn to_writer<W>(mut self, file: W) -> buck2_error::Result<()>
    where
        W: Write,
    {
        self.span_counters
            .counter
            .flush_all_to(&mut self.trace_events)?;
        self.snapshot_counters
            .flush_all_to(&mut self.trace_events)?;
        self.process_memory_counters
            .flush_all_to(&mut self.trace_events)?;
        self.rate_of_change_counters
            .counters
            .flush_all_to(&mut self.trace_events)?;

        serde_json::to_writer(
            file,
            &json!({
                "traceEvents": self.trace_events
            }),
        )?;
        Ok(())
    }

    fn open_span(
        &mut self,
        event: &BuckEvent,
        span: ChromeTraceOpenSpan,
    ) -> buck2_error::Result<()> {
        self.open_spans.insert(event.span_id().unwrap(), span);
        Ok(())
    }

    fn open_named_span(
        &mut self,
        event: &BuckEvent,
        name: String,
        track_key: SpanCategorization,
    ) -> buck2_error::Result<()> {
        // Allocate this span to its parent's track or to a new track.
        let track = self.assign_track_for_span(track_key, event)?;
        if let Some(track) = track {
            self.open_span(
                event,
                ChromeTraceOpenSpan {
                    name,
                    start: event.timestamp(),
                    process_id: 0,
                    track,
                    categories: vec!["buck2"],
                    args: json!({
                        "span_id": event.span_id(),
                    }),
                },
            )?;
        }

        Ok(())
    }

    fn handle_event(&mut self, event: &Arc<BuckEvent>) -> buck2_error::Result<()> {
        match event.data() {
            buck2_data::buck_event::Data::SpanStart(buck2_data::SpanStartEvent {
                data: Some(start_data),
            }) => {
                let on_critical_path = event.span_id().is_some_and(|span_id| {
                    self.first_pass
                        .critical_path_span_ids
                        .contains(&span_id.into())
                });

                enum Categorization<'a> {
                    /// Show this node on a specific track
                    Show {
                        category: SpanCategorization,
                        name: Cow<'a, str>,
                    },
                    /// Show this node if its parent is being shown.
                    ShowIfParent { name: Cow<'a, str> },
                    /// Do not show this node.
                    Omit,
                }

                let categorization = match start_data {
                    buck2_data::span_start_event::Data::Command(_command) => Categorization::Show {
                        category: SpanCategorization::Uncategorized,
                        name: self.invocation.command_line_args.join(" ").into(),
                    },
                    buck2_data::span_start_event::Data::Analysis(analysis) => {
                        self.span_counters
                            .bump_counter_while_span(event, "analysis", 1)?;

                        let category = if on_critical_path {
                            Some(SpanCategorization::CriticalPath)
                        } else if self
                            .first_pass
                            .long_analyses
                            .contains(&event.span_id().unwrap())
                        {
                            Some(SpanCategorization::Uncategorized)
                        } else {
                            None
                        };

                        match category {
                            Some(category) => {
                                let name = format!(
                                    "analysis {}",
                                    display::display_analysis_target(
                                        analysis
                                            .target
                                            .as_ref()
                                            .expect("AnalysisStart event missing 'target' field"),
                                        TargetDisplayOptions::for_chrome_trace()
                                    )?,
                                );

                                Categorization::Show {
                                    category,
                                    name: name.into(),
                                }
                            }
                            None => Categorization::Omit,
                        }
                    }
                    buck2_data::span_start_event::Data::Load(eval) => {
                        self.span_counters
                            .bump_counter_while_span(event, "load", 1)?;

                        let category = if on_critical_path {
                            Some(SpanCategorization::CriticalPath)
                        } else if self
                            .first_pass
                            .long_loads
                            .contains(&event.span_id().unwrap())
                        {
                            Some(SpanCategorization::Uncategorized)
                        } else {
                            None
                        };

                        match category {
                            Some(category) => Categorization::Show {
                                category,
                                name: format!("load {}", eval.module_id).into(),
                            },
                            None => Categorization::Omit,
                        }
                    }
                    buck2_data::span_start_event::Data::LoadPackage(load_package) => {
                        let category = if on_critical_path {
                            Some(SpanCategorization::CriticalPath)
                        } else if self
                            .first_pass
                            .long_load_packages
                            .contains(&event.span_id().unwrap())
                        {
                            Some(SpanCategorization::Uncategorized)
                        } else {
                            None
                        };

                        match category {
                            Some(category) => Categorization::Show {
                                category,
                                name: format!("listing {}", load_package.path).into(),
                            },
                            None => Categorization::Omit,
                        }
                    }
                    buck2_data::span_start_event::Data::ActionExecution(action) => {
                        #[allow(clippy::if_same_then_else)]
                        let category = if self
                            .first_pass
                            .critical_path_action_keys
                            .contains(action.key.as_ref().unwrap())
                        {
                            Some(SpanCategorization::CriticalPath)
                        } else if on_critical_path {
                            Some(SpanCategorization::CriticalPath)
                        } else if self
                            .first_pass
                            .local_actions
                            .contains(&event.span_id().unwrap())
                        {
                            Some(SpanCategorization::Uncategorized)
                        } else {
                            None
                        };

                        match category {
                            Some(category) => {
                                let name = display::display_action_identity(
                                    action.key.as_ref(),
                                    action.name.as_ref(),
                                    TargetDisplayOptions::for_chrome_trace(),
                                )?;

                                Categorization::Show {
                                    category,
                                    name: name.into(),
                                }
                            }
                            None => Categorization::Omit,
                        }
                    }
                    buck2_data::span_start_event::Data::ExecutorStage(stage) => {
                        let name = stage
                            .stage
                            .as_ref()
                            .and_then(display::display_executor_stage);

                        match name {
                            Some(name) => {
                                self.span_counters.bump_counter_while_span(event, name, 1)?;
                                Categorization::ShowIfParent { name: name.into() }
                            }
                            None => Categorization::Omit,
                        }
                    }
                    buck2_data::span_start_event::Data::ReUpload(_) => {
                        let name = "re_upload";
                        self.span_counters.bump_counter_while_span(event, name, 1)?;
                        Categorization::ShowIfParent { name: name.into() }
                    }
                    buck2_data::span_start_event::Data::FinalMaterialization(..) => {
                        if on_critical_path {
                            Categorization::Show {
                                category: SpanCategorization::CriticalPath,
                                name: "materialization".into(),
                            }
                        } else {
                            Categorization::Omit
                        }
                    }
                    buck2_data::span_start_event::Data::FileWatcher(_file_watcher) => {
                        Categorization::Show {
                            category: SpanCategorization::CriticalPath,
                            name: "file_watcher_sync".into(),
                        }
                    }
                    _ if on_critical_path => Categorization::Show {
                        category: SpanCategorization::CriticalPath,
                        name: "<unknown>".into(),
                    },
                    _ => Categorization::Omit,
                };

                match categorization {
                    Categorization::Show { category, name } => {
                        self.open_named_span(event, name.into_owned(), category)?;
                    }
                    Categorization::ShowIfParent { name } => {
                        let parent_is_open = event
                            .parent_id()
                            .is_some_and(|id| self.open_spans.contains_key(&id));

                        if parent_is_open {
                            // Inherit the parent's track.
                            self.open_named_span(
                                event,
                                name.into_owned(),
                                SpanCategorization::Uncategorized,
                            )?;
                        }
                    }

                    Categorization::Omit => {}
                }
            }
            // Data field is oneof and `None` means the event is produced with newer version of `.proto` file
            // which added a variant which is not available in version used when compiling this program.
            buck2_data::buck_event::Data::SpanStart(buck2_data::SpanStartEvent { data: None }) => {}
            buck2_data::buck_event::Data::SpanEnd(end) => self.handle_event_end(end, event)?,
            buck2_data::buck_event::Data::Instant(buck2_data::InstantEvent {
                data: Some(instant_data),
            }) => match instant_data {
                buck2_data::instant_event::Data::Snapshot(snapshot) => {
                    self.process_memory_counters.set(
                        event.timestamp(),
                        "max_rss_gigabyte",
                        (snapshot.buck2_max_rss) as f64 / Self::BYTES_PER_GIGABYTE,
                    )?;
                    if let Some(malloc_bytes_active) = snapshot.malloc_bytes_active {
                        self.process_memory_counters.set(
                            event.timestamp(),
                            "malloc_active_gigabyte",
                            (malloc_bytes_active) as f64 / Self::BYTES_PER_GIGABYTE,
                        )?;
                    }
                    self.rate_of_change_counters
                        .set_average_rate_of_change_per_s(
                            event.timestamp(),
                            "average_user_cpu_in_usecs_per_s",
                            snapshot.buck2_user_cpu_us,
                        )?;
                    self.rate_of_change_counters
                        .set_average_rate_of_change_per_s(
                            event.timestamp(),
                            "average_system_cpu_in_usecs_per_s",
                            snapshot.buck2_system_cpu_us,
                        )?;
                    if let Some(cpu_usage_system) = snapshot.host_cpu_usage_system_ms {
                        self.rate_of_change_counters
                            .set_average_rate_of_change_per_s(
                                event.timestamp(),
                                "host_cpu_usage_system_in_msecs_per_s",
                                cpu_usage_system,
                            )?;
                    }
                    if let Some(cpu_usage_user) = snapshot.host_cpu_usage_user_ms {
                        self.rate_of_change_counters
                            .set_average_rate_of_change_per_s(
                                event.timestamp(),
                                "host_cpu_usage_user_in_msecs_per_s",
                                cpu_usage_user,
                            )?;
                    }
                    self.snapshot_counters.set(
                        event.timestamp(),
                        "deferred_materializer_queue_size",
                        snapshot.deferred_materializer_queue_size,
                    )?;
                    self.snapshot_counters.set(
                        event.timestamp(),
                        "blocking_executor_io_queue_size",
                        snapshot.blocking_executor_io_queue_size,
                    )?;
                    self.snapshot_counters.set(
                        event.timestamp(),
                        "tokio_blocking_queue_depth",
                        snapshot.tokio_blocking_queue_depth,
                    )?;
                    self.snapshot_counters.set(
                        event.timestamp(),
                        "tokio_num_blocking_threads",
                        snapshot.tokio_num_blocking_threads,
                    )?;
                    self.snapshot_counters.set(
                        event.timestamp(),
                        "tokio_num_idle_blocking_threads",
                        snapshot.tokio_num_idle_blocking_threads,
                    )?;
                    for (nic, stats) in &snapshot.network_interface_stats {
                        self.rate_of_change_counters
                            .set_average_rate_of_change_per_s(
                                event.timestamp(),
                                &format!("{}_send_bytes", &nic),
                                stats.tx_bytes,
                            )?;
                        self.rate_of_change_counters
                            .set_average_rate_of_change_per_s(
                                event.timestamp(),
                                &format!("{}_receive_bytes", &nic),
                                stats.rx_bytes,
                            )?;
                    }
                    self.rate_of_change_counters
                        .set_average_rate_of_change_per_s(
                            event.timestamp(),
                            "re_upload_bytes",
                            snapshot.re_upload_bytes,
                        )?;
                    self.rate_of_change_counters
                        .set_average_rate_of_change_per_s(
                            event.timestamp(),
                            "re_download_bytes",
                            snapshot.re_download_bytes,
                        )?;
                    self.rate_of_change_counters
                        .set_average_rate_of_change_per_s(
                            event.timestamp(),
                            "http_download_bytes",
                            snapshot.http_download_bytes,
                        )?;
                }
                buck2_data::instant_event::Data::ResourceControlEvent(events) => {
                    self.snapshot_counters.set(
                        event.timestamp(),
                        "allprocs_memory_pressure",
                        events.allprocs_memory_pressure,
                    )?
                }
                buck2_data::instant_event::Data::CommandPreempted(..) => {
                    self.trace_events.push(
                        ChromeTraceInstant {
                            name: "command_preempted".to_owned(),
                            timestamp: event.timestamp(),
                            scope: ChromeTraceInstantScope::Global,
                            args: None,
                        }
                        .to_json()?,
                    );
                }
                _ => {}
            },
            // Data field is oneof and `None` means the event is produced with newer version of `.proto` file
            // which added a variant which is not available in version used when compiling this program.
            buck2_data::buck_event::Data::Instant(buck2_data::InstantEvent { data: None }) => {}
            buck2_data::buck_event::Data::Record(_) => {}
        };
        Ok(())
    }

    fn write_instant_events(&mut self, events: Vec<ChromeTraceInstant>) -> buck2_error::Result<()> {
        self.trace_events.reserve(events.len());
        for event in events.into_iter() {
            self.trace_events.push(event.to_json()?);
        }
        Ok(())
    }

    fn write_critical_path(
        &mut self,
        name: SpanCategorization,
        critical_path: &[buck2_data::CriticalPathEntry2],
    ) -> buck2_error::Result<()> {
        // Write critical path as a series of spans on a dedicated track
        let target_display_options = TargetDisplayOptions::for_chrome_trace();
        self.write_critical_path_hierarchical(name, critical_path, target_display_options)
    }

    /// Write the critical path with hierarchical structure:
    /// - Non-waiting entries become parent spans
    /// - Preceding waiting entries become child spans (nested within parent)
    /// - The execution time of the non-waiting entry becomes a "critical execution time" child span
    fn write_critical_path_hierarchical(
        &mut self,
        name: SpanCategorization,
        critical_path: &[buck2_data::CriticalPathEntry2],
        target_display_options: TargetDisplayOptions,
    ) -> buck2_error::Result<()> {
        use buck2_data::critical_path_entry2::Entry;

        // All spans go on track 1. Parent-child relationships are determined by
        // time containment in Chrome trace format.
        const TRACK: u64 = 1;

        // Collect entries into groups: each group is [Waiting*, NonWaiting]
        // where the waiting entries are associated with the following non-waiting entry.
        let mut pending_waiting: Vec<&buck2_data::CriticalPathEntry2> = Vec::new();

        for entry in critical_path {
            let is_waiting = matches!(&entry.entry, Some(Entry::Waiting(_)));

            if is_waiting {
                pending_waiting.push(entry);
            } else {
                // This is a non-waiting entry - render the group
                self.write_critical_path_group(
                    name,
                    &pending_waiting,
                    entry,
                    target_display_options,
                    TRACK,
                )?;
                pending_waiting.clear();
            }
        }

        // Handle any trailing waiting entries by creating a synthetic parent
        if !pending_waiting.is_empty() {
            // Create a synthetic GenericEntry to act as the parent for orphaned waiting entries.
            // The parent span needs to cover all waiting entries, so we set start_offset_ns
            // to the last waiting entry's start and total_duration to its duration.
            // This way the parent span will cover from first_waiting.start to last_waiting.end.
            let last_waiting = pending_waiting.last().unwrap();

            let synthetic_parent = buck2_data::CriticalPathEntry2 {
                span_ids: Vec::new(),
                duration: last_waiting.total_duration,
                user_duration: None,
                total_duration: last_waiting.total_duration,
                potential_improvement_duration: None,
                queue_duration: None,
                non_critical_path_duration: last_waiting.non_critical_path_duration,
                start_offset_ns: last_waiting.start_offset_ns,
                entry: Some(Entry::GenericEntry(
                    buck2_data::critical_path_entry2::GenericEntry {
                        kind: "waiting".to_owned(),
                    },
                )),
            };

            self.write_critical_path_group(
                name,
                &pending_waiting,
                &synthetic_parent,
                target_display_options,
                TRACK,
            )?;
        }

        Ok(())
    }

    /// Write a single group: parent span covering all entries, with child spans for
    /// waiting entries and the execution time.
    fn write_critical_path_group(
        &mut self,
        name: SpanCategorization,
        waiting_entries: &[&buck2_data::CriticalPathEntry2],
        main_entry: &buck2_data::CriticalPathEntry2,
        target_display_options: TargetDisplayOptions,
        track: u64,
    ) -> buck2_error::Result<()> {
        // Calculate the overall start time and duration for the parent span
        let first_start_offset = waiting_entries
            .first()
            .map(|e| e.start_offset_ns.unwrap_or(0))
            .unwrap_or_else(|| main_entry.start_offset_ns.unwrap_or(0));

        let parent_start_time = self
            .first_pass
            .command_start
            .checked_add(Duration::from_nanos(first_start_offset))
            .unwrap();

        // Calculate the end time from the main entry
        let main_start_offset = main_entry.start_offset_ns.unwrap_or(0);
        let main_critical_duration = main_entry
            .total_duration
            .as_ref()
            .and_then(|d| d.try_into_duration().ok())
            .unwrap_or(Duration::ZERO);
        let main_non_critical_duration = main_entry
            .non_critical_path_duration
            .as_ref()
            .and_then(|d| d.try_into_duration().ok())
            .unwrap_or(Duration::ZERO);
        let main_total_duration = main_critical_duration + main_non_critical_duration;

        let parent_end_offset_ns = main_start_offset + main_total_duration.as_nanos() as u64;
        let parent_duration = Duration::from_nanos(parent_end_offset_ns - first_start_offset);

        if parent_duration < Duration::from_millis(1) {
            return Ok(());
        }

        // Get display info for the main entry (this becomes the parent name)
        let main_display =
            CriticalPathEntryDisplay::from_entry(main_entry, target_display_options)?;
        let main_display = match main_display {
            Some(display) => display,
            None => return Ok(()),
        };

        // Build args for parent span
        let mut parent_args = serde_json::Map::new();
        parent_args.insert("kind".to_owned(), json!(main_display.kind));
        if !main_display.name.is_empty() {
            parent_args.insert("name".to_owned(), json!(main_display.name));
        }
        if let Some(category) = main_display.category {
            parent_args.insert("category".to_owned(), json!(category));
        }
        if let Some(identifier) = main_display.identifier {
            parent_args.insert("identifier".to_owned(), json!(identifier));
        }
        if let Some(execution_kind) = main_display.execution_kind {
            parent_args.insert("execution_kind".to_owned(), json!(execution_kind));
        }

        let parent_name = main_display.display_name();

        // Create the track ID for this group - parent owns it, children inherit it
        let track_id = TrackId(name, track);

        // Write parent span (owns the track)
        self.trace_events.push(
            ChromeTraceClosedSpan {
                open: ChromeTraceOpenSpan {
                    name: parent_name,
                    start: parent_start_time,
                    process_id: 0,
                    track: SpanTrackAssignment::Owned(track_id),
                    categories: vec![],
                    args: parent_args.into(),
                },
                duration: parent_duration,
            }
            .to_json()?,
        );

        // Small offset to avoid trace viewer rendering issues when parent/child
        // share exact start/end times. Chrome trace uses microseconds.
        const CHILD_TIME_OFFSET: Duration = Duration::from_micros(1);

        // Write child spans for waiting entries (inherit the track from parent)
        // Offset start time by 1 unit to avoid exact overlap with parent start
        for waiting_entry in waiting_entries {
            self.write_critical_path_child_entry(
                waiting_entry,
                target_display_options,
                track_id,
                CHILD_TIME_OFFSET,
                Duration::ZERO, // no end offset for waiting entries
            )?;
        }

        // Write "critical execution time" child span for the main entry's execution
        let main_start_time = self
            .first_pass
            .command_start
            .checked_add(Duration::from_nanos(main_start_offset))
            .unwrap();

        if main_total_duration >= Duration::from_millis(1) {
            // Offset start by 1 unit and reduce duration by 1 unit so it ends before parent
            let adjusted_start = main_start_time + CHILD_TIME_OFFSET;
            let adjusted_duration = main_total_duration.saturating_sub(CHILD_TIME_OFFSET * 2);

            if adjusted_duration >= Duration::from_millis(1) {
                let exec_name = format!("{}: execution", main_display.kind);
                self.trace_events.push(
                    ChromeTraceClosedSpan {
                        open: ChromeTraceOpenSpan {
                            name: exec_name,
                            start: adjusted_start,
                            process_id: 0,
                            track: SpanTrackAssignment::Inherited(track_id),
                            categories: vec![],
                            args: json!({"kind": "execution"}),
                        },
                        duration: adjusted_duration,
                    }
                    .to_json()?,
                );
            }
        }

        Ok(())
    }

    /// Write a child critical path entry as a span (inherits track from parent).  start_offset and
    /// end_offset are used to slightly shrink the child span to avoid exact overlap with parent
    /// boundaries (as trace viewers have inconsistent behavior if child spans aren't strictly
    /// nested in the parent).
    fn write_critical_path_child_entry(
        &mut self,
        entry: &buck2_data::CriticalPathEntry2,
        target_display_options: TargetDisplayOptions,
        parent_track_id: TrackId,
        start_offset: Duration,
        end_offset: Duration,
    ) -> buck2_error::Result<()> {
        let start_time = self
            .first_pass
            .command_start
            .checked_add(Duration::from_nanos(entry.start_offset_ns.unwrap_or(0)))
            .unwrap()
            + start_offset;
        let critical_duration = entry
            .total_duration
            .as_ref()
            .and_then(|d| d.try_into_duration().ok())
            .unwrap_or(Duration::ZERO);
        let non_critical_duration = entry
            .non_critical_path_duration
            .as_ref()
            .and_then(|d| d.try_into_duration().ok())
            .unwrap_or(Duration::ZERO);
        let duration = (critical_duration + non_critical_duration)
            .saturating_sub(start_offset)
            .saturating_sub(end_offset);

        if duration < Duration::from_millis(1) {
            return Ok(());
        }

        let entry_display = CriticalPathEntryDisplay::from_entry(entry, target_display_options)?;
        let entry_display = match entry_display {
            Some(display) => display,
            None => return Ok(()),
        };

        let mut args = serde_json::Map::new();
        args.insert("kind".to_owned(), json!(entry_display.kind));
        if !entry_display.name.is_empty() {
            args.insert("name".to_owned(), json!(entry_display.name));
        }

        let entry_name = entry_display.display_name();

        self.trace_events.push(
            ChromeTraceClosedSpan {
                open: ChromeTraceOpenSpan {
                    name: entry_name,
                    start: start_time,
                    process_id: 0,
                    track: SpanTrackAssignment::Inherited(parent_track_id),
                    categories: vec![],
                    args: args.into(),
                },
                duration,
            }
            .to_json()?,
        );

        Ok(())
    }

    fn handle_event_end(
        &mut self,
        end: &buck2_data::SpanEndEvent,
        event: &BuckEvent,
    ) -> buck2_error::Result<()> {
        self.span_counters.handle_event_end(end, event)?;
        if let Some(open) = self.open_spans.remove(&event.span_id().unwrap()) {
            let duration = end
                .duration
                .as_ref()
                .ok_or_else(|| internal_error!("Expected SpanEndEvent to have duration"))?
                .try_into_duration()?;
            if let SpanTrackAssignment::Owned(track_id) = &open.track {
                self.unused_track_ids
                    .get_mut(&track_id.0)
                    .unwrap()
                    .mark_unused(track_id.1);
            }
            self.trace_events
                .push(ChromeTraceClosedSpan { open, duration }.to_json()?);
        }

        match end.data.as_ref() {
            Some(buck2_data::span_end_event::Data::Materialization(materialization)) => {
                if !materialization.success {
                    self.trace_events.push(
                        ChromeTraceInstant {
                            name: "materialization_failure".to_owned(),
                            timestamp: event.timestamp(),
                            // These have parent_id == 0, so we can't relate
                            // them back to the action that cause them right now
                            scope: ChromeTraceInstantScope::Global,
                            args: Some(json!({
                                "file_count": materialization.file_count,
                                "total_bytes": materialization.total_bytes,
                                "path": materialization.path,
                                "action_digest": materialization.action_digest.as_ref().map(|digest| digest.to_string()),
                                "success": materialization.success,
                                "error": materialization.error,
                                // "method": materialization.method, // TODO: convert to string?
                            })),
                        }
                        .to_json()?,
                    );
                }
            }
            _ => {}
        }

        Ok(())
    }
}

impl ChromeTraceCommand {
    pub fn exec(
        self,
        matches: BuckArgMatches<'_>,
        ctx: ClientCommandContext<'_>,
        events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        ctx.exec(self, matches, events_ctx)
    }

    async fn load_events(
        log_path: EventLogPathBuf,
    ) -> buck2_error::Result<(
        Invocation,
        BoxStream<'static, buck2_error::Result<BuckEvent>>,
    )> {
        let (invocation, stream_values) = log_path.unpack_stream().await?;
        let stream = stream_values.try_filter_map(|stream_value| async move {
            match stream_value {
                StreamValue::Event(e) => Ok(Some(BuckEvent::try_from(e)?)),
                _ => Ok(None),
            }
        });

        Ok((invocation, Box::pin(stream)))
    }

    fn trace_path_from_dir(
        dir: AbsPathBuf,
        log: &std::path::Path,
    ) -> buck2_error::Result<AbsPathBuf> {
        match log.file_name() {
            None => Err(buck2_error!(
                buck2_error::ErrorTag::Input,
                "Could not determine filename from event log path: `{:#}`",
                log.display()
            )),
            Some(file_name) => {
                let mut trace_path = dir;
                trace_path.push(file_name);
                trace_path.set_extension("trace");
                Ok(trace_path)
            }
        }
    }
}

impl BuckSubcommand for ChromeTraceCommand {
    const COMMAND_NAME: &'static str = "chrome-trace";

    async fn exec_impl(
        self,
        _matches: BuckArgMatches<'_>,
        ctx: ClientCommandContext<'_>,
        _events_ctx: &mut EventsCtx,
    ) -> ExitResult {
        // For backward compatibility, use the path field if it's set
        let log = if let Some(path) = self.path {
            EventLogPathBuf::infer(path.resolve(&ctx.working_dir))?
        } else {
            self.event_log.get(&ctx).await?
        };
        let trace_path = self.trace_path.resolve(&ctx.working_dir);
        let dest_path = if trace_path.is_dir() {
            Self::trace_path_from_dir(trace_path, log.path())
                .buck_error_context("Could not determine trace path")?
        } else {
            trace_path
        };

        let mut instant_events = Vec::with_capacity(self.instant.len() / 2);
        let (instant_args, instant_remainder) = self.instant.as_chunks::<2>();
        if !instant_remainder.is_empty() {
            return ExitResult::err(buck2_error::internal_error!(
                "Expected even number of arguments for --instant"
            ));
        }
        for instant in instant_args {
            let event = Self::parse_marker_arg(&instant[0], &instant[1])?;
            instant_events.push(event);
        }

        let writer = Self::trace_writer(log, self.max_tracks, instant_events).await?;

        let tracefile = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(dest_path)?;
        writer.to_writer(BufWriter::new(tracefile))?;

        ExitResult::success()
    }
}

impl ChromeTraceCommand {
    fn parse_marker_arg(time: &str, name: &str) -> buck2_error::Result<ChromeTraceInstant> {
        // Look for integer or float times. Note that we don't parse f64 in order to maintain the tv_nsec values
        let datetime = buck2_event_log::utils::timestamp::parse(time)?;
        Ok(ChromeTraceInstant {
            name: name.to_owned(),
            timestamp: datetime.into(),
            scope: ChromeTraceInstantScope::Global,
            args: None,
        })
    }

    async fn trace_writer(
        log: EventLogPathBuf,
        max_tracks: Option<u64>,
        instant_events: Vec<ChromeTraceInstant>,
    ) -> buck2_error::Result<ChromeTraceWriter> {
        let (invocation, mut stream) = Self::load_events(log.clone()).await?;
        let mut first_pass = ChromeTraceFirstPass::new();
        let mut build_graph_info = None;
        while let Some(event) = tokio_stream::StreamExt::try_next(&mut stream).await? {
            first_pass
                .handle_event(&event)
                .with_buck_error_context(|| {
                    display::InvalidBuckEvent(Arc::new(event.clone())).to_string()
                })?;
            if let Ok(UnpackedBuckEvent::Instant(
                _,
                _,
                buck2_data::instant_event::Data::BuildGraphInfo(info),
            )) = unpack_event(&event)
            {
                build_graph_info = Some(info.clone());
            }
        }

        let max_tracks = max_tracks.unwrap_or(
            first_pass
                .command_options
                .map_or(20, |opt| opt.configured_parallelism),
        );

        let mut writer = ChromeTraceWriter::new(invocation, first_pass, max_tracks);

        // We do this to ensure that these are the first two tracks.
        if let Some(info) = build_graph_info {
            if !info.slowest_path.is_empty() {
                writer.write_critical_path(
                    SpanCategorization::DetailedSlowestPath,
                    &info.slowest_path,
                )?;
            }
            if !info.critical_path2.is_empty() {
                writer.write_critical_path(
                    SpanCategorization::DetailedCriticalPath,
                    &info.critical_path2,
                )?;
            }
        }

        writer.write_instant_events(instant_events)?;

        // We just read events again from log file, in order to avoid holding all logs in memory
        let (_invocation, mut stream) = Self::load_events(log).await?;
        while let Some(event) = tokio_stream::StreamExt::try_next(&mut stream).await? {
            let event = Arc::new(event);
            writer
                .handle_event(&event)
                .with_buck_error_context(|| display::InvalidBuckEvent(event).to_string())?;
        }
        Ok(writer)
    }
}
