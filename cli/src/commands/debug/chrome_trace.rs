/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::BTreeSet;
use std::collections::HashMap;
use std::collections::HashSet;
use std::io::BufWriter;
use std::io::Write;
use std::path::PathBuf;
use std::time::Duration;
use std::time::SystemTime;

use anyhow::Context;
use buck2_client::exit_result::ExitResult;
use buck2_client::subscribers::display::TargetDisplayOptions;
use buck2_client::subscribers::event_log::EventLogPathBuf;
use buck2_common::convert::ProstDurationExt;
use buck2_events::subscriber::VisitorError;
use buck2_events::BuckEvent;
use buck2_events::SpanId;
use futures::TryStreamExt;
use gazebo::prelude::*;
use serde::Serialize;
use serde_json::json;
use thiserror::Error;
use tokio::runtime;

use crate::commands::debug::replay::retrieve_nth_recent_log;

#[derive(Error, Debug)]
pub(crate) enum ChromeTraceError {
    #[error("Couldn't assign track to {0:?}: no track assigned to parent {1:?}")]
    ParentSpanNotFound(SpanId, SpanId),
}

use buck2_client::client_ctx::ClientCommandContext;
use buck2_client::stream_value::StreamValue;
use buck2_client::subscribers::display;
use buck2_client::subscribers::event_log::Invocation;

#[derive(Debug, clap::Parser)]
pub(crate) struct ChromeTraceCommand {
    #[clap(
        long,
        help = "Where to write the chrome trace JSON. If a directory is passed, the filename of the event log will be used as a base filename."
    )]
    pub trace_path: PathBuf,
    /// The path to read the event log from.
    #[clap(
        long,
        help = "A path to an event-log file to read from. Only works for log files with a single command in them. If no event-log is passed, the most recent one will be used.",
        group = "event_log",
        value_name = "PATH"
    )]
    pub path: Option<PathBuf>,

    /// Which recent command to read the event log from.
    #[clap(
        long,
        help = "Use the event-log from the Nth most recent command (`--recent 0` is the most recent).",
        group = "event_log",
        value_name = "NUMBER"
    )]
    pub recent: Option<usize>,
}

struct ChromeTraceFirstPass {
    /// Track assignment needs to know, when it sees a SpanStart, whether that
    /// span is going to be included in the final trace.
    /// But some spans need to be filtered based on later events, like:
    /// 1. We shouldn't assign tracks to StartLoad events whose SpanEnd records
    ///    a really short duration.
    /// 2. We shouldn't assign tracks to ActionExecutionStart events who have
    ///    no child LocalStage spans.
    /// 3. (eventually) We should assign tracks to ActionExecutionStart events
    ///    only if they appear in the CriticalPath, but the CriticalPath is one
    ///    of the last events.
    /// So this first pass builds up several lists of "interesting" span IDs.
    pub long_analyses: HashSet<buck2_events::SpanId>,
    pub long_loads: HashSet<buck2_events::SpanId>,
    pub local_actions: HashSet<buck2_events::SpanId>,
    pub critical_path_action_keys: HashSet<buck2_data::ActionKey>,
}

impl ChromeTraceFirstPass {
    const LONG_ANALYSIS_CUTOFF: Duration = Duration::from_millis(50);
    const LONG_LOAD_CUTOFF: Duration = Duration::from_millis(50);
    fn new() -> Self {
        Self {
            long_analyses: HashSet::new(),
            long_loads: HashSet::new(),
            local_actions: HashSet::new(),
            critical_path_action_keys: HashSet::new(),
        }
    }

    fn handle_event(&mut self, event: &BuckEvent) -> anyhow::Result<()> {
        match event.data {
            buck2_data::buck_event::Data::SpanStart(ref start) => {
                match start.data.as_ref() {
                    Some(buck2_data::span_start_event::Data::ExecutorStage(exec)) => {
                        // A local stage means that we want to show the entire action execution.
                        if let Some(buck2_data::executor_stage_start::Stage::Local(_)) = exec.stage
                        {
                            self.local_actions.insert(event.parent_id.unwrap());
                        }
                    }
                    _ => {}
                }
            }
            buck2_data::buck_event::Data::SpanEnd(ref end) => {
                match end.data.as_ref() {
                    Some(buck2_data::span_end_event::Data::Analysis(_)) => {
                        if end
                            .duration
                            .as_ref()
                            .expect("Analysis SpanEnd missing duration")
                            .try_into_duration()?
                            > Self::LONG_ANALYSIS_CUTOFF
                        {
                            self.long_analyses.insert(event.span_id.unwrap());
                        }
                    }
                    Some(buck2_data::span_end_event::Data::Load(_)) => {
                        if end
                            .duration
                            .as_ref()
                            .expect("Load SpanEnd missing duration")
                            .try_into_duration()?
                            > Self::LONG_LOAD_CUTOFF
                        {
                            self.long_loads.insert(event.span_id.unwrap());
                        }
                    }
                    _ => {}
                };
            }
            buck2_data::buck_event::Data::Instant(ref instant) => match instant.data.as_ref() {
                Some(buck2_data::instant_event::Data::BuildGraphInfo(info)) => {
                    self.critical_path_action_keys = info
                        .critical_path
                        .iter()
                        .map(|entry| entry.action_key.clone().unwrap())
                        .collect();
                }
                _ => {}
            },
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
    fn to_json(self) -> anyhow::Result<serde_json::Value> {
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
struct TrackId(&'static str, u64);

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
    pub(crate) fn new() -> Self {
        Self {
            unused_track_ids: BTreeSet::new(),
            lowest_never_used: 0,
        }
    }

    fn get_smallest(&mut self) -> u64 {
        let maybe_smallest = self.unused_track_ids.iter().next().copied();
        if let Some(n) = maybe_smallest {
            self.unused_track_ids.remove(&n);
            n
        } else {
            let n = self.lowest_never_used;
            self.lowest_never_used += 1;
            n
        }
    }

    pub(crate) fn mark_unused(&mut self, tid: u64) {
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
    counters: HashMap<&'static str, Option<T>>,
    start_value: T,
    trace_events: Vec<serde_json::Value>,
}

impl<T> SimpleCounters<T>
where
    T: std::ops::Sub<Output = T>
        + std::cmp::PartialEq
        + std::ops::Add<Output = T>
        + std::marker::Copy
        + Serialize,
{
    const BUCKET_DURATION: Duration = Duration::from_millis(10);
    pub(crate) fn new(name: &'static str, start_value: T) -> Self {
        Self {
            name,
            next_flush: SystemTime::UNIX_EPOCH,
            counters: HashMap::new(),
            trace_events: vec![],
            start_value,
        }
    }

    /// Process the given timestamp and flush if needed and update next_flush accordingly
    fn process_timestamp(&mut self, timestamp: SystemTime) -> anyhow::Result<()> {
        if self.next_flush == SystemTime::UNIX_EPOCH {
            self.next_flush = timestamp + Self::BUCKET_DURATION;
        }
        if timestamp > self.next_flush + Self::BUCKET_DURATION {
            self.flush()?;
            self.next_flush = timestamp - Duration::from_micros(1);
        }
        Ok(())
    }

    /// If the given key is new to the map, initialize it to self.start_value and flush
    /// Return the value stored at the given key
    fn initialize_first_entry_if_needed(
        &mut self,
        timestamp: SystemTime,
        key: &'static str,
    ) -> anyhow::Result<T> {
        // If counter is being bumped from zero, we need to output its zero count
        // immediately so the line graph won't interpolate from the last time it was zero.
        let entry = *self.counters.entry(key).or_insert(None);
        if entry.is_none() {
            // Add a zero output immediately before the counter changes from zero.
            self.next_flush = timestamp - Duration::from_micros(1);
            self.counters.insert(key, Some(self.start_value));
            self.flush()?;
            self.next_flush = timestamp;
        } else if timestamp > self.next_flush {
            self.flush()?;
            self.next_flush = timestamp + Self::BUCKET_DURATION;
        }
        Ok(entry.unwrap_or(self.start_value))
    }

    fn set(&mut self, timestamp: SystemTime, key: &'static str, amount: T) -> anyhow::Result<()> {
        self.process_timestamp(timestamp)?;
        self.counters.insert(key, Some(amount));
        Ok(())
    }

    fn bump(&mut self, timestamp: SystemTime, key: &'static str, amount: T) -> anyhow::Result<()> {
        self.process_timestamp(timestamp)?;
        let entry = self.initialize_first_entry_if_needed(timestamp, key);
        self.counters.insert(key, Some(entry.unwrap() + amount));
        Ok(())
    }

    fn subtract(
        &mut self,
        timestamp: SystemTime,
        key: &'static str,
        amount: T,
    ) -> anyhow::Result<()> {
        self.process_timestamp(timestamp)?;
        let entry = self.initialize_first_entry_if_needed(timestamp, key);
        self.counters.insert(key, Some(entry.unwrap() - amount));
        Ok(())
    }

    fn flush(&mut self) -> anyhow::Result<()> {
        // Output size optimization: omit counters that were previously, and still are, zero.
        let mut output_counters = json!({});
        for (key, value) in self.counters.iter_mut() {
            if let Some(v) = value {
                output_counters[key] = json!(v);
                if *v == self.start_value {
                    *value = None;
                }
            }
        }

        self.trace_events.push(json!(
            {
                "name": self.name,
                "pid": 0,
                "tid": "counters",
                "ph": "C",
                "ts": self.next_flush
                    .duration_since(SystemTime::UNIX_EPOCH)?
                    .as_micros() as u64,
                "args": output_counters,
            }
        ));
        self.next_flush += Self::BUCKET_DURATION;
        Ok(())
    }

    pub(crate) fn flush_all_to(
        &mut self,
        output: &mut Vec<serde_json::Value>,
    ) -> anyhow::Result<()> {
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
    counters: SimpleCounters<f32>,
    previous_timestamp_and_amount_by_key: HashMap<&'static str, TimestampAndAmount>,
}

impl AverageRateOfChangeCounters {
    const MSECS_PER_SEC: f32 = 1000.0;
    pub(crate) fn new(name: &'static str) -> Self {
        Self {
            previous_timestamp_and_amount_by_key: HashMap::new(),
            counters: SimpleCounters::<f32>::new(name, 0.0),
        }
    }

    fn set_average_rate_of_change_per_ms(
        &mut self,
        timestamp: SystemTime,
        key: &'static str,
        amount: u64,
    ) -> anyhow::Result<()> {
        // We only plot if there exists a previous item to compute the rate of change off of
        if let Some(previous) = self.previous_timestamp_and_amount_by_key.get(key) {
            let secs_since_last_datapoint =
                timestamp.duration_since(previous.timestamp)?.as_secs_f32();
            let value_change_since_last_datapoint = (amount - previous.amount) as f32;
            self.counters.set(
                timestamp,
                key,
                value_change_since_last_datapoint
                    / (Self::MSECS_PER_SEC * secs_since_last_datapoint),
            )?;
        }
        self.previous_timestamp_and_amount_by_key
            .insert(key, TimestampAndAmount { timestamp, amount });

        Ok(())
    }
}

struct SpanCounters {
    counter: SimpleCounters<i32>,
    // Stores how current open spans contribute to counter values.
    open_spans: HashMap<buck2_events::SpanId, (&'static str, i32)>,
}

impl SpanCounters {
    pub(crate) fn new(name: &'static str) -> Self {
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
    ) -> anyhow::Result<()> {
        self.open_spans
            .insert(event.span_id.unwrap(), (key, amount));
        self.counter.bump(event.timestamp, key, amount)
    }

    fn handle_event_end(
        &mut self,
        _end: &buck2_data::SpanEndEvent,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        if let Some((key, value)) = self.open_spans.remove(&event.span_id.unwrap()) {
            self.counter.subtract(event.timestamp, key, value)?;
        }
        Ok(())
    }
}

struct ChromeTraceWriter {
    trace_events: Vec<serde_json::Value>,
    open_spans: HashMap<buck2_events::SpanId, ChromeTraceOpenSpan>,
    invocation: Invocation,
    first_pass: ChromeTraceFirstPass,
    span_counters: SpanCounters,
    unused_track_ids: HashMap<&'static str, TrackIdAllocator>,
    // Wrappers to contain values from InstantEvent.Data.Snapshot as a timeseries
    snapshot_counters: SimpleCounters<u64>,
    max_rss_gigabytes_counter: SimpleCounters<f64>,
    rate_of_change_counters: AverageRateOfChangeCounters,
}

impl ChromeTraceWriter {
    const UNCATEGORIZED: &'static str = "uncategorized";
    const CRITICAL_PATH: &'static str = "critical-path";
    const BYTES_PER_GIGABYTE: f64 = 1000000000.0;

    pub(crate) fn new(invocation: Invocation, first_pass: ChromeTraceFirstPass) -> Self {
        Self {
            trace_events: vec![],
            open_spans: HashMap::new(),
            invocation,
            first_pass,
            unused_track_ids: HashMap::new(),
            span_counters: SpanCounters::new("spans"),
            snapshot_counters: SimpleCounters::<u64>::new("snapshot_counters", 0),
            max_rss_gigabytes_counter: SimpleCounters::<f64>::new("max_rss", 0.0),
            rate_of_change_counters: AverageRateOfChangeCounters::new("rate_of_change_counters"),
        }
    }

    fn assign_track_for_span(
        &mut self,
        track_key: &'static str,
        event: &BuckEvent,
    ) -> anyhow::Result<SpanTrackAssignment> {
        match event.parent_id {
            None => Ok(SpanTrackAssignment::Owned(TrackId(
                track_key,
                self.unused_track_ids
                    .entry(track_key)
                    .or_insert_with(TrackIdAllocator::new)
                    .get_smallest(),
            ))),
            Some(parent_id) => {
                let track_id = self
                    .open_spans
                    .get(&parent_id)
                    .map(|open_span| open_span.track.get_track_id())
                    .ok_or_else(|| {
                        ChromeTraceError::ParentSpanNotFound(event.span_id.unwrap(), parent_id)
                    })?;
                Ok(SpanTrackAssignment::Inherited(track_id))
            }
        }
    }

    pub(crate) fn to_writer<W>(mut self, file: W) -> anyhow::Result<()>
    where
        W: Write,
    {
        self.span_counters
            .counter
            .flush_all_to(&mut self.trace_events)?;
        self.snapshot_counters
            .flush_all_to(&mut self.trace_events)?;
        self.max_rss_gigabytes_counter
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

    fn open_span(&mut self, event: &BuckEvent, span: ChromeTraceOpenSpan) -> anyhow::Result<()> {
        self.open_spans.insert(event.span_id.unwrap(), span);
        Ok(())
    }

    fn open_named_span(
        &mut self,
        event: &BuckEvent,
        name: String,
        track_key: &'static str,
    ) -> anyhow::Result<()> {
        // Allocate this span to its parent's track or to a new track.
        let track = self.assign_track_for_span(track_key, event)?;
        self.open_span(
            event,
            ChromeTraceOpenSpan {
                name,
                start: event.timestamp,
                process_id: 0,
                track,
                categories: vec!["buck2"],
                args: json!({
                    "span_id": event.span_id,
                }),
            },
        )
    }

    fn handle_event(&mut self, event: &BuckEvent) -> anyhow::Result<()> {
        match event.data {
            buck2_data::buck_event::Data::SpanStart(ref start) => match start
                .data
                .as_ref()
                .ok_or_else(|| VisitorError::MissingField((*event).clone()))?
            {
                buck2_data::span_start_event::Data::Command(_command) => {
                    self.open_named_span(
                        event,
                        self.invocation.command_line_args.join(" "),
                        Self::UNCATEGORIZED,
                    )?;
                }
                buck2_data::span_start_event::Data::Analysis(analysis) => {
                    self.span_counters
                        .bump_counter_while_span(event, "analysis", 1)?;
                    if self
                        .first_pass
                        .long_analyses
                        .contains(&event.span_id.unwrap())
                    {
                        let name = format!(
                            "analysis {}",
                            display::display_configured_target_label(
                                analysis
                                    .target
                                    .as_ref()
                                    .expect("AnalysisStart event missing 'target' field"),
                                TargetDisplayOptions::for_console()
                            )?,
                        );
                        self.open_named_span(event, name, Self::UNCATEGORIZED)?;
                    }
                }
                buck2_data::span_start_event::Data::Load(eval) => {
                    self.span_counters
                        .bump_counter_while_span(event, "load", 1)?;
                    if self.first_pass.long_loads.contains(&event.span_id.unwrap()) {
                        let name = format!("load {}", eval.module_id);
                        self.open_named_span(event, name, Self::UNCATEGORIZED)?;
                    }
                }
                buck2_data::span_start_event::Data::ActionExecution(action) => {
                    let maybe_track = if self
                        .first_pass
                        .critical_path_action_keys
                        .contains(action.key.as_ref().unwrap())
                    {
                        Some(Self::CRITICAL_PATH)
                    } else if self
                        .first_pass
                        .local_actions
                        .contains(&event.span_id.unwrap())
                    {
                        Some(Self::UNCATEGORIZED)
                    } else {
                        None
                    };
                    if let Some(track) = maybe_track {
                        let name = display::display_action_identity(
                            action.key.as_ref(),
                            action.name.as_ref(),
                            TargetDisplayOptions::for_console(),
                        )?;
                        self.open_named_span(event, name, track)?;
                    }
                }
                buck2_data::span_start_event::Data::ExecutorStage(stage) => {
                    let name = display::display_executor_stage(
                        stage.stage.as_ref().context("expected stage")?,
                    )?;
                    self.span_counters.bump_counter_while_span(event, name, 1)?;
                    if self.open_spans.contains_key(&event.parent_id.unwrap()) {
                        // As a child event, this will inherit its parent's track.
                        self.open_named_span(event, name.to_owned(), Self::UNCATEGORIZED)?;
                    }
                }
                buck2_data::span_start_event::Data::Watchman(_watchman) => {
                    self.open_named_span(event, "watchman_sync".to_owned(), Self::CRITICAL_PATH)?;
                }
                _ => {}
            },
            buck2_data::buck_event::Data::SpanEnd(ref end) => self.handle_event_end(end, event)?,
            buck2_data::buck_event::Data::Instant(ref instant) => {
                let instant_data = instant
                    .data
                    .as_ref()
                    .ok_or_else(|| VisitorError::MissingField((*event).clone()))?;
                if let buck2_data::instant_event::Data::Snapshot(_snapshot) = instant_data {
                    self.max_rss_gigabytes_counter.set(
                        event.timestamp,
                        "max_rss_gigabyte",
                        (_snapshot.buck2_max_rss) as f64 / Self::BYTES_PER_GIGABYTE,
                    )?;
                    self.rate_of_change_counters
                        .set_average_rate_of_change_per_ms(
                            event.timestamp,
                            "average_user_cpu_in_usecs_per_msec",
                            _snapshot.buck2_user_cpu_us,
                        )?;
                    self.rate_of_change_counters
                        .set_average_rate_of_change_per_ms(
                            event.timestamp,
                            "average_system_cpu_in_usecs_per_msec",
                            _snapshot.buck2_system_cpu_us,
                        )?;
                    self.snapshot_counters.set(
                        event.timestamp,
                        "blocking_executor_io_queue_size",
                        _snapshot.blocking_executor_io_queue_size,
                    )?;
                }
            }
            buck2_data::buck_event::Data::Record(_) => {}
        };
        Ok(())
    }

    fn handle_event_end(
        &mut self,
        end: &buck2_data::SpanEndEvent,
        event: &BuckEvent,
    ) -> anyhow::Result<()> {
        self.span_counters.handle_event_end(end, event)?;
        if let Some(open) = self.open_spans.remove(&event.span_id.unwrap()) {
            let duration = end
                .duration
                .as_ref()
                .context("Expected SpanEndEvent to have duration")?
                .try_into_duration()?;
            if let SpanTrackAssignment::Owned(track_id) = &open.track {
                self.unused_track_ids
                    .get_mut(track_id.0)
                    .unwrap()
                    .mark_unused(track_id.1);
            }
            self.trace_events
                .push(ChromeTraceClosedSpan { open, duration }.to_json()?);
        }
        Ok(())
    }
}

impl ChromeTraceCommand {
    async fn load_events(
        path: PathBuf,
    ) -> anyhow::Result<(Invocation, Vec<buck2_events::BuckEvent>)> {
        let log_path = EventLogPathBuf::infer(path)?;
        let (invocation, mut stream_values) = log_path.unpack_stream().await?;

        let mut buck_events = Vec::new();

        while let Some(stream_value) = stream_values.try_next().await? {
            match stream_value {
                StreamValue::Event(e) => {
                    let buck_event_result = buck2_events::BuckEvent::try_from(e);
                    match buck_event_result {
                        Ok(buck_event) => buck_events.push(buck_event),
                        Err(e) => buck2_client::eprintln!("Error converting event-log: {:#}", e)?,
                    }
                }
                _ => (),
            }
        }

        Ok((invocation, buck_events))
    }

    fn trace_path_from_dir(dir: PathBuf, log: &std::path::Path) -> anyhow::Result<PathBuf> {
        match log.file_name() {
            None => Err(anyhow::anyhow!(
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

    pub(crate) fn exec(
        self,
        _matches: &clap::ArgMatches,
        _ctx: ClientCommandContext,
    ) -> ExitResult {
        let rt = runtime::Builder::new_current_thread()
            .enable_all()
            .build()?;

        let log = match self.path {
            Some(path) => path,
            None => retrieve_nth_recent_log(&_ctx, self.recent.unwrap_or(0))?,
        };

        let dest_path_result = if self.trace_path.is_dir() {
            Self::trace_path_from_dir(self.trace_path, &log)
        } else {
            Ok(self.trace_path)
        };

        let dest_path = match dest_path_result {
            Ok(dest_path) => dest_path,
            Err(e) => {
                buck2_client::eprintln!("Could not determine trace path, {:#}", e)?;
                return ExitResult::failure();
            }
        };

        let (invocation, events) = rt.block_on(async move { Self::load_events(log).await })?;

        let mut first_pass = ChromeTraceFirstPass::new();
        for event in events.iter() {
            first_pass
                .handle_event(event)
                .with_context(|| display::InvalidBuckEvent(event.clone()))?;
        }
        let mut writer = ChromeTraceWriter::new(invocation, first_pass);
        for event in events.iter() {
            writer
                .handle_event(event)
                .with_context(|| display::InvalidBuckEvent(event.clone()))?;
        }
        let tracefile = std::fs::OpenOptions::new()
            .create(true)
            .write(true)
            .truncate(true)
            .open(dest_path)?;
        writer.to_writer(BufWriter::new(tracefile))?;
        ExitResult::success()
    }
}
