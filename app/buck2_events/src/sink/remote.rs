/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! A Sink for forwarding events directly to Remote service.
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

use fbinit::FacebookInit;

#[cfg(fbcode_build)]
mod fbcode {
    use std::sync::Arc;
    use std::time::SystemTime;

    use buck2_core::buck2_env;
    use buck2_data::InstantEvent;
    use buck2_data::Location;
    use buck2_data::StructuredError;
    use buck2_error::conversion::from_any_with_tag;
    use buck2_error::ErrorTag;
    use buck2_util::truncate::truncate;
    use fbinit::FacebookInit;
    use prost::Message;
    pub use scribe_client::ScribeConfig;

    use crate::metadata;
    use crate::schedule_type::ScheduleType;
    use crate::sink::smart_truncate_event::smart_truncate_event;
    use crate::BuckEvent;
    use crate::Event;
    use crate::EventSink;
    use crate::EventSinkStats;
    use crate::EventSinkWithStats;
    use crate::TraceId;

    // 1 MiB limit
    static SCRIBE_MESSAGE_SIZE_LIMIT: usize = 1024 * 1024;
    // 50k characters
    static TRUNCATED_SCRIBE_MESSAGE_SIZE: usize = 50000;

    /// RemoteEventSink is a ScribeSink backed by the Thrift-based client in the `buck2_scribe_client` crate.
    pub struct RemoteEventSink {
        category: String,
        client: scribe_client::ScribeClient,
        schedule_type: ScheduleType,
    }

    impl RemoteEventSink {
        /// Creates a new RemoteEventSink that forwards messages onto the Thrift-backed Scribe client.
        pub fn new(
            fb: FacebookInit,
            category: String,
            config: ScribeConfig,
        ) -> buck2_error::Result<RemoteEventSink> {
            let client = scribe_client::ScribeClient::new(fb, config)
                .map_err(|e| from_any_with_tag(e, ErrorTag::Tier0))?;

            // schedule_type can change for the same daemon, because on OD some builds are pre warmed for users
            // This would be problematic, because this is run just once on the daemon
            // But in this case we only check for 'diff' type, which shouldn't change
            let schedule_type = ScheduleType::new()?;
            Ok(RemoteEventSink {
                category,
                client,
                schedule_type,
            })
        }

        // Send this event now, bypassing internal message queue.
        pub async fn send_now(&self, event: BuckEvent) -> buck2_error::Result<()> {
            self.send_messages_now(vec![event]).await
        }

        // Send multiple events now, bypassing internal message queue.
        pub async fn send_messages_now(&self, events: Vec<BuckEvent>) -> buck2_error::Result<()> {
            let messages = events
                .into_iter()
                .filter_map(|e| {
                    let message_key = e.trace_id().unwrap().hash();
                    Self::encode_message(e, false).map(|bytes| scribe_client::Message {
                        category: self.category.clone(),
                        message: bytes,
                        message_key: Some(message_key),
                    })
                })
                .collect();
            self.client
                .send_messages_now(messages)
                .await
                .map_err(|e| from_any_with_tag(e, ErrorTag::Tier0))
        }

        // Send this event by placing it on the internal message queue.
        pub fn offer(&self, event: BuckEvent) {
            let message_key = event.trace_id().unwrap().hash();
            if let Some(bytes) = Self::encode_message(event, false) {
                self.client.offer(scribe_client::Message {
                    category: self.category.clone(),
                    message: bytes,
                    message_key: Some(message_key),
                });
            }
        }

        // Encodes message into something scribe understands.
        fn encode_message(mut event: BuckEvent, is_truncated: bool) -> Option<Vec<u8>> {
            smart_truncate_event(event.data_mut());
            let mut proto: Box<buck2_data::BuckEvent> = event.into();

            Self::prepare_event(&mut proto);

            // Add a header byte to indicate this is _not_ base64 encoding.
            let mut buf = Vec::with_capacity(proto.encoded_len() + 1);
            buf.push(b'!');
            let mut proto_bytes = proto.encode_to_vec();
            buf.append(&mut proto_bytes);

            if buf.len() > SCRIBE_MESSAGE_SIZE_LIMIT {
                // if this BuckEvent is already a truncated one but the buffer byte size exceeds the limit,
                // do not send Scribe another truncated version
                if is_truncated {
                    return None;
                }
                let json = serde_json::to_string(&proto).unwrap();

                Self::encode_message(
                    BuckEvent::new(
                        SystemTime::now(),
                        TraceId::new(),
                        None,
                        None,
                        buck2_data::buck_event::Data::Instant(InstantEvent {
                            data: Some(
                                StructuredError {
                                    location: Some(Location {
                                        file: file!().to_owned(),
                                        line: line!(),
                                        column: column!(),
                                    }),
                                    payload: format!("Soft Error: oversized_scribe: Message is oversized. Event data: {}. Original message size: {}", truncate(&json, TRUNCATED_SCRIBE_MESSAGE_SIZE),
                                    buf.len()),
                                    metadata: metadata::collect(),
                                    backtrace: Vec::new(),
                                    quiet: false,
                                    task: Some(true),
                                    soft_error_category: Some(buck2_data::SoftError {category: "oversized_scribe".to_owned(), is_quiet:false}),
                                    daemon_in_memory_state_is_corrupted: false,
                                    daemon_materializer_state_is_corrupted: false,
                                    action_cache_is_corrupted: false,
                                    deprecation: false,
                                }
                                .into(),
                            ),
                        }),
                    ),
                    true,
                )
            } else {
                Some(buf)
            }
        }

        fn prepare_event(event: &mut buck2_data::BuckEvent) {
            use buck2_data::buck_event::Data;

            match &mut event.data {
                Some(Data::SpanEnd(s)) => match &mut s.data {
                    Some(buck2_data::span_end_event::Data::ActionExecution(action)) => {
                        let mut is_cache_hit = false;

                        for command in action.commands.iter_mut() {
                            let Some(details) = command.details.as_mut() else {
                                continue;
                            };

                            {
                                let Some(ref command_kind) = details.command_kind else {
                                    continue;
                                };
                                let Some(ref command) = command_kind.command else {
                                    continue;
                                };
                                let buck2_data::command_execution_kind::Command::RemoteCommand(
                                    ref remote,
                                ) = command
                                else {
                                    continue;
                                };
                                if !remote.cache_hit {
                                    continue;
                                }
                            }

                            is_cache_hit = true;
                            details.metadata = None;
                        }

                        if is_cache_hit {
                            action.dep_file_key = None;
                            action.outputs.clear();
                        }
                    }
                    _ => {}
                },
                _ => {}
            }
        }
    }

    impl EventSink for RemoteEventSink {
        fn send(&self, event: Event) {
            match event {
                Event::Buck(event) => {
                    if should_send_event(event.data(), &self.schedule_type) {
                        self.offer(event);
                    }
                }
                Event::CommandResult(..) => {}
                Event::PartialResult(..) => {}
            }
        }
    }

    impl EventSinkWithStats for RemoteEventSink {
        fn to_event_sync(self: Arc<Self>) -> Arc<dyn EventSink> {
            self as _
        }

        fn stats(&self) -> EventSinkStats {
            let counters = self.client.export_counters();
            EventSinkStats {
                successes: counters.successes,
                failures_invalid_request: counters.failures_invalid_request,
                failures_unauthorized: counters.failures_unauthorized,
                failures_rate_limited: counters.failures_rate_limited,
                failures_pushed_back: counters.failures_pushed_back,
                failures_enqueue_failed: counters.failures_enqueue_failed,
                failures_internal_error: counters.failures_internal_error,
                failures_timed_out: counters.failures_timed_out,
                failures_unknown: counters.failures_unknown,
                buffered: counters.queue_depth,
                dropped: counters.dropped,
                bytes_written: counters.bytes_written,
            }
        }
    }

    fn should_send_event(d: &buck2_data::buck_event::Data, schedule_type: &ScheduleType) -> bool {
        use buck2_data::buck_event::Data;

        match d {
            Data::SpanStart(s) => {
                use buck2_data::span_start_event::Data;

                match &s.data {
                    Some(Data::Command(..)) => true,
                    None => false,
                    _ => false,
                }
            }
            Data::SpanEnd(s) => {
                use buck2_data::span_end_event::Data;
                use buck2_data::ActionExecutionKind;

                match &s.data {
                    Some(Data::Command(..)) => true,
                    Some(Data::ActionExecution(a)) => {
                        a.failed
                            || match ActionExecutionKind::try_from(a.execution_kind) {
                                // Those kinds are not used in downstreams
                                Ok(ActionExecutionKind::Simple) => false,
                                Ok(ActionExecutionKind::Deferred) => false,
                                Ok(ActionExecutionKind::NotSet) => false,
                                _ => true,
                            }
                    }
                    Some(Data::Analysis(..)) => !schedule_type.is_diff(),
                    Some(Data::Load(..)) => true,
                    Some(Data::CacheUpload(..)) => true,
                    Some(Data::DepFileUpload(..)) => true,
                    Some(Data::Materialization(..)) => true,
                    Some(Data::TestDiscovery(..)) => true,
                    Some(Data::TestEnd(..)) => true,
                    None => false,
                    _ => false,
                }
            }
            Data::Instant(i) => {
                use buck2_data::instant_event::Data;

                match i.data {
                    Some(Data::BuildGraphInfo(..)) => true,
                    Some(Data::RageResult(..)) => true,
                    Some(Data::ReSession(..)) => true,
                    Some(Data::StructuredError(..)) => true,
                    Some(Data::PersistEventLogSubprocess(..)) => true,
                    Some(Data::CleanStaleResult(..)) => true,
                    Some(Data::ConfigurationCreated(..)) => true,
                    None => false,
                    _ => false,
                }
            }
            Data::Record(r) => {
                use buck2_data::record_event::Data;

                match r.data {
                    Some(Data::InvocationRecord(..)) => true,
                    Some(Data::BuildGraphStats(..)) => true,
                    None => false,
                }
            }
        }
    }

    pub fn scribe_category() -> buck2_error::Result<String> {
        const DEFAULT_SCRIBE_CATEGORY: &str = "buck2_events";
        // Note that both daemon and client are emitting events, and that changing this variable has
        // no effect on the daemon until buckd is restarted but has effect on the client.
        Ok(
            buck2_env!("BUCK2_SCRIBE_CATEGORY", applicability = internal)?
                .unwrap_or(DEFAULT_SCRIBE_CATEGORY)
                .to_owned(),
        )
    }
}

#[cfg(not(fbcode_build))]
mod fbcode {
    use std::sync::Arc;
    use std::time::Duration;

    use crate::BuckEvent;
    use crate::Event;
    use crate::EventSink;
    use crate::EventSinkStats;
    use crate::EventSinkWithStats;

    pub enum RemoteEventSink {}

    impl RemoteEventSink {
        pub async fn send_now(&self, _event: BuckEvent) -> buck2_error::Result<()> {
            Ok(())
        }
        pub async fn send_messages_now(&self, _events: Vec<BuckEvent>) -> buck2_error::Result<()> {
            Ok(())
        }
    }

    impl EventSink for RemoteEventSink {
        fn send(&self, _event: Event) {}
    }

    impl EventSinkWithStats for RemoteEventSink {
        fn to_event_sync(self: Arc<Self>) -> Arc<dyn EventSink> {
            self as _
        }

        fn stats(&self) -> EventSinkStats {
            match *self {}
        }
    }

    #[derive(Default)]
    pub struct ScribeConfig {
        pub buffer_size: usize,
        pub retry_backoff: Duration,
        pub retry_attempts: usize,
        pub message_batch_size: Option<usize>,
        pub thrift_timeout: Duration,
    }
}

pub use fbcode::*;

fn new_remote_event_sink_if_fbcode(
    fb: FacebookInit,
    config: ScribeConfig,
) -> buck2_error::Result<Option<RemoteEventSink>> {
    #[cfg(fbcode_build)]
    {
        Ok(Some(RemoteEventSink::new(fb, scribe_category()?, config)?))
    }
    #[cfg(not(fbcode_build))]
    {
        let _ = (fb, config);
        Ok(None)
    }
}

pub fn new_remote_event_sink_if_enabled(
    fb: FacebookInit,
    config: ScribeConfig,
) -> buck2_error::Result<Option<RemoteEventSink>> {
    if is_enabled() {
        new_remote_event_sink_if_fbcode(fb, config)
    } else {
        Ok(None)
    }
}

/// Whether or not remote event logging is enabled for this process. It must be explicitly disabled via `disable()`.
static REMOTE_EVENT_SINK_ENABLED: AtomicBool = AtomicBool::new(true);

/// Returns whether this process should actually write to remote sink, even if it is fully supported by the platform and
/// binary.
pub fn is_enabled() -> bool {
    REMOTE_EVENT_SINK_ENABLED.load(Ordering::Relaxed)
}

/// Disables remote event logging for this process. Remote event logging must be disabled explicitly on startup, otherwise it is
/// on by default.
pub fn disable() {
    REMOTE_EVENT_SINK_ENABLED.store(false, Ordering::Relaxed);
}
