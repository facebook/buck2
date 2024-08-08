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
use std::time::Duration;

use fbinit::FacebookInit;

#[cfg(fbcode_build)]
mod fbcode {
    use std::sync::Arc;
    use std::time::Duration;
    use std::time::SystemTime;

    use buck2_core::buck2_env;
    use buck2_data::InstantEvent;
    use buck2_data::Location;
    use buck2_data::StructuredError;
    use buck2_util::truncate::truncate;
    use fbinit::FacebookInit;
    use prost::Message;

    use crate::metadata;
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
    }

    impl RemoteEventSink {
        /// Creates a new RemoteEventSink that forwards messages onto the Thrift-backed Scribe client.
        pub fn new(
            fb: FacebookInit,
            category: String,
            buffer_size: usize,
            retry_backoff: Duration,
            retry_attempts: usize,
            message_batch_size: Option<usize>,
        ) -> anyhow::Result<RemoteEventSink> {
            let client = scribe_client::ScribeClient::new(
                fb,
                buffer_size,
                retry_backoff,
                retry_attempts,
                message_batch_size,
            )?;
            Ok(RemoteEventSink { category, client })
        }

        // Send this event now, bypassing internal message queue.
        pub async fn send_now(&self, event: BuckEvent) {
            self.send_messages_now(vec![event]).await;
        }

        // Send multiple events now, bypassing internal message queue.
        pub async fn send_messages_now(&self, events: Vec<BuckEvent>) {
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
            self.client.send_messages_now(messages).await;
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
            let proto: Box<buck2_data::BuckEvent> = event.into();

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
                                    soft_error_category: Some("oversized_scribe".to_owned()),
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
    }

    impl EventSink for RemoteEventSink {
        fn send(&self, event: Event) {
            match event {
                Event::Buck(event) => {
                    if should_send_event(event.data()) {
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
            }
        }
    }

    fn should_send_event(d: &buck2_data::buck_event::Data) -> bool {
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
                        match ActionExecutionKind::from_i32(a.execution_kind) {
                            // Those kinds are not used in downstreams
                            Some(ActionExecutionKind::Simple) => false,
                            Some(ActionExecutionKind::Deferred) => false,
                            Some(ActionExecutionKind::NotSet) => false,
                            _ => true,
                        }
                    }
                    Some(Data::Analysis(..)) => true,
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

    pub fn scribe_category() -> anyhow::Result<String> {
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

    use crate::BuckEvent;
    use crate::Event;
    use crate::EventSink;
    use crate::EventSinkStats;
    use crate::EventSinkWithStats;

    pub enum RemoteEventSink {}

    impl RemoteEventSink {
        pub async fn send_now(&self, _event: BuckEvent) {}
        pub async fn send_messages_now(&self, _events: Vec<BuckEvent>) {}
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
}

pub use fbcode::*;

fn new_remote_event_sink_if_fbcode(
    fb: FacebookInit,
    buffer_size: usize,
    retry_backoff: Duration,
    retry_attempts: usize,
    message_batch_size: Option<usize>,
) -> anyhow::Result<Option<RemoteEventSink>> {
    #[cfg(fbcode_build)]
    {
        Ok(Some(RemoteEventSink::new(
            fb,
            scribe_category()?,
            buffer_size,
            retry_backoff,
            retry_attempts,
            message_batch_size,
        )?))
    }
    #[cfg(not(fbcode_build))]
    {
        let _ = (
            fb,
            buffer_size,
            retry_backoff,
            retry_attempts,
            message_batch_size,
        );
        Ok(None)
    }
}

pub fn new_remote_event_sink_if_enabled(
    fb: FacebookInit,
    buffer_size: usize,
    retry_backoff: Duration,
    retry_attempts: usize,
    message_batch_size: Option<usize>,
) -> anyhow::Result<Option<RemoteEventSink>> {
    if is_enabled() {
        new_remote_event_sink_if_fbcode(
            fb,
            buffer_size,
            retry_backoff,
            retry_attempts,
            message_batch_size,
        )
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
