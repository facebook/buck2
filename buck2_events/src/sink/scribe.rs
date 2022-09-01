//! A Sink for forwarding events directly to Scribe.
use std::sync::atomic::AtomicBool;
use std::sync::atomic::Ordering;

#[cfg(fbcode_build)]
mod fbcode {

    use std::time::SystemTime;

    use buck2_core::truncate::truncate;
    use buck2_data::InstantEvent;
    use buck2_data::Location;
    use buck2_data::Panic;
    use fbinit::FacebookInit;
    use prost::Message;

    use crate::metadata;
    use crate::BuckEvent;
    use crate::ControlEvent;
    use crate::EventSink;
    use crate::TraceId;

    // 1 MiB limit
    static SCRIBE_MESSAGE_SIZE_LIMIT: usize = 1024 * 1024;
    // 50k characters
    static TRUNCATED_SCRIBE_MESSAGE_SIZE: usize = 50000;

    /// ThriftScribeSink is a ScribeSink backed by the Thrift-based client in the `buck2_scribe_client` crate.
    pub struct ThriftScribeSink {
        category: String,
        client: scribe_client::ScribeClient,
    }

    impl ThriftScribeSink {
        /// Creates a new ThriftScribeSink that forwards messages onto the Thrift-backed Scribe client.
        pub fn new(
            fb: FacebookInit,
            category: String,
            buffer_size: usize,
        ) -> anyhow::Result<ThriftScribeSink> {
            let client = scribe_client::ScribeClient::new(fb, buffer_size)?;
            Ok(ThriftScribeSink { category, client })
        }

        pub async fn flush_blocking(&self) {
            self.client.flush_blocking().await;
        }

        fn send_internal(&self, mut event: BuckEvent, is_truncation: bool) {
            let message_key = event.trace_id.hash();

            Self::smart_truncate_event(&mut event.data);
            let proto: buck2_data::BuckEvent = event.into();

            let mut buf = Vec::with_capacity(proto.encoded_len());
            proto
                .encode(&mut buf)
                .expect("failed to encode protobuf message");

            // Scribe requires that payloads sent through it be valid strings. Since protobuf serializes to bytes, we
            // re-encode them as base64 here. This is not super ideal, but it does work.
            let b64 = base64::encode(&buf);

            if b64.len() > SCRIBE_MESSAGE_SIZE_LIMIT {
                // if this BuckEvent is already a truncated one but the b64 byte size exceeds the limit,
                // do not send Scribe another truncated version
                if is_truncation {
                    return;
                }
                let json = serde_json::to_string(&proto).unwrap();

                return self.send_internal(
                    BuckEvent {
                        data: buck2_data::buck_event::Data::Instant(InstantEvent {
                            data: Some(
                                Panic {
                                    location: Some(Location {
                                        file: file!().to_string(),
                                        line: line!(),
                                        column: column!(),
                                    }),
                                    payload: format!("Soft Error: oversized_scribe: Message is oversized. Event data: {}. Original message size: {}", truncate(&json, TRUNCATED_SCRIBE_MESSAGE_SIZE),
                                    b64.len()),
                                    metadata: metadata::collect(),
                                    backtrace: Vec::new(),
                                }
                                .into(),
                            ),
                        }),
                        parent_id: None,
                        span_id: None,
                        timestamp: SystemTime::now(),
                        trace_id: TraceId::new(),
                    },
                    true,
                );
            }

            self.client.offer(scribe_client::Message {
                category: self.category.clone(),
                message: b64.as_bytes().to_vec(),
                message_key: Some(message_key),
            });
        }

        fn smart_truncate_event(d: &mut buck2_data::buck_event::Data) {
            use buck2_data::buck_event::Data;

            match d {
                Data::SpanEnd(ref mut s) => {
                    use buck2_data::span_end_event::Data;

                    match &mut s.data {
                        Some(Data::ActionExecution(ref mut action_execution)) => {
                            // truncate(...) can panic if asked to truncate too short.
                            const MIN_CMD_TRUNCATION: usize = 20;
                            let per_command_size_budget = ((500 * 1024)
                                / action_execution.commands.len().max(1))
                            .max(MIN_CMD_TRUNCATION);

                            let truncate_cmd =
                                |cmd: &mut buck2_data::CommandExecution, truncate_all: bool| {
                                    if let Some(details) = &mut cmd.details {
                                        details.stderr = if truncate_all {
                                            "<<omitted>>".to_owned()
                                        } else {
                                            truncate(&details.stderr, per_command_size_budget)
                                        };
                                    }
                                };

                            if let Some((last_command, retries)) =
                                action_execution.commands.split_last_mut()
                            {
                                for retried in retries {
                                    truncate_cmd(retried, false);
                                }
                                // Current Scribe tailers don't read stderr of successful actions.
                                // Save some bytes.
                                truncate_cmd(last_command, !action_execution.failed);
                            }
                        }
                        _ => {}
                    };
                }
                Data::Instant(ref mut inst) => {
                    use buck2_data::instant_event::Data;
                    match &mut inst.data {
                        Some(Data::TestResult(ref mut test_result)) => {
                            const TRUNCATED_DETAILS_LENGTH: usize = 512 * 1024; // 512Kb
                            test_result.details =
                                truncate(&test_result.details, TRUNCATED_DETAILS_LENGTH);
                        }
                        _ => {}
                    }
                }
                _ => {}
            };
        }
    }

    impl EventSink for ThriftScribeSink {
        fn send(&self, event: BuckEvent) {
            if !should_send_event(&event.data) {
                return;
            }
            self.send_internal(event, false)
        }

        fn send_control(&self, _control_event: ControlEvent) {}
    }

    fn should_send_event(d: &buck2_data::buck_event::Data) -> bool {
        use buck2_data::buck_event::Data;

        match d {
            Data::SpanStart(s) => {
                use buck2_data::span_start_event::Data;

                match s.data {
                    Some(Data::Command(..)) => true, // used in CommandReporterProcessor
                    Some(Data::ActionExecution(..)) => true, // used in ActionCounterProcessor
                    Some(Data::Analysis(..)) => false,
                    Some(Data::AnalysisStage(..)) => false,
                    Some(Data::FinalMaterialization(..)) => false,
                    Some(Data::Load(..)) => false,
                    Some(Data::LoadPackage(..)) => false,
                    Some(Data::ExecutorStage(..)) => false,
                    Some(Data::TestDiscovery(..)) => false,
                    Some(Data::TestStart(..)) => false,
                    Some(Data::Watchman(..)) => false,
                    Some(Data::MatchDepFiles(..)) => false,
                    Some(Data::SharedTask(..)) => false,
                    Some(Data::Fake(..)) => false,
                    None => false,
                }
            }
            Data::SpanEnd(s) => {
                use buck2_data::span_end_event::Data;

                match s.data {
                    Some(Data::Command(..)) => true,
                    Some(Data::ActionExecution(..)) => true,
                    Some(Data::Analysis(..)) => true,
                    Some(Data::AnalysisStage(..)) => false,
                    Some(Data::FinalMaterialization(..)) => true,
                    Some(Data::Load(..)) => true,
                    Some(Data::LoadPackage(..)) => true,
                    Some(Data::ExecutorStage(..)) => false,
                    Some(Data::TestDiscovery(..)) => true,
                    Some(Data::TestEnd(..)) => true,
                    Some(Data::SpanCancelled(..)) => false,
                    Some(Data::Watchman(..)) => true,
                    Some(Data::MatchDepFiles(..)) => false,
                    Some(Data::SharedTask(..)) => false,
                    Some(Data::Fake(..)) => true,
                    None => false,
                }
            }
            Data::Instant(i) => {
                use buck2_data::instant_event::Data;

                match i.data {
                    Some(Data::RawOutput(..)) => false,
                    Some(Data::Snapshot(..)) => false,
                    Some(Data::DiceStateSnapshot(..)) => false,
                    Some(Data::LspResult(..)) => false,
                    None => false,
                    _ => true,
                }
            }
            Data::Record(_) => true,
        }
    }
}

#[cfg(fbcode_build)]
pub use fbcode::*;

/// Whether or not Scribe logging is enabled for this process. It must be explicitly disabled via `disable()`.
static SCRIBE_ENABLED: AtomicBool = AtomicBool::new(true);

/// Returns whether this process should actually write to Scribe, even if it is fully supported by the platform and
/// binary.
pub fn is_enabled() -> bool {
    SCRIBE_ENABLED.load(Ordering::Relaxed)
}

/// Disables Scribe logging for this process. Scribe logging must be disabled explicitly on startup, otherwise it is
/// on by default.
pub fn disable() {
    SCRIBE_ENABLED.store(false, Ordering::Relaxed);
}
